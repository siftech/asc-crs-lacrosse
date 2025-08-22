import argparse
import json
import logging
import os.path
import shutil
import subprocess
import sys
import tempfile
from typing import Any, Dict, List, NoReturn, Optional
from traceback import print_exception

from langchain_core.prompts import PromptTemplate
from langchain_core.pydantic_v1 import BaseModel

import lacrosse_llm.standard_args
import lacrosse_llm.patch_by_diffs
from lacrosse_llm import LLM
import lacrosse_llm.llm
from lacrosse_llm.llm import ProxyMisconfigurationError
from lacrosse_llm.patch_by_diffs import generate_patch_directly
from lacrosse_llm.standard_args import prepare
from lacrosse_llm.utils import double_braces, parse_resp_field, strip_doubles
from lacrosse_llm.patch_repair import generate_repaired_patch

repair_prompt_string: str = """
System: You are a security researcher, expert in detecting security vulnerabilities
and creating secure code. Apply symbolic execution.
Provide response only in following format.
Return NA if you do not find any vulnerabilities in the following code snippet.
Otherwise provide the complete program with all the vulnerabilites fixed in the
following format
[FIXEDCODE]
<repaired piece of code>
[/FIXEDCODE]
Do not include anything else in response.

User: Search for all highly probable vunerabilities that may exist in the following code snippet.
Rewrite the code to remove all the vulnerabilities to produce fixed code.
{code_snippet}
Response:
"""
repair_prompt: PromptTemplate = PromptTemplate(
    input_variables=["code_snippet", "basename"], template=repair_prompt_string
)

llm: Optional[LLM] = None

logger = logging.getLogger(__name__)
# Log to stdout so that the NeoFuzz infrastructure can find the
# output...
logger.addHandler(logging.StreamHandler(sys.stdout))
logger.setLevel(logging.INFO)


class RespDict(BaseModel):
    is_vulnerable: bool
    examples: Optional[str]
    fixed_code: Optional[str]


def no_vuln_resp_dict() -> RespDict:
    return RespDict(
        is_vulnerable=False,
        examples=None,
        fixed_code=None,
    )


def parse_resp_string(contents: str) -> RespDict:
    t = "FULL RESPONSE"
    end = "END FULL RESPONSE"
    logger.debug(f"--- {t} " + "-" * (70 - len(t)))
    logger.debug(f"{contents}\n--- {end} " + "-" * (70 - len(end)) + "\n")
    if contents.strip() == "NA":
        logger.info("No vulnerabilities or patch found")
        return no_vuln_resp_dict()
    return RespDict(
        is_vulnerable=True,
        # Dropping examples from the expected response.
        # examples=parse_resp_field(contents, "EXAMPLES", multi_line=True),
        examples=None,
        fixed_code=parse_resp_field(contents, "FIXEDCODE", multi_line=True),
    )


def check_file(filename: str) -> RespDict:
    code: str
    try:
        with open(filename, "r") as file:
            code = file.read()
    except IOError as e:
        logger.error(f"File {filename} not found or unreadable. Error: {e}")
        exit(1)
    return check_str(code, os.path.splitext(filename)[0])


def check_str(code: str, basename: str) -> RespDict:
    global llm
    code = double_braces(code)
    if llm is None:
        raise ValueError("llm should be initialized.")

    system_prompt = """
System: You are a security researcher, expert in detecting security vulnerabilities
and creating secure code. Apply symbolic execution.
Provide response only in following format.
Return NA if you do not find any vulnerabilities in the following code snippet.
Otherwise provide the complete program with all the vulnerabilites fixed in the
following format
[FIXEDCODE]
<repaired piece of code>
[/FIXEDCODE]
Do not include anything else in response.
"""
    user_prompt = f"""
User: Search for all highly probable vunerabilities that may exist in the following code snippet.
Rewrite the code to remove all the vulnerabilities to produce fixed code.
{code}
Response:
"""
    repair_resp: str
    err: Optional[Exception] = None
    repair_resp, err = llm.chat_completion(
        [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": strip_doubles(user_prompt)},
        ]
    )

    print(f"Repair response is:\n{repair_resp}", file=sys.stderr)
    if err is not None:
        print(f"LLM error is: {err}; trying to recover", file=sys.stderr)
    try:
        res = parse_resp_string(repair_resp)
    except Exception as e:
        if err is not None:
            print(f"LLM error is: {err}; failed to recover", file=sys.stderr)
            raise err
        else:
            raise e
    return res


def generate_patch(file, patched_file, patch_file) -> str:
    """Generate a patch file from `file` and `patched_file` and write it to `patch_file`"""
    filename: str = os.path.basename(file)
    filedir: str = os.path.dirname(file)

    # work in a temporary directory
    td_path: str = tempfile.mkdtemp()
    # copy the original file to a tmp directory
    orig: os.PathLike = shutil.copyfile(file, os.path.join(td_path, filename))  # type: ignore
    # copy the patched file over the original
    shutil.copy(patched_file, file)
    try:
        try:
            cmd = ["git", "diff", "--patch", filename]
            cp: subprocess.CompletedProcess = subprocess.run(
                cmd,
                text=True,
                stdout=subprocess.PIPE,
                cwd=filedir,
                stderr=subprocess.PIPE,
            )
        except subprocess.SubprocessError as e:
            logger.error(
                f"Error in constructing patch file between {file} and {patched_file}  with diff: {e}"
            )
            raise e
        if cp.returncode != 0:
            logger.error(
                f"Failed to construct patch file: diff between {file} and {patched_file} exited with {cp.returncode}.\nError output:\n{cp.stderr}"
            )
            raise subprocess.CalledProcessError(
                returncode=cp.returncode, cmd=cmd, stderr=cp.stderr
            )
        try:
            with open(patch_file, "w") as f:
                print(cp.stdout, file=f)
        except IOError as e:
            logger.error(f"Error in writing patch file, {patched_file}: {e}")
            raise e
    finally:
        # copy the original file back to its original location
        shutil.copyfile(orig, file)
        # remove the tmp directory
        shutil.rmtree(td_path)
    return str(patch_file)


class NonLocalExit(Exception):
    pass


def generate_patch_by_rewrite(args, opts) -> Optional[str]:
    try:

        def patch_error(e: Exception) -> NoReturn:
            if args.neo_fuzz:
                print(f"patch.py error: {e}")
                raise NonLocalExit()
            else:
                exit(1)

        try:
            val: RespDict = check_file(str(opts["file"]))
        except Exception as e:
            logger.error(f"Error patching file: {e}")
            patch_error(e)

        if not val.is_vulnerable:
            print(f"no vulnerabilities detected in {opts['file']}")
            logger.info("no vulnerabilities detected in {patch_args.file}")
            raise NonLocalExit()
        logger.debug(f"Writing patched file to {opts['patched_file']}")
        try:
            with open(opts["patched_file"], "w") as file:
                print(val.fixed_code, file=file)
        except IOError as e:
            print(f"Error in writing patched file: {e}")
            patch_error(e)
        patch_file: str = generate_patch(
            opts["file"], opts["patched_file"], opts["patch_file"]
        )
    except NonLocalExit:
        return None
    return patch_file


def setup_args(description: Optional[str] = None) -> argparse.ArgumentParser:
    parser = lacrosse_llm.standard_args.setup_args(description)
    parser.add_argument(
        "--no-direct-patch",
        action="store_false",
        dest="use_direct_patch",
        default=True,
    )
    return parser


_DESCRIPTION = """
Python script that invokes a LLM to find patches for bugs in the argument
file.
"""


def main(
    func_args: Optional[List[str]] = None,
) -> None:
    args: argparse.Namespace
    opts: Dict[str, Any]
    logger.debug("\n\n\n" + "--- START of RUN ---" * 3)
    args, opts = prepare(
        func_args=func_args, setup_func=setup_args, description=_DESCRIPTION
    )
    output_directory = opts['output_directory']
    try:
        os.mkdir(output_directory)
    except FileExistsError:
        pass

    # set up logging
    logger.addHandler(logging.FileHandler(os.path.join(output_directory, f"{__name__}.log")))
    lacrosse_llm.patch_by_diffs.logger.addHandler(logging.FileHandler(os.path.join(output_directory, "patch_by_diffs.log")))
    lacrosse_llm.llm.logger.addHandler(logging.FileHandler(os.path.join(output_directory, "llm.log")))
    lacrosse_llm.llm.logger.setLevel(logging.DEBUG)
    
    global llm
    llm = opts["llm"]
    if llm is None:
        raise ValueError("LLM not properly initialized, llm is None")
    patch_files: List[str] = []
    try:
        patch_file1 = generate_patch_by_rewrite(args, opts)
    except ProxyMisconfigurationError as e:
        logger.error("Got proxy misconfiguration error trying patch generation by rewrite. This error is fatal.")
        raise(e)
    except Exception as e:
        logger.error("Got error trying patch generation by rewrite:")
        logging.error(e, exc_info=True)
        patch_file1 = None
        
    if patch_file1:
        patch_files.append(os.path.abspath(patch_file1))
        try:
            rep_patch: Optional[str] = generate_repaired_patch(
                os.path.abspath(patch_file1), opts["file"]
            )
        except Exception as e:
            logger.error(f"Trying to generate repaired patch, got error {e}")
            rep_patch = None
        if rep_patch is not None:
            _fp, patch_file3 = tempfile.mkstemp(
                prefix=opts["file"], suffix=".patch", dir=opts["output_directory"]
            )
            with open(patch_file3, "w") as fp:
                print(rep_patch, file=fp)
            patch_files.append(os.path.abspath(patch_file3))

    if args.use_direct_patch:
        logger.info("Using direct patch generation from LLM")
        try:
            patch: Optional[str] = generate_patch_directly(llm, opts["file"])
        except Exception as e:
            logger.error("Got error trying direct patch generation:")
            logging.error(e, exc_info=True)
            patch = None
        if patch:
            logger.info("Successfully generated patch file directly")
            _fp, patch_file2 = tempfile.mkstemp(
                #prefix=args.filename, suffix=".patch", dir=opts["output_directory"]
                prefix=opts["file"], suffix=".patch", dir=opts["output_directory"]
            )
            logger.info(f"Saving patch file to {os.path.abspath(patch_file2)}")
            with open(patch_file2, "w") as fp:
                print(patch, file=fp)
            patch_files.append(os.path.abspath(patch_file2))
            try:
                rep_patch = generate_repaired_patch(
                    os.path.abspath(patch_file2), opts["file"]
                )
            except Exception as e:
                logger.error(f"Trying to generate repaired patch, got error {e}")
                rep_patch = None
            if rep_patch is not None:
                _fp, patch_file4 = tempfile.mkstemp(
                     prefix=opts["file"], suffix=".patch", dir=opts["output_directory"]
                )
                with open(patch_file4, "w") as fp:
                    print(rep_patch, file=fp)
                patch_files.append(os.path.abspath(patch_file4))

 
    else:
        logger.info("NOT using direct patch generation from LLM")
    json.dump({"outputFiles": patch_files}, fp=args.json_output_file)
    args.json_output_file.close()


if __name__ == "__main__":
    main()
    sys.exit(0)
