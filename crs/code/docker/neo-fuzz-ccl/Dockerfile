# NOTE you cannot use inline comments-- do not put a comment at end of line, you'll get a very obscure error.
FROM ubuntu:22.04

ARG CRS_CONTEXT_PREFIX=crs/code/docker/neo-fuzz-ccl

# Need this for various things below to configure non-interactively.
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends software-properties-common && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
	virtualenvwrapper python3 python3-dev python3-pip python3-pygraphviz python3-setuptools \
	build-essential git vim flex libfl-dev acl apt-utils dmsetup fakeroot kpartx file gpg-agent \
	tzdata libc6-dev-i386 libgraphviz-dev tcpdump libz-dev libpcap-dev libnet1-dev libnids-dev libtool \
	automake gdb gdb-multiarch patchelf patchutils \
	gnuplot graphviz gosu sudo wget curl ssh rsync unzip nmap ncat snmp \
	tree jq jc ca-certificates make nano cargo iputils-ping && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY ${CRS_CONTEXT_PREFIX}/slime-v2.20.tar.gz /
RUN (cd /usr/local/; tar zxf /slime-v2.20.tar.gz; ln -s slime-2.20 slime) && rm -f /slime-v2.20.tar.gz

# Install Python 3.12.3, pip, and other dependencies
RUN add-apt-repository ppa:deadsnakes/ppa && \
    apt-get update && \
    apt-get install -y python3.12 python3.12-venv python3.12-dev python3-pip python3.12-distutils && \
    if [ ! -e /usr/bin/python ]; then ln -s /usr/bin/python3.12 /usr/bin/python; fi && \
    if [ ! -e /usr/bin/pip ]; then ln -s /usr/bin/pip3 /usr/bin/pip; fi && \
    rm -rf /var/lib/apt/lists/*

# Install setuptools and wheel using get-pip.py to ensure compatibility
RUN wget https://bootstrap.pypa.io/get-pip.py && \
    python3.12 get-pip.py && \
    rm get-pip.py

# Upgrade pip and install langchain
RUN python3.12 -m pip install --no-cache --upgrade pip
COPY "${CRS_CONTEXT_PREFIX}/../../langchain/requirements.txt" /tmp/langchain-requirements.txt
RUN python3.12 -m pip install -r /tmp/langchain-requirements.txt
RUN python3.12 -m pip install xmltodict

RUN wget https://github.com/mikefarah/yq/releases/download/v4.43.1/yq_linux_amd64.tar.gz -O - | \
  tar xz && mv yq_linux_amd64 /usr/bin/yq

# get github CLI
RUN curl -sS https://webi.sh/gh | sh

# Install mise (optional dependency installer for crs-sandbox/.tool-versions)
RUN curl https://mise.run | sh


# Now install a ccl.  (Based on https://github.com/daewok/lisp-devel-docker)
#ADD ccl-1.11.5-linuxx86.tar.gz /usr/local/src
ADD ${CRS_CONTEXT_PREFIX}/ccl-1.12.2-linuxx86.tar.gz /usr/local/src
ENV CCL_DEFAULT_DIRECTORY /usr/local/src/ccl
# Poss counter-intuitive, but I do prefer making ccl64 the default.
RUN ln -s /usr/local/src/ccl/scripts/ccl64 /usr/local/bin/ccl && \
    ln -s /usr/local/src/ccl/scripts/ccl /usr/local/bin/ccl32

# Ubuntu comes with /bin/sh linked to dash; that can cause issues b/c doesnt support expected redirection syntax.
# web suggested just linking bash to it...but.. that seems wrong given diff b/w bourne shell expected syntax, but whatever.
RUN ln -f /bin/bash /bin/sh

# Here's the realuser stuff we have from other projects, to make the user ID same as who runs it (on cgc-submit)

RUN groupadd sift && useradd -m -g sift realuser && adduser realuser sudo

RUN chmod -R 777 /home/realuser /usr/local/slime-2.20 && \
        echo root:sift | chpasswd

# this porks caching, do it late
RUN pip3 install --upgrade pip
RUN pip3 install --no-cache-dir numpy scipy requests dool \
	pyyaml xlsxwriter twisted cryptography pyasn1 dpkt python-levenshtein numpy scipy terminaltables scikit-learn pymodbus==2.2.0.rc4 && \
    rm -rf /root/.cache

RUN curl -sSL https://get.docker.com/ | sh

RUN chmod -R 777 /home/realuser && \
        echo root:sift | chpasswd
ENTRYPOINT ["/bin/sh", "/realuser.sh"]
CMD ["/bin/bash"]

COPY ${CRS_CONTEXT_PREFIX}/realuser.sh /
COPY ${CRS_CONTEXT_PREFIX}/bashrc /home/realuser/.bashrc
COPY ${CRS_CONTEXT_PREFIX}/bashrc /root/.bashrc

ADD ${CRS_CONTEXT_PREFIX}/jc-1.25.3-linux-x86_64.tar.gz /tmp
RUN cp /tmp/jc /usr/local/bin/jc

COPY crs/code/afl-linux/harness /lacrosse/code/afl-linux/harness
RUN cargo build --release --manifest-path /lacrosse/code/afl-linux/harness/Cargo.toml && \
    install -Dt /usr/local/bin /lacrosse/code/afl-linux/harness/target/release/afl-linux-harness

COPY crs/code/grab-a-func /lacrosse/code/grab-a-func
RUN cargo build --release --manifest-path /lacrosse/code/grab-a-func/Cargo.toml && \
    install -Dt /usr/local/bin /lacrosse/code/grab-a-func/target/release/grab-a-func

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    splitpatch && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# This is not ideal but we don't have a better option.
# This is how we make our code available when running from
# within the k8s cluster.
COPY crs /lacrosse
# RUN touch /lacrosse/.is_docker
