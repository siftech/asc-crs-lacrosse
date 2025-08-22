FROM ubuntu:bionic

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    python3-dev python3-pip python3-wheel virtualenv python3-venv \
    build-essential libxml2-dev libxslt1-dev git libffi-dev cmake \
    libreadline-dev libtool debootstrap debian-archive-keyring libglib2.0-dev \
    libpixman-1-dev qtdeclarative5-dev binutils-multiarch nasm \
    libssl-dev \
    automake gdb gdb-multiarch binfmtc \
    gosu libz-dev libpcap-dev libnet1-dev libnids-dev \
    python3-pygraphviz python3-pip python3-dev \
    python3-setuptools build-essential libxml2-dev libxslt1-dev git libffi-dev \
    cmake libreadline-dev libtool debootstrap debian-archive-keyring \
    libglib2.0-dev libpixman-1-dev qtdeclarative5-dev binutils-multiarch nasm \
    libssl-dev libssl1.0.0 libssl1.1 openssl curl \
    less psmisc \ 
    sudo parallel && \
    apt-get remove -y emacs humanity-icon-theme && \
    apt-get -y autoremove && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /var/cache/apk/* && \
    rm -rf /root/.cache


RUN useradd -s /bin/bash -m angr

RUN curl -L https://github.com/tianon/gosu/releases/download/1.10/gosu-amd64 \
	-o /usr/local/bin/gosu
RUN chmod +x /usr/local/bin/gosu
RUN groupadd sift && useradd -mg sift realuser
RUN chmod -R 777 /home/realuser && echo root:sift | chpasswd
COPY realuser.sh /
COPY bashrc /home/realuser/.bashrc
COPY bashrc /root/.bashrc
ENTRYPOINT ["/bin/sh", "/realuser.sh"]

