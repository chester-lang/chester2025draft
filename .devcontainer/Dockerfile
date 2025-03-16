FROM mcr.microsoft.com/devcontainers/base:ubuntu-22.04

# Install system dependencies
RUN apt-get update && apt-get install -y \
    wget \
    curl \
    git \
    python3 \
    python3-pip \
    clang \
    build-essential \
    zsh \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js 23
RUN curl -fsSL https://deb.nodesource.com/setup_23.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

# Install PNPM
RUN npm install -g pnpm

# Install GraalVM
RUN wget -q -O /tmp/graalvm.tar.gz https://download.oracle.com/graalvm/23/latest/graalvm-jdk-23_linux-x64_bin.tar.gz \
    && mkdir -p /opt/graalvm \
    && tar -xzf /tmp/graalvm.tar.gz -C /opt/graalvm --strip-components=1 \
    && rm /tmp/graalvm.tar.gz

# Set up environment variables
ENV JAVA_HOME=/opt/graalvm
ENV PATH=$JAVA_HOME/bin:$PATH

# Install SBT
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list \
    && echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list \
    && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add \
    && apt-get update \
    && apt-get install -y sbt \
    && rm -rf /var/lib/apt/lists/*

# Set up zshrc
RUN echo 'export JAVA_HOME=/opt/graalvm' >> /home/vscode/.zshrc \
    && echo 'export PATH=$JAVA_HOME/bin:$PATH' >> /home/vscode/.zshrc

# Switch to non-root user
USER vscode 