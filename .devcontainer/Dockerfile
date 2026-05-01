FROM mcr.microsoft.com/devcontainers/java:1-17-bullseye

ARG USERNAME=vscode
ARG TEA_VERSION=0.14.0

RUN rm -f /etc/apt/sources.list.d/yarn.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends curl ca-certificates gnupg2 \
    && curl -fsSL https://keyserver.ubuntu.com/pks/lookup?op=get\&search=0x99E82A75642AC823 | gpg --dearmor -o /usr/share/keyrings/sbt.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/sbt.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" > /etc/apt/sources.list.d/sbt.list \
    && echo "deb [signed-by=/usr/share/keyrings/sbt.gpg] https://repo.scala-sbt.org/scalasbt/debian /" > /etc/apt/sources.list.d/sbt_old.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends sbt \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
      | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg \
    && chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" > /etc/apt/sources.list.d/github-cli.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends gh \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN arch="$(dpkg --print-architecture)" \
    && case "${arch}" in \
      amd64) tea_arch="amd64" ;; \
      arm64) tea_arch="arm64" ;; \
      *) echo "Unsupported architecture: ${arch}" && exit 1 ;; \
    esac \
    && curl -fsSL "https://dl.gitea.com/tea/${TEA_VERSION}/tea-${TEA_VERSION}-linux-${tea_arch}" -o /usr/local/bin/tea \
    && chmod +x /usr/local/bin/tea \
    && tea --version

USER ${USERNAME}
