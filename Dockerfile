FROM ubuntu:22.04
RUN mkdir -p /opt/openplan \
        && apt-get update \
        && apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN

WORKDIR       /opt/openplan
COPY openplan /opt/openplan
COPY static   /opt/openplan/static
COPY config   /opt/openplan/config
COPY demo     /opt/openplan/demo

ENV YESOD_PORT=8080
ENV YESOD_DEMO_LANG=${YESOD_DEMO_LANG}

EXPOSE 8080
CMD ["./openplan"]
