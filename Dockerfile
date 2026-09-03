FROM python:3.12-bookworm AS build

RUN apt-get update && apt-get install -y --no-install-recommends \
        cmake g++ git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
COPY CMakeLists.txt ./
COPY src ./src
COPY tests ./tests
COPY scripts ./scripts

RUN cmake -S . -B build -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
        -DCHESS_BUILD_TESTS=OFF -DCHESS_BUILD_PYTHON=ON \
        -DPython_EXECUTABLE="$(command -v python3)" \
    && cmake --build build -j \
    && ls -l build/chessengine*.so

FROM python:3.12-slim-bookworm

RUN apt-get update && apt-get install -y --no-install-recommends git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /src/build/chessengine*.so /app/
COPY bot/engine.py /app/engine.py
COPY bot/homemade.py /app/homemade.py
COPY bot/config.yml /app/config.yml
COPY bot/entrypoint.sh /app/entrypoint.sh
RUN chmod +x /app/entrypoint.sh \
    && git clone --depth 1 https://github.com/lichess-bot-devs/lichess-bot.git /app/lichess-bot \
    && pip install --no-cache-dir -r /app/lichess-bot/requirements.txt \
    && cp /app/homemade.py /app/lichess-bot/homemade.py \
    && cp /app/config.yml /app/lichess-bot/config.yml \
    && cp /app/engine.py /app/lichess-bot/engine.py

ENV PYTHONPATH=/app
ENV LICHESS_BOT_CONFIG=/app/lichess-bot/config.yml

CMD ["/app/entrypoint.sh"]
