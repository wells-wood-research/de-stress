FROM python:3.8

ENV PYTHONFAULTHANDLER=1 \
  PYTHONUNBUFFERED=1 \
  PYTHONHASHSEED=random \
  PIP_NO_CACHE_DIR=off \
  PIP_DISABLE_PIP_VERSION_CHECK=on \
  PIP_DEFAULT_TIMEOUT=100 \
  POETRY_VERSION=1.0.5

# System deps:
RUN pip install "poetry==$POETRY_VERSION"

# Copy only requirements to cache them in docker layer
WORKDIR /app
COPY ./big-structure/poetry.lock ./big-structure/pyproject.toml /app/

# Project initialization:
RUN poetry config virtualenvs.create false \
  && poetry install --no-interaction --no-ansi

# Creating folders, and files for a project:
WORKDIR dependencies_for_de-stress
COPY ./dependencies_for_de-stress /dependencies_for_de-stress/
RUN ln -s /dependencies_for_de-stress/dssp/dssp-2.0.4 /usr/local/bin/mkdssp
WORKDIR /app
COPY ./big-structure /app/

RUN poetry install --no-interaction --no-ansi

# Run webserver
CMD gunicorn \
    -w ${GUNICORN_WORKERS} \
    -b 0.0.0.0:${APP_PORT} \
    -k flask_sockets.worker \
    --reload \
    destress_big_structure:app

