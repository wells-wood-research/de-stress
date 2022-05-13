FROM python:3.8

RUN apt update 

# && apt-get install -y dssp

WORKDIR /app

ENV PYTHONFAULTHANDLER=1 \
  PYTHONUNBUFFERED=1 \
  PYTHONHASHSEED=random \
  PIP_NO_CACHE_DIR=off \
  PIP_DISABLE_PIP_VERSION_CHECK=on \
  PIP_DEFAULT_TIMEOUT=100 \
  POETRY_VERSION=1.0.5

# System deps:
RUN pip install "poetry==$POETRY_VERSION"

# Installing aggrescan3D
RUN wget https://bootstrap.pypa.io/pip/2.7/get-pip.py \
    && python2 get-pip.py \ 
    && pip2 install Aggrescan3D

# Copy only requirements to cache them in docker layer
COPY ./poetry.lock ./pyproject.toml /app/

# Project initialization:
RUN poetry config virtualenvs.create false \
  && poetry install --no-interaction --no-ansi

# Creating folders, and files for a project:
COPY ./ /app/

RUN poetry install --no-interaction --no-ansi

# Run webserver
CMD gunicorn \
    -w ${GUNICORN_WORKERS} \
    -b 0.0.0.0:${APP_PORT} \
    -k flask_sockets.worker \
    --reload \
    destress_big_structure:app

