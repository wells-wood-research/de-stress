FROM python:3.7.2
COPY . /app
WORKDIR /app
RUN curl -sSL https://raw.githubusercontent.com/sdispater/poetry/master/get-poetry.py | python
RUN /root/.poetry/bin/poetry install
CMD ["/root/.poetry/bin/poetry", "run", "pytest --doctest-modules"]
