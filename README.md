# lc3-ensemble-test: Python backend & autograder for LC-3 assembly

This project is built upon [lc3-ensemble](https://github.com/endorpersand/lc3-ensemble). This module is an autograder library to test programs written in LC-3 assembly. To see the functions and how to implement an autograder, see [`doc/API.md`](https://github.com/endorpersand/lc3-ensemble-test/blob/main/doc/API.md).

## Installation

To install this package, you can use pip:

```zsh
pip install lc3-ensemble-test
```

To install additional packages to help create autograders for GT CS 2110,

```zsh
pip install "lc3-ensemble-test[std]"
```

## Running

### With standard dependencies

There are several ways to display test results using `pytest`'s built-in functionality.

#### Display in command-line

```zsh
pytest 
```

#### Display as HTML

```zsh
pytest --html=report.html --self-contained-html
```

*The `conftest.py` provided in `examples/` will automatically open the generated page in a web browser.*

#### Display as JUnitXML

```zsh
pytest --junitxml=report.xml
```

### Without standard dependencies

If standard dependencies are not included, autograders can still be run with `unittest`.

```zsh
python3 -m unittest <DIR>
```

## Development

### Development Setup

1. Create a Python virtual environment with `python -m venv .env`
2. Activate the environment by running the activate script:
    - Windows: `.env\Scripts\activate`
    - Other: `source .env/bin/activate`
3. Install maturin (`pip install maturin`)
4. Run `maturin develop`
5. Import the `ensemble_test.core` or `ensemble_test.autograder` modules while inside the virtual environment

### Development Installation

If installing directly from this repository,

- `pip install .`: Install the barebones autograder
- `pip install ".[std]"`: Install the autograder and additional packages to help create autograders for CS 2110.
