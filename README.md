# lc3-ensemble Python Backend & Autograder

## Development Setup

1. Create a Python virtual environment with `python -m venv .env`
2. Activate the environment by running the activate script:
    - Windows: `.env\Scripts\activate`
    - Other: `source .env/bin/activate`
3. Install maturin (`pip install maturin`)
4. Run `maturin develop`
5. Import the `ensemble_test.core` or `ensemble_test.autograder` modules while inside the virtual environment

## Installation

If installing directly from this repository,

- `pip install .`: Install the barebones autograder
- `pip install ".[std]"`: Install the autograder and additional packages to help create autograders for CS 2110.

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

#### Display as JSON

```zsh
pytest --json-report
```

#### Display as JUnitXML

```zsh
pytest --junitxml=report.xml
```

### Without standard dependencies

If standard dependencies are not included, autograders can still be run with `unittest`.

```zsh
python3 -m unittest <DIR>
```
