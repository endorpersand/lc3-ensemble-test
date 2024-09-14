# Autograder Template

This is a simple template that can be used to kickstart creating an LC3 autograder.

To use this template,

1. copy all of the template files into a new folder
2. delete the solution file `student.asm`
3. rename `test_template.py` to `test_xx.py`
4. make autograder to heart's content

This folder contains:

- `autograder.py`: A script which automatically calls pytest and opens the results in the browser.
- `conftest.py`: Pytest configuration, which changes the HTML format.
- `test_template.py`: The test cases.
- `README.md`: Me!
- `student.asm`: A solution file to check the autograder works.
