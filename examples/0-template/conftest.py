# LC3 pytest-html formatting!
# 
# This edits the pytest HTML output to: 
# - include a Description column, and 
# - remove the Links column.

import pytest

def pytest_html_results_table_header(cells):
    cells.insert(2, "<th>Description</th>")
    # delete Links column
    del cells[4]

def pytest_html_results_table_row(report, cells):
    cells.insert(2, f"<td>{getattr(report, 'description', '')}</td>")
    # delete Links column
    del cells[4]

@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_makereport(item, call):
    outcome = yield
    report = outcome.get_result()
    doc = item.function.__doc__
    if doc is not None:
        report.description = doc
