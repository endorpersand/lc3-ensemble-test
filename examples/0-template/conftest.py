# pytest config for LC3 autograders.
# 
# You can ignore this file.
#
# This edits the pytest HTML output to
# include a Description column and
# removes the Links column.
#
# It also edits the JUnitXML output
# to include a description property.

import inspect
import pytest

def get_short_doc(f):
    """
    Gets the short doc of a function
    """
    doc = inspect.getdoc(f)
    if not doc: return ""
    return doc.splitlines()[0].strip()

def pytest_html_results_table_header(cells):
    cells.insert(2, "<th>Description</th>")
    # delete Links column
    del cells[4]

def pytest_html_results_table_row(report, cells):
    cells.insert(2, f"<td>{getattr(report, 'description', '')}</td>")
    # delete Links column
    del cells[4]

def pytest_collection_modifyitems(session, config, items):
    for item in items:
        doc = get_short_doc(item.function)
        item.user_properties.append(("description", doc))

@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_makereport(item, call):
    outcome = yield
    report = outcome.get_result()
    doc = get_short_doc(item.function)
    report.description = doc
