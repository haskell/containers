# -*- coding: utf-8 -*-
#

from docutils.parsers.rst import roles
from docutils import nodes
import itertools
import string
import os
import sphinx_rtd_theme
import sys


# -- General configuration ------------------------------------------------

# Add the _extenions dir to the search path.
sys.path.insert(0, os.path.abspath('.') + '/_extensions')

extensions = ['sphinx.ext.intersphinx',
              'sphinx.ext.ifconfig',
              'haddock-autolink']

templates_path = ['_templates']

source_suffix = '.rst'

master_doc = 'index'

# General information about the project.
project = u'containers'
copyright = u'2017, Matt Renaud'
author = u'Matt Renaud'

# The short X.Y version.
version = u'0.5.10'
# The full version, including alpha/beta/rc tags.
release = u'0.5.10.2'

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
language = None

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This patterns also effect to html_static_path and html_extra_path
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'

# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = False


# -- Options for HTML output ----------------------------------------------

# on_rtd is whether we are on readthedocs.org, this line of code grabbed from docs.readthedocs.org
on_rtd = os.environ.get('READTHEDOCS', None) == 'True'

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
if not on_rtd:  # only import and set the theme if we're building docs locally
    import sphinx_rtd_theme
    html_theme = 'sphinx_rtd_theme'
    html_theme_path = [sphinx_rtd_theme.get_html_theme_path()]

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
# html_theme_options = {}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_logo = '_static/images/haskell-logo-black.png'
html_static_path = ['_static']
html_context = {
    'source_url_prefix': "https://github.com/haskell/containers/tree/master/docs/",
    "display_github": True,
    "github_host": "github.com",
    "github_user": "haskell",
    "github_repo": 'containers',
    "github_version": "master/",
    "conf_py_path": "docs/",
    "source_suffix": '.rst',
}

# Custom sidebar templates, must be a dictionary that maps document names
# to template names.
#
# This is required for the alabaster theme
# refs: http://alabaster.readthedocs.io/en/latest/installation.html#sidebars
html_sidebars = {
    '**': [
        'relations.html',  # needs 'show_related': True theme option to display
        'searchbox.html',
    ]
}
