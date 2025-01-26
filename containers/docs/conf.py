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
sys.path.insert(0, os.path.abspath('.') + '/_extensions/haddock-autolink')

extensions = ['sphinx.ext.intersphinx',
              'sphinx.ext.ifconfig',
              'haddock-autolink',
              'sphinx_rtd_theme',
              'hs-theme']

templates_path = ['_templates']

source_suffix = {'.rst': 'restructuredtext'}

master_doc = 'index'

# General information about the project.
project = u'containers'
copyright = u'2018, Matt Renaud'
author = u'Matt Renaud'

# The short X.Y version.
version = u'0.5.10'
# The full version, including alpha/beta/rc tags.
release = u'0.5.10.2'

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
language = 'en'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This patterns also effect to html_static_path and html_extra_path
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'friendly'

# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = False


# -- Options for HTML output ----------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
# html_theme_options = {}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_logo = '_static/images/haskell-logo-green.svg'
html_static_path = ['_static']
html_context = {
    'source_url_prefix': "https://github.com/haskell/containers/tree/master/docs/",
    "display_github": True,
    "github_host": "github.com",
    "github_user": "haskell",
    "github_repo": 'containers',
    "github_version": "master/",
    "conf_py_path": "containers/docs/",
}
