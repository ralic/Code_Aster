# coding: utf-8

# Copyright (C) 1991 - 2016  EDF R&D                www.code-aster.org
#
# This file is part of Code_Aster.
#
# Code_Aster is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Code_Aster is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Code_Aster.  If not, see <http://www.gnu.org/licenses/>.

"""
Code_Aster Syntax Utilities
---------------------------

List of utilities for syntax objects.
"""

import os
from functools import partial
from collections import OrderedDict

from .DataStructure import AsType


def mixedcopy(obj):
    """"Make a mixed copy (copy of all dicts, lists and tuples, no copy
    for all others)."""
    if isinstance(obj, list):
        new = [mixedcopy(i) for i in obj]
    elif isinstance(obj, tuple):
        new = tuple(mixedcopy(list(obj)))
    elif isinstance(obj, dict):
        new = obj.__class__([(i, mixedcopy(obj[i])) for i in obj])
    else:
        new = obj
    return new

def remove_none(obj):
    """"Remove None values from dict **in place**, do not change values of
    other types."""
    if isinstance(obj, (list, tuple)):
        for i in obj:
            remove_none(i)
    elif isinstance(obj, dict):
        for key, value in obj.items():
            if value is None:
                del obj[key]
            else:
                remove_none(obj[key])

def force_list(values):
    """Ensure `values` is a list or tuple."""
    if type(values) not in (list, tuple):
        values = [values]
    return values

# Keep consistency with SyntaxUtils.block_utils from AsterStudy, AsterXX
def block_utils(evaluation_context):
    """Define some helper functions to write block conditions.

    Arguments:
        evaluation_context (dict): The context containing the keywords.
    """

    def exists(name):
        """Tell if the keyword name exists in the context.
        The context is set to the evaluation context. In the catalog, just
        use: `exists("keyword")`"""
        return evaluation_context.get(name) is not None

    def is_in(name, values):
        """Checked if the/a value of 'keyword' is at least once in 'values'.
        Similar to the rule AtLeastOne, 'keyword' may contain several
        values."""
        name = force_list(name)
        values = force_list(values)
        # convert name to keyword
        keyword = []
        for name_i in name:
            if not exists(name_i):
                return False
            value_i = force_list(evaluation_context[name_i])
            keyword.extend(value_i)
        test = set(keyword)
        values = set(values)
        return not test.isdisjoint(values)

    def value(name, default=""):
        """Return the value of a keyword or the default value if it does
        not exist.
        The *default* default value is an empty string as it is the most
        used type of keywords."""
        return evaluation_context.get(name, default)

    def is_type(name):
        """Return the type of a keyword."""
        return AsType(value(name))
    equal_to = is_in

    return locals()

def sorted_dict(kwargs):
    """Sort a dict in the order of the items."""
    if not kwargs:
        # empty dict
        return OrderedDict()
    vk = sorted(zip(kwargs.values(), kwargs.keys()))
    newv, newk = zip(*vk)
    return OrderedDict(zip(newk, newv))

def debug_mode():
    """
    Check if application is running in DEBUG mode.

    Returns:
        int: 0 if DEBUG mode is switched OFF; 1 or more otherwise.
    """
    debug = getattr(debug_mode, "DEBUG", 0)
    debug = debug or int(os.getenv("DEBUG", 0))
    return debug

def debug_message(*args, **kwargs):
    """
    Print debug message.

    While this function is mainly dedicated for printing textual
    message, you may pass any printable object(s) as parameter.

    Example:
        >>> from common.utilities import debug_message, debug_mode
        >>> previous = debug_mode()
        >>> debug_mode.DEBUG = 1
        >>> debug_message("Start operation:", "Compute", "[args]", 100)
        AsterStudy: Start operation: Compute [args] 100
        >>> debug_message("Operation finished:", "Compute")
        AsterStudy: Operation finished: Compute
        >>> debug_mode.DEBUG = previous

    Note:
        Message is only printed if application is running in debug mode.
        See `debug_mode()`.

    Arguments:
        *args: Variable length argument list.
    """
    level = kwargs.get('level', 0)
    if debug_mode() > level:
        if args:
            print "AsterStudy:" + (" " + "." * level if level else ""),
            for arg in args:
                print arg,
            print

# pragma pylint: disable=invalid-name
debug_message2 = partial(debug_message, level=1)
