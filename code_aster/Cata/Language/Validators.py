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

# person_in_charge: mathieu.courtois@edf.fr

from .SyntaxUtils import force_list


class Validator(object):
    """Abstract class for validators."""

    def __init__(self, *args, **kwargs):
        """Initialization"""
        self.args = args

    def __repr__(self):
        """Simple representation"""
        return "%s( %r )" % (self.__class__, self.args)

    def check(self, values):
        """Check values"""
        raise NotImplementedError("must be defined in a subclass")


class NoRepeat(Validator):
    """Check that all values are different."""

    def check(self, values):
        """Check values"""
        values = force_list(values)
        if len(set(values)) != len(values):
            raise ValueError("All the values must be different")


class LongStr(Validator):
    """Check that the length of string."""

    def __init__(self, *args, **kwargs):
        super(LongStr, self).__init__(*args)
        assert len(args) == 2, "Exactly two arguments required for LongStr."
        assert (type(args[0]), type(args[1])) == (int, int), (
            "LongStr arguments must be 'int'")

    def check(self, values):
        """Check values"""
        values = force_list(values)
        for string in values:
            if not (self.args[0] <= len(string) <= self.args[1]):
                raise ValueError("String length must be in [{0[0]}, {0[1]}]: "
                                 "length of '{1}' is {2}"
                                 .format(self.args, string, len(string)))


class AndVal(Validator):
    """Check that all validators are checked."""

    def __init__(self, *args, **kwargs):
        if type(args) in (list, tuple) and len(args) == 1:
            args = args[0]
        super(AndVal, self).__init__(*args)
        for i in self.args:
            assert isinstance(i, Validator), (
                "Arguments of AndVal must be Validator objects.")

    def check(self, values):
        """Check values"""
        values = force_list(values)
        for i in self.args:
            i.check(values)


class OrVal(Validator):
    """Check that at least one of the validators in argument is checked."""

    def __init__(self, *args, **kwargs):
        if type(args) in (list, tuple) and len(args) == 1:
            args = args[0]
        super(OrVal, self).__init__(*args)
        for i in self.args:
            assert isinstance(i, Validator), (
                "Arguments of OrVal must be Validator objects.")

    def check(self, values):
        """Check values"""
        values = force_list(values)
        for value in values:
            ok = False
            err = []
            for i in self.args:
                try:
                    i.check(value)
                except ValueError as exc:
                    err.append(str(exc))
                else:
                    ok = True
            if not ok:
                raise ValueError("No validator is valid: {0}".format(err))


class OrdList(Validator):
    """Check that the values are ordered.

    Usage:
        OrdList(): Check that order of values is increasing.
        OrdList(reverse=True): Check that order is decreasing.

        Old usage: OrdList('croissant')
    """

    def __init__(self, *args, **kwargs):
        super(OrdList, self).__init__()
        assert len(args) <= 1, "At most one argument is required for OrdList."
        reverse = kwargs.get('reverse', False)
        if len(args) == 1 and args[0] != 'croissant':
            reverse = True

        def predicate(a, b):
            if None not in (a, b):
                if not reverse:
                    return a <= b
                else:
                    return a >= b
            return True
        self._predicate = predicate

    def check(self, values):
        """Check values"""
        values = list(force_list(values))
        if not values:
            return
        previous = values.pop(0)
        while len(values) > 0:
            current = values.pop(0)
            if not self._predicate(previous, current):
                raise ValueError("The values are not ordered as "
                                 "expected: {0} followed by {1}"
                                 .format(previous, current))
            previous = current


class Together(Validator):
    """Check that if one of the values is used, all must be defined.

    Usage:
        Together([expected values])
    """

    def __init__(self, *args, **kwargs):
        super(Together, self).__init__(*args)
        assert len(args) == 1, "Exactly one argument is required for Together."

    def check(self, values):
        """Check values"""
        missing = set(self.args[0]).difference(force_list(values))
        if missing and len(missing) != len(self.args[0]):
            raise ValueError("Missing values: {0}"
                             .format([str(i) for i in missing]))


class Absent(Validator):
    """Check that if none of the values is defined.

    Usage:
        Absent([unexpected values])
    """

    def __init__(self, *args, **kwargs):
        super(Absent, self).__init__(*args)
        assert len(args) == 1, "Exactly one argument is required for Absent."

    def check(self, values):
        """Check values"""
        invalid = set(self.args[0]).intersection(force_list(values))
        if invalid:
            raise ValueError("Unauthorized values: {0}"
                             .format([str(i) for i in invalid]))


class Compulsory(Validator):
    """Check that all the values are defined.

    Usage:
        Compulsory([expected values])
    """

    def __init__(self, *args, **kwargs):
        super(Compulsory, self).__init__(*args)
        assert len(args) == 1, (
            "Exactly one argument is required for Compulsory.")

    def check(self, values):
        """Check values"""
        missing = set(self.args[0]).difference(force_list(values))
        if missing:
            raise ValueError("Missing values: {0}"
                             .format([str(i) for i in missing]))
