# coding=utf-8

# Copyright (C) 1991 - 2017  EDF R&D                www.code-aster.org
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

from functools import wraps

from .SyntaxUtils import mixedcopy, remove_none


def work_on_copy(func):
    """Decorator to automatically copy the keywords dict argument and remove
    None (that means undefined) values before calling a function."""
    @wraps(func)
    def wrapper(self, dict_orig):
        """wrapper"""
        dict_arg = mixedcopy(dict_orig)
        remove_none(dict_arg)
        return func(self, dict_arg)
    return wrapper


class Rule(object):
    """Abstract class for rules."""

    def __init__(self, *curTuple):
        """Initialization"""
        self.ruleArgs = curTuple

    def __repr__(self):
        """Simple representation"""
        return "%s( %r )" % (self.__class__, self.ruleArgs)

    def check(self, dictSyntax):
        """Check the rule"""
        if type(dictSyntax) != dict:
            raise TypeError("'dict' is expected")

    def _firstExists(self, dictSyntax):
        """Filter that tells if the first keyword exists"""
        return dictSyntax.has_key( self.ruleArgs[0] )

    def _not_none(self, dictSyntax):
        """Filter that returns existing values"""
        return [dictSyntax.has_key(i) for i in self.ruleArgs]


class AtLeastOne(Rule):
    """Check that at least one keyword from a list is defined."""

    @work_on_copy
    def check(self, dictSyntax):
        """Check the rule"""
        super(AtLeastOne, self).check(dictSyntax)
        if sum( self._not_none(dictSyntax) ) < 1:
            raise ValueError("At least one argument of {} must be defined".format(self.ruleArgs))


class ExactlyOne(Rule):
    """Check that exactly one keyword from a list is defined."""

    @work_on_copy
    def check(self, dictSyntax):
        """Check the rule"""
        super(ExactlyOne, self).check(dictSyntax)
        if sum( self._not_none(dictSyntax) ) != 1:
            raise ValueError("Exactly one argument of {} is required".format(self.ruleArgs))


class AtMostOne(Rule):
    """Check that at most one keyword from a list is defined."""

    @work_on_copy
    def check(self, dictSyntax):
        """Check the rule"""
        super(AtMostOne, self).check(dictSyntax)
        if sum( self._not_none(dictSyntax) ) > 1:
            raise ValueError("At most one argument of {} can be defined".format(self.ruleArgs))


class IfFirstAllPresent(Rule):
    """Check that if a keyword is defined all others from the list
    are defined."""

    @work_on_copy
    def check(self, dictSyntax):
        """Check the rule"""
        super(IfFirstAllPresent, self).check(dictSyntax)
        if self._firstExists(dictSyntax) and \
           sum( self._not_none(dictSyntax) ) != len(self.ruleArgs):
            raise ValueError("{} must be all defined".format(self.ruleArgs[1:]))


class OnlyFirstPresent(Rule):
    """Check that if a keyword is defined no of the others from the list
    is defined."""

    @work_on_copy
    def check(self, dictSyntax):
        """Check the rule"""
        super(OnlyFirstPresent, self).check(dictSyntax)
        if self._firstExists(dictSyntax) and \
           sum( self._not_none(dictSyntax)[1:] ) != 0:
            raise ValueError("{} must be all undefined".format(self.ruleArgs[1:]))


class AllTogether(Rule):
    """Check that if all the keywords from the list are defined or
    all undefined."""

    @work_on_copy
    def check(self, dictSyntax):
        """Check the rule"""
        super(AllTogether, self).check(dictSyntax)
        if sum( self._not_none(dictSyntax) ) not in ( 0, len(self.ruleArgs) ):
            raise ValueError("{} must be all defined or all undefined".format(self.ruleArgs))
