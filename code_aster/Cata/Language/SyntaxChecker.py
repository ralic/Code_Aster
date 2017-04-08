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
This module allows to check the syntax of a user command file that used the
syntax of legacy operators.
The check is performed at execution of an operator. So, the user file can mix
legacy operators and pure Python instructions.
"""

import numpy

import DataStructure as DS
from .SyntaxUtils import force_list, mixedcopy, remove_none, debug_message2


def fromTypeName(typename):
    """Convert a typename to a list of valid Python types (or an empty list)
    Example: 'I' returns [int, ...]"""
    if not hasattr(fromTypeName, "convTypes"):
        convTypes = {
            'TXM' : [str, unicode],
            'I' : [int, numpy.int, numpy.int32, numpy.int64],
        }
        convTypes['R'] = [float, numpy.float, numpy.float32, numpy.float64] \
                       + convTypes['I']
        convTypes['C'] = [complex, numpy.complex, numpy.complex64,
                          numpy.complex128] + convTypes['R']
        # exceptions
        convTypes[DS.MeshEntity] = convTypes['TXM']
        for deprec in ('Fichier', '', 'Sauvegarde'):
            convTypes[deprec] = convTypes['TXM']
        # When these objects will be removed...
        # convTypes[DS.listr8_sdaster] = convTypes['R']
        # convTypes[DS.listis_sdaster] = convTypes['I']
        fromTypeName.convTypes = convTypes
    return fromTypeName.convTypes.get(typename, [])


def _gettype(obj):
    """Return the type of an object"""
    # AsterStudy: for Command, use gettype()
    if hasattr(obj, "gettype"):
        return obj.gettype()
    return type(obj)


def isValidType(obj, expected):
    """Check that `obj` has one of the `expected` type"""
    # None means undefined: type is valid
    if obj is None:
        return True

    # AsterStudy: for PythonVariable
    if isinstance(obj, DS.PythonVariable):
        # can not be checked now, it will be when it will become a Variable
        return True

    typobj = _gettype(obj)
    debug_message2("checking type:", obj, type(obj), typobj, expected)
    # if a Variable is not yet evaluated, gettype returns None
    if typobj is type(None):
        return True
    if typobj in expected or obj in expected:
        return True
    try:
        if issubclass(typobj, tuple(expected)):
            return True
    except TypeError:
        pass

    # accept str for MeshEntity
    if issubclass(expected[0], (DS.MeshEntity, DS.GEOM)):
        if isinstance(obj, basestring):
            assert len(expected) == 1, 'several types for MeshEntity ?!'
            return True
    # accept all DataStructures for CO
    if DS.CO in expected and issubclass(typobj, DS.DataStructure):
        return True
    try:
        typname = obj.getType()
        expectname = [i.getType() for i in expected if issubclass(i, DS.DataStructure)]
        debug_message2(obj, typname, 'expecting:', expectname)
        return typname in expectname
    except AttributeError:
        pass
    return False


class SyntaxCheckerVisitor(object):

    """This class walks along the tree of a Command object to check its syntax"""

    def __init__(self):
        """Initialization"""
        self._stack = []
        self._parent_context = []

    @property
    def stack(self):
        return self._stack

    def set_parent_context(self, context):
        """Set the parent context.

        If the checker is directly called on a keyword (without checking the
        command itself) the values defined upper may be required to evaluate
        blocks conditions but the parent context has not been filled.

        This parent context could be an optional argument in visit* functions.
        """
        self._parent_context.append(context)

    def visitCommand(self, step, userDict=None):
        """Visit a Command object"""
        # do not check these fake commands
        if step.name in ("_CONVERT_VARIABLE", "_CONVERT_COMMENT",
                         "_RESULT_OF_MACRO"):
            return
        keywords = mixedcopy(userDict)
        step.addDefaultKeywords(keywords)
        debug_message2("checking syntax of", step.name, "with", keywords)
        self._parent_context.append(keywords)
        self._visitComposite(step, keywords)
        self._parent_context.pop()

    def visitBloc(self, step, userDict=None):
        """Visit a Bloc object"""
        pass

    def visitFactorKeyword(self, step, userDict=None):
        """Visit a FactorKeyword object"""
        # debug_message2("checking factor with", userDict)
        self._visitComposite(step, userDict)

    def visitSimpleKeyword(self, step, skwValue):
        """Visit a SimpleKeyword object
        Checks that :
            - the type is well known,
            - the values are in `into`,
            - the values are in [val_min, val_max],
            - the number of values is in [min, max]
        """
        if step.undefined(skwValue):
            # the keyword does not exist and it should have been checked by
            # its parent
            return
        # Liste les types possibles
        currentType = step.definition["typ"]
        currentType = force_list(currentType)
        validType = []
        specificTypes = (DS.DataStructure, DS.MeshEntity, DS.ValueCheckMixing)
        for i in currentType:
            pytypes = fromTypeName(i)
            if not pytypes and issubclass(i, specificTypes):
                pytypes = [i]
            validType.extend(pytypes)
        if not validType:
            raise TypeError("Unsupported type: {0!r}".format(currentType))

        # Vérification des valeurs max et min
        valMin = step.definition.get('val_min')
        valMax = step.definition.get('val_max')

        if type(skwValue) in (list, tuple, numpy.ndarray):
            # Vérification du nombre de valeurs
            nbMin = step.definition.get('min')
            nbMax = step.definition.get('max')
            if nbMax == "**":
                nbMax = None
            if nbMax != None and len(skwValue) > nbMax:
                debug_message2(step)
                raise ValueError('At most {0} values are expected'.format(nbMax))
            if nbMin != None and len(skwValue) < nbMin:
                debug_message2(step)
                raise ValueError('At least {0} values are expected'.format(nbMin))
        else:
            skwValue = [skwValue]

        # Vérification du type et des bornes des valeurs
        for i in skwValue:
            # AsterStudy: for PythonVariable
            if isinstance(i, DS.PythonVariable):
                # can not be checked now, it will be when it will become a Variable
                continue
            if hasattr(i, "evaluation"):
                i = i.evaluation
            # if a Variable is not yet evaluated
            if i is None:
                continue

            # Let expected types check for themselves
            for typeobj in validType:
                if issubclass(typeobj, DS.ValueCheckMixing):
                    if not typeobj.checkValue(i):
                        raise ValueError("Unexpected value: %s" % i)

                    if step.definition.has_key("into"):
                        into = step.definition["into"]
                        if not typeobj.checkInto(i, into):
                            raise ValueError("Unexpected value: %s" % i)

                    if valMax != None:
                        if not typeobj.checkMax(i, valMax):
                            raise ValueError("Unexpected value: %s" % i)

                    if valMin != None:
                        if not typeobj.checkMin(i, valMin):
                            raise ValueError("Unexpected value: %s" % i)

                    return
            # type
            if not isValidType(i, validType):
                step._context(i)
                raise TypeError('Unexpected type: {0}, expecting: {1}'
                    .format(type(i), validType))
            # into
            if step.definition.has_key("into"):
                if i not in step.definition["into"]:
                    raise ValueError("Unexpected value: {0!r}, must be in {1!r}"
                                     .format(i, step.definition["into"]))
            # val_min/val_max
            if valMax != None and i > valMax:
                raise ValueError('Value must be smaller than {0}, {1} is not'
                    .format(valMax, i))
            if valMin != None and i < valMin:
                raise ValueError('Value must be bigger than {0}, {1} is not'
                    .format(valMin, i))

        # call validators
        for valid in force_list(step.definition.get('validators', [])):
            valid.check(skwValue)

    def _visitComposite(self, step, userDict):
        """Visit a composite object (containing BLOC, FACT and SIMP objects)
        Check that :
            - the number of occurences is as expected
            - the rules are validated
            - the mandatory simple keywords are present
        One walks the Bloc objects to add the keywords according to the
        conditions.
        """
        # debug_message2("checking composite with", userDict)
        if step.undefined(userDict):
            # the keyword does not exist and it should have been checked by
            # its parent
            return
        if type(userDict) == dict:
            userDict = [userDict]
        elif type(userDict) in (list, tuple):
            pass
        else:
            raise TypeError("Type 'dict' or 'tuple' is expected")

        # check the number of occurrences
        if len(userDict) < step.definition.get('min', 0):
            raise ValueError("Too few factor keyword, at least {0} "
                "occurrence(s) expected".format(step.definition.get('min', 0)))
        if len(userDict) > step.definition.get('max', 1000000000):
            raise ValueError("Too much factor keyword, at most {0} "
                "occurrence(s) expected".format(step.definition.get('max', 0)))

        # loop on occurrences filled by the user
        for userOcc in userDict:
            # check rules
            if step.rules != None:
                for rule in step.rules:
                    self._stack.append(rule)
                    rule.check(userOcc)
                    self._stack.pop()
            # check that the required keywords are provided by the user
            ctxt = self._parent_context[-1] if self._parent_context else {}
            step.checkMandatory(userOcc, self._stack, ctxt)
            # loop on keywords provided by the user
            for key, value in userOcc.iteritems():
                # print key, value
                if key == "reuse":
                    if step.definition.get("reentrant") not in ("o", "f"):
                        self._stack.append(key)
                        raise KeyError("reuse is not allowed!")
                    continue
                kwd = step.getKeyword(key, userOcc, ctxt)
                if kwd is None:
                    debug_message2("keyword:", key, "user dict:", userOcc,
                                   "parent context:", ctxt)
                    self._stack.append(key)
                    raise KeyError("Unauthorized keyword: {!r}".format(key))
                else:
                    self._stack.append(key)
                    kwd.accept(self, value)
                    self._stack.pop()


# counter of 'printed' command
_cmd_counter = 0

def checkCommandSyntax(command, keywords, printSyntax=False):
    """Check the syntax of a command
    `keywords` contains the keywords filled by the user"""
    from pprint import pformat
    if not hasattr(checkCommandSyntax, "_cmd_counter"):
        checkCommandSyntax._cmd_counter = 0
    if type(keywords) != dict:
        raise TypeError("'dict' object is expected")
    if printSyntax:
        checkCommandSyntax._cmd_counter += 1
        printed_args = mixedcopy(keywords)
        remove_none(printed_args)
        print("\n{0:-^100}\n Command #{1:0>4}\n{0:-^100}"
            .format("", checkCommandSyntax._cmd_counter))
        print(" {0}({1})".format(command.name or "COMMAND",
                                 pformat(printed_args)))

    checker = SyntaxCheckerVisitor()
    command.accept(checker, keywords)
