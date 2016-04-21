# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# person_in_charge: jacques.pellet@edf.fr

"""
This modules defines the base classes for the finite elements description:
    * physical quantities,
    * located components,
    * options,
    * mesh types,
    * elements.
"""

# Liste des changements de noms:
#
# famille           location            emplacement
# add_famille       addLocation
# add_elrefe        addElrefe
# modiCalcul        modifyCalcul
# TypeMaille        MeshType            maille support
# Attribut          Attribute
# Grandeur          PhysicalQuantity    grandeur physique
#                   ArrayOfQuantities   vecteur/matrice de grandeurs physiques
# Phenomene         Phenomenon
# EnsembleNoeud     SetOfNodes
# ModeLocal         LocatedComponents   ensemble de composantes localisées
#                   ArrayOfComponents   vecteur/matrice de LocatedComponents
# ParaOptIn         InputParameter
# ParaOptOut        OutputParameter
# ParamIn           para_in
# ParamOut          para_out

import sys
import os
import string
import re
import types
import copy
import os.path as osp
from glob import glob
from collections import OrderedDict

# TODO do not return list/dict internal storage, should be treat before
from cataelem.Tools.modifier import ChangeComponentsVisitor


class BaseCataEntity(object):

    """Abstract class for all elements of the catalog"""
    _currentId = -1
    idLength = 8

    def __init__(self):
        """Initialisation"""
        self.__class__._currentId += 1
        self._idx = self.__class__._currentId
        self._name = None
        self._comment = None

    def __getIndex(self):
        """Return the object index"""
        return self._idx
    idx = property(__getIndex)

    def __getName(self):
        """Return the object name"""
        return self._name

    def setName(self, name):
        """Define the object name"""
        assert not self._name or name == self._name, ("already named: '{0}' "
                                                      "(old: '{1}')".format(name, self._name))
        verif_identificateur(name, self.idLength)
        self._name = name

    name = property(__getName)

    # all entity can store a comment
    def __getComment(self):
        """Return the comment of the entity"""
        return self._comment

    def __setComment(self, comment):
        """Store the comment of the entity"""
        assert not comment or type(comment) is str, repr(comment)
        self._comment = comment

    comment = property(__getComment, __setComment)


# Catalog entities
class MeshType(BaseCataEntity):

    """Define a type of mesh"""
    _currentId = -1

    def __init__(self, nbno, dim, code):
        """Initialisation"""
        super(MeshType, self).__init__()
        a_creer_seulement_dans(self, ['mesh_types'])
        assert type(nbno) is int and 0 < nbno <= 54, nbno
        self._nbno = nbno
        assert type(dim) is int and dim in (0, 1, 2, 3), dim
        self._dim = dim
        assert type(code) is str and len(code) == 3, code
        self._code = code
        # TODO key unused, can be a list
        self._elrefe = OrderedDict()

    def __getNbNode(self):
        """Return the number of nodes"""
        return self._nbno
    nbNodes = property(__getNbNode)

    def __getDimension(self):
        """Return the dimension"""
        return self._dim
    dim = property(__getDimension)

    def __getCode(self):
        """Return the code string"""
        return self._code
    code = property(__getCode)

    def getElrefe(self):
        """Return all the Elrefe supported on this mesh"""
        return self._elrefe.values()

    def addElrefe(self, elrefe):
        """Store an Elrefe supported by the mesh"""
        key = elrefe.name or len(self._elrefe)
        self._elrefe[key] = elrefe
        # elrefe.setSupportMeshType(self)

    def setName(self, name):
        """Define the object name"""
        super(MeshType, self).setName(name)
        for key, elrefe in self._elrefe.items():
            if type(key) is int:
                del self._elrefe[key]
                elname = elrefe.name
                assert elname, ("Elrefe must be named before MeshType: "
                                "{0}".format(elrefe))
                self._elrefe[elname] = elrefe


class PhysicalQuantity(BaseCataEntity):

    """Definition of a physical quantity"""
    _currentId = -1

    def __init__(self, type, components, comment=None):
        """Initialisation"""
        super(PhysicalQuantity, self).__init__()
        a_creer_seulement_dans(self, ['physical_quantities'])
        assert type in ('R', 'I', 'C', 'K8', 'K16', 'K24'), type
        lcmp2 = expandComponents(components)
        assert noduplicates(lcmp2), "PhysicalQuantity: duplicated components: {0}".format(lcmp2)
        for cmp in lcmp2:
            verif_identificateur(cmp, 8)
        self._type = type
        self._components = lcmp2
        self.comment = comment

    def __getComponents(self):
        """Return the list of components"""
        return self._components
    components = property(__getComponents)

    def __getType(self):
        """Return the type of the physical quantity"""
        return self._type
    type = property(__getType)

    def hasComponent(self, cmp):
        """Tell if the quantity knows this component"""
        return cmp in self._components

    def accept(self, visitor):
        """Implements the visitor pattern"""
        return visitor.visitPhysicalQuantity(self)


class ArrayOfQuantities(BaseCataEntity):

    """Definition of a elementary quantity, based on a physical quantity"""
    _currentId = -1

    def __init__(self, elem, phys, comment=None):
        """Initialisation
        elem: 'MS', 'MR' or 'V'
        phys: physical quantity which this quantity is based on
        """
        super(ArrayOfQuantities, self).__init__()
        a_creer_seulement_dans(self, ['physical_quantities'])
        check_type([phys], PhysicalQuantity)
        self._elem = elem
        self._phys = phys
        self.comment = comment

    def __getPhys(self):
        """Return the physical quantity of the elementary quantity"""
        return self._phys
    physicalQuantity = property(__getPhys)

    def __getDim(self):
        """Return the dimension of the elementary quantity"""
        return self._elem
    dim = property(__getDim)

    def accept(self, visitor):
        """Implements the visitor pattern"""
        return visitor.visitArrayOfQuantities(self)


class Attribute(BaseCataEntity):

    """Definition of an attribute"""
    _currentId = -1
    idLength = 16

    def __init__(self, value, comment=None, auto=False):
        """Initialisation"""
        super(Attribute, self).__init__()
        a_creer_seulement_dans(self, ['attributes'])
        self.comment = comment
        self._value = check_type(force_tuple(value), str)
        # values can't be checked because automatically computed
        self._auto = auto

    def __getValue(self):
        """Return the list of authorized values"""
        return self._value
    value = property(__getValue)

    def isValid(self, value):
        """Tell if the value is a possible value"""
        return self._auto or value in self._value or value is None


class SetOfNodes(BaseCataEntity):

    """Definition of a set of nodes"""
    _currentId = -1

    def __init__(self, nom, nodes):
        """Initialisation"""
        super(SetOfNodes, self).__init__()
        self._name = nom
        self._nodes = check_type(force_tuple(nodes), int)
        for node in nodes:
            assert node > 0 and node <= 54, node
        assert noduplicates(nodes), "SetOfNodes: duplicated node id"

    def __getNodes(self):
        """Return the list of nodes"""
        return self._nodes
    nodes = property(__getNodes)


class LocatedComponents(BaseCataEntity):

    """Definition of a local mode"""
    _currentId = -1

    def __init__(self, phys, type, components, diff=False, location=None):
        """Initialisation"""
        super(LocatedComponents, self).__init__()
        a_creer_seulement_dans(self, ['located_components', 'Elements'])
        check_type([phys], PhysicalQuantity)
        assert type in ('ELEM', 'ELNO', 'ELGA')
        if type is not 'ELNO':
            assert diff is False
        if type is 'ELGA':
            assert location is not None, location
        else:
            assert location is None, location
        if not diff:
            lcmp2 = expandComponents(components)
            assert noduplicates(lcmp2), "LocatedComponents: duplicated components: {0}".format(lcmp2)
            for cmp in lcmp2:
                verif_identificateur(cmp, 8)
                assert phys.hasComponent(cmp), (phys.name, cmp)
            self._components = lcmp2
        else:
            self._components = []
            for setNodes, cmps in components:
                check_type([setNodes], str)
                lcmp2 = list(expandComponents(cmps))
                assert noduplicates(lcmp2), "LocatedComponents: duplicated components: {0}".format(lcmp2)
                for cmp in lcmp2:
                    verif_identificateur(cmp, 8)
                    assert phys.hasComponent(cmp), (phys.name, cmp)
                lcmp2.insert(0, setNodes)
                self._components.append(tuple(lcmp2))
            # checkings
            setNodesNames = self.getSetOfNodesNames()
            assert noduplicates(setNodesNames), \
                "LocatedComponents: duplicated SetOfNodes names"
        self._phys = phys
        self._type = type
        self._diff = diff
        self._location = location

    def __getPhys(self):
        """Return the physical quantity of the local mode"""
        return self._phys
    physicalQuantity = property(__getPhys)

    def __getComponents(self):
        """Return the list of components"""
        return self._components
    components = property(__getComponents)

    def __getType(self):
        """Return the type of field"""
        return self._type
    type = property(__getType)

    def __getDiff(self):
        """Tell if all nodes have the same components"""
        return self._diff
    diff = property(__getDiff)

    def __getLocation(self):
        """Return the location of integration for this mode"""
        return self._location
    location = property(__getLocation)

    def getSetOfNodesNames(self):
        """Return the names of the SetOfNodes"""
        return [cmp[0] for cmp in self._components]

    def copy(self, components=None):
        """Return a new LocatedComponents object, allow to change the list of
        components"""
        new = LocatedComponents(self._phys, self._type,
                                components or self._components[:],
                                self._diff, self._location)
        new.setName(self.name)
        return new

    def accept(self, visitor):
        """Implements the visitor pattern"""
        return visitor.visitLocatedComponents(self)


class ArrayOfComponents(BaseCataEntity):

    """Definition of a elementary mode, vector or matrix based on a local mode"""
    _currentId = -1

    def __init__(self, phys, locatedComponents):
        """Initialisation"""
        super(ArrayOfComponents, self).__init__()
        a_creer_seulement_dans(self, ['located_components', 'Elements'])
        check_type([phys], ArrayOfQuantities)
        self._phys = phys
        check_type([locatedComponents], LocatedComponents)
        self._locCmp = locatedComponents
        if phys.dim == 'V':
            self._type = 'VEC'
        else:
            self._type = 'MAT'

    def __getPhys(self):
        """Return the physical quantity of the elementary quantity"""
        return self._phys
    physicalQuantity = property(__getPhys)

    def __getLocatedComponents(self):
        """Return the underlying located components"""
        return self._locCmp
    locatedComponents = property(__getLocatedComponents)

    def __getType(self):
        """Return the type of the physical quantity"""
        return self._type
    type = property(__getType)

    def copy(self, locatedComponents=None):
        """Return a new ArrayOfComponents object, allow to change
        the located components"""
        new = ArrayOfComponents(self._phys,
                                locatedComponents or self._locCmp[:])
        new.setName(self.name)
        return new

    def accept(self, visitor):
        """Implements the visitor pattern"""
        return visitor.visitArrayOfComponents(self)


class InputParameter(BaseCataEntity):

    """Definition of an input parameter of an option"""
    _currentId = -1

    def __init__(self, phys, comment=None, container=None):
        """Initialisation"""
        super(InputParameter, self).__init__()
        a_creer_seulement_dans(self, ['parameters', 'Options'])
        check_type([phys], (PhysicalQuantity, ArrayOfQuantities))
        check_type([container], [types.NoneType, str])
        self._phys = phys
        self.comment = comment
        self._container = container

    def __getPhys(self):
        """Return the physical quantity of the elementary quantity"""
        return self._phys
    physicalQuantity = property(__getPhys)

    def __getLocalisation(self):
        """Tell """
        return self._container
    container = property(__getLocalisation)

    def accept(self, visitor):
        """Implements the visitor pattern"""
        return visitor.visitInputParameter(self)


class OutputParameter(BaseCataEntity):

    """Definition of an output parameter of an option"""
    _currentId = -1

    def __init__(self, phys, type, comment=None):
        """Initialisation"""
        super(OutputParameter, self).__init__()
        a_creer_seulement_dans(self, ['parameters', 'Options'])
        check_type([phys], (PhysicalQuantity, ArrayOfQuantities))
        check_type([type], str)
        assert type in ('RESL', 'ELEM', 'ELGA', 'ELNO')
        self._phys = phys
        self._type = type
        self.comment = comment

    def __getPhys(self):
        """Return the physical quantity of the elementary quantity"""
        return self._phys
    physicalQuantity = property(__getPhys)

    def __getType(self):
        """Return the type of the output"""
        return self._type
    type = property(__getType)

    def accept(self, visitor):
        """Implements the visitor pattern"""
        return visitor.visitOutputParameter(self)


class Option(BaseCataEntity):

    """Definition of an calculation option"""
    _currentId = -1
    idLength = 16

    def __init__(self, condition, para_in=None, para_out=None, comment=None):
        """Initialisation"""
        super(Option, self).__init__()
        a_creer_seulement_dans(self, ['Options'])
        if para_in:
            check_type(force_tuple(para_in), InputParameter)
        if para_out:
            check_type(force_tuple(para_out), OutputParameter)
        check_type(force_tuple(condition), CondCalcul)
        self._para_in = para_in
        self._para_out = para_out
        self._condition = condition
        self.comment = comment
        self._cacheParaDict = {}

    def _fillParaDict(self):
        """Create a dict to acces parameters by name"""
        for para in self._para_in + self._para_out:
            assert para.name, ("Option '{0}' {1}: parameters must be named "
                               "before accessing them".format(self.name, self))
            assert self._cacheParaDict.get(para.name) is None, (
                "Option '{0}' {1}: parameter '{2}' already used"
                .format(self.name, self, para.name))
            self._cacheParaDict[para.name] = para

    def __getParaIn(self):
        """Return the list of input parameters"""
        return self._para_in
    para_in = property(__getParaIn)

    def __getParaOut(self):
        """Return the list of output parameters"""
        return self._para_out
    para_out = property(__getParaOut)

    def __getCondition(self):
        """Return the list of conditions"""
        return self._condition
    condition = property(__getCondition)

    def __getattr__(self, attr):
        """Give quick access to input parameters by name"""
        if not self._cacheParaDict:
            self._fillParaDict()
        return self._cacheParaDict[attr]

    def __call__(self, te, para_in=None, para_out=None):
        """Define how to commpute this option for an element"""
        return Calcul(self, te, para_in, para_out)

    def accept(self, visitor):
        """Implements the visitor pattern"""
        return visitor.visitOption(self)


class CondCalcul(object):

    """Definition of the set of elements that must (or not) compute an option"""
    _currentId = -1

    def __init__(self, sign, lcond):
        """Initialisation"""
        a_creer_seulement_dans(self, ['Options'])
        assert sign in '+-', sign
        self._add = sign == '+'
        assign = []
        for attr, val in lcond:
            attr = checkAttr(attr, val)
            assign.append((attr, val))
        self._condition = assign

    def __getCondition(self):
        """Return the list of conditions"""
        return self._condition
    conditions = property(__getCondition)

    def addCondition(self):
        """Tell if the condition is an addition"""
        return self._add


class Elrefe(BaseCataEntity):

    """Define a reference element"""
    _currentId = -1

    def __init__(self):
        """Initialisation"""
        super(Elrefe, self).__init__()
        a_creer_seulement_dans(self, ['mesh_types', ])
        self._locations = OrderedDict()

    def addLocation(self, location, nbpg):
        """Define a location with its size of storage"""
        verif_identificateur(location, 8)
        check_type([nbpg], int)
        self._locations[location] = nbpg

    def __getLocations(self):
        """Return all locations"""
        return self._locations
    locations = property(__getLocations)


class ElrefeLoc(object):

    """Definition of a "local" reference element"""
    _currentId = -1

    def __init__(self, elrefe, gauss=None, mater=None):
        """Initialisation"""
        check_type([elrefe], Elrefe)
        self._elrefe = elrefe
        self._gauss = OrderedDict()
        self._mater = []
        if gauss:
            for gauss1 in gauss:
                loca, globa = gauss1.split('=')
                assert globa in elrefe.locations, ("In Elrefe '{0}': "
                                                   "unknown location '{1}'".format(elrefe.name, globa))
                self._gauss[loca] = globa
        if mater:
            assert gauss, "'gauss' is necessary before using 'mater'!"
            check_type(mater, str)
            for loca in mater:
                assert self._gauss.get(loca), ("unknown location: "
                                               "'{0}'".format(loca))
                self._mater.append(loca)

    def __getGauss(self):
        """Return the Gauss locations"""
        return self._gauss
    gauss = property(__getGauss)

    def __getMater(self):
        """Return the Material locations"""
        return self._mater
    mater = property(__getMater)

    def __getElrefe(self):
        """Return all the underlying Elrefe"""
        return self._elrefe
    elrefe = property(__getElrefe)


class Calcul(object):

    """Definition of an elementary calculation"""
    _currentId = -1

    def __init__(self, option, te, para_in=None, para_out=None):
        """Initialisation"""
        check_type([option], Option)
        check_type([te], int)
        assert (te > 0 and te <= 602) or te in (-1, -2), te
        if para_in is not None:
            for param, locCmp in para_in:
                assert param.physicalQuantity.name == locCmp.physicalQuantity.name
                check_type([param], InputParameter)
                check_type([locCmp], (LocatedComponents, ArrayOfComponents))
        if para_out is not None:
            for param, locCmp in para_out:
                assert param.physicalQuantity.name == locCmp.physicalQuantity.name
                check_type([param], OutputParameter)
                check_type([locCmp], (LocatedComponents, ArrayOfComponents))
        self._option = option
        self._te = te
        self._para_in = para_in or []
        self._para_out = para_out or []

    def __getOption(self):
        """Return the computed Option"""
        return self._option
    option = property(__getOption)

    def __getTe(self):
        """Return the TE number"""
        return self._te
    te = property(__getTe)

    def __getParaIn(self):
        """Return the list of the couples input parameters, components"""
        return list(self._para_in)
    para_in = property(__getParaIn)

    def __getParaOut(self):
        """Return the list of the couples output parameters, components"""
        return list(self._para_out)
    para_out = property(__getParaOut)

    def setParaIn(self, para):
        """Set the input parameters, couples (param, locCmp))"""
        self._para_in = para

    def setParaOut(self, para):
        """Set the output parameters, couples (param, locCmp))"""
        self._para_out = para

    def accept(self, visitor):
        """Implements the visitor pattern"""
        return visitor.visitCalcul(self)


class Element(BaseCataEntity):

    """Definition of a finite element by subclassing
    Subclasses must defined:
    - meshType
    - elrefe
    - nodes
    - attrs
    - calculs
    The first attributes are overridden by subclassing.
    The `calculs` attribute is extended at each subclassing. An existing Calcul
    is modified by subclassing.
    """

    _currentId = -1
    idLength = 16
    meshType = None
    elrefe = ()
    nodes = ()
    calculs = None
    attrs = ()

    def __init__(self):
        """Initialisation"""
        super(Element, self).__init__()
        self.elrefe = force_tuple(self.elrefe)
        check_type([self.meshType], MeshType)
        setNames = []
        if self.nodes:
            self.nodes = force_tuple(self.nodes)
            check_type(self.nodes, SetOfNodes)
            setNames = [setNodes.name for setNodes in self.nodes]
            assert noduplicates(setNames), "Element: duplicated SetOfNodes names: {0}".format(setNames)
            ids = []
            for setNodes in self.nodes:
                ids.extend(setNodes.nodes)
            assert noduplicates(ids), "Element: duplicated nodes ids between SetOfNodes"
            allNodes = set(range(1, self.meshType.nbNodes + 1))
            notUsed = allNodes.difference(ids)
            assert not notUsed, "Element: nodes not in a SetOfNodes: {0}".format(tuple(notUsed))
            tooMuch = set(ids).difference(allNodes)
            assert not tooMuch, "Element: nodes do not belong to the Element: {0}".format(tuple(tooMuch))
        check_type(self.elrefe, ElrefeLoc)
        # check attributes
        assign = []
        for attr, val in self.attrs:
            attr = checkAttr(attr, val)
            assign.append((attr, val))
        self._attrs = assign
        # fill _calculs
        self.calculs = force_tuple(self.calculs)
        check_type(self.calculs, Calcul)
        self._calculs = OrderedDict()
        # walk on inheritance from the abstract class to the element one
        for klass in reversed(self.__class__.mro()):
            if issubclass(klass, Element):
                for calc in klass.calculs or []:
                    if not self._calculs.get(calc.option.name):
                        self.addCalcul(calc)
                    else:
                        self.modifyCalcul(calc)
        self.postInit()
        self.checkSetOfNodes()

    def postInit(self):
        """Elements can define a post-initialization function to change its
        definition from its parent elements for example."""
        # use cataelem/Tools/modifier.py for this changes

    def getAttrs(self):
        """Return the attributes"""
        return self._attrs

    def getCalculs(self):
        """Return the list of calculations supported by the element"""
        return self._calculs.items()

    def addCalcul(self, calc):
        """Define a calculation on this element"""
        optname = calc.option.name
        assert optname, ("Element '{0}' {1}: options must be named "
                         "before accessing them".format(self.name, self))
        assert not self._calculs.get(optname), optname
        # assert calc.te > 0, calc.te
        self._calculs[optname] = calc

    def modifyCalcul(self, calc):
        """Change a calculation on this element"""
        optname = calc.option.name
        orig = self._calculs.get(optname)
        assert orig, "Element '{0}': unknown option '{1}'".format(self._name)
        if calc.te < 0:
            assert not calc.para_in, calc.para_in
            assert not calc.para_out, calc.para_out
            self._calculs[optname] = calc
        else:
            para_in = calc.para_in
            if not calc.para_in:
                para_in = orig.para_in
            para_out = calc.para_out
            if not calc.para_out:
                para_out = orig.para_out
            self._calculs[optname] = Calcul(calc.option, calc.te,
                                            para_in, para_out)

    def checkSetOfNodes(self):
        """Check the set of nodes usage in the LocatedComponents"""
        def _getNodes(name):
            """Return the nodes belonging to the set 'name'"""
            for setN in self.nodes:
                if setN.name == name:
                    return setN.nodes
            # assert False, "set {0!r} not defined in this element".format(name)
            return []
        allNodes = set(range(1, self.meshType.nbNodes + 1))
        for locCmp in self.usedLocatedComponents():
            if type(locCmp) is ArrayOfComponents or not locCmp.diff:
                continue
            defNodes = []
            for name in locCmp.getSetOfNodesNames():
                defNodes.extend(_getNodes(name))
            notUsed = tuple(allNodes.difference(defNodes))
            assert len(notUsed) == 0, "Element: components not defined on these nodes " \
                "(located components! {1}): {0}".format(notUsed, locCmp.name)

    def usedLocatedComponents(self):
        """Return the LocatedComponents used by this element"""
        loco = OrderedDict()
        for calc in self._calculs.values():
            for _, loc_i in calc.para_in + calc.para_out:
                loco[id(loc_i)] = loc_i
                if type(loc_i) is ArrayOfComponents:
                    loc_i = loc_i.locatedComponents
                    loco[id(loc_i)] = loc_i
        return loco.values()

    def changeComponents(self, locCmpName, components):
        """Modify the components of a LocatedComponents used in the element.
        A new LocatedComponents object is created and replaces the existing one."""
        self.accept( ChangeComponentsVisitor(locCmpName, components) )

    def accept(self, visitor):
        """Implements the visitor pattern"""
        return visitor.visitElement(self)


class Modelisation(object):

    """Definition of the properties of a modelisation"""
    _currentId = -1

    def __init__(self, dim, code, attrs=None, elements=None):
        """Initialisation"""
        assert len(dim) == 2, dim
        check_type(dim, int)
        assert len(code) == 3, code
        check_type([code], str)
        if attrs:
            assign = []
            for attr, val in attrs:
                attr = checkAttr(attr, val)
                assign.append((attr, val))
            attrs = assign
        if elements:
            for tyma, tyel in elements:
                check_type([tyma], MeshType)
                check_type([tyel], Element)
        self._dim = dim
        self._code = code
        self._attrs = attrs
        self._elements = elements

    def __getCode(self):
        """Return the code string"""
        return self._code
    code = property(__getCode)

    def __getAttrs(self):
        """Return the attributes"""
        return self._attrs
    attrs = property(__getAttrs)

    def __getDim(self):
        """Return the dimension"""
        return self._dim
    dim = property(__getDim)

    def __getElements(self):
        """Return the couples (MeshType, Element)"""
        return self._elements
    elements = property(__getElements)

    def accept(self, visitor):
        """Implements the visitor pattern"""
        return visitor.visitModelisation(self)


class Phenomenon(BaseCataEntity):

    """Definition of the modelisations of a phenomenon"""
    _currentId = -1
    idLength = 16

    def __init__(self, code):
        """Initialisation"""
        super(Phenomenon, self).__init__()
        assert len(code) == 2, code
        check_type([code], str)
        self._code = code
        self._modeli = OrderedDict()

    def add(self, name, modelisation):
        """Add a modelisation"""
        check_type([name], str)
        check_type([modelisation], Modelisation)
        assert self._modeli.get(
            name) is None, "'{0}' already exists".format(name)
        self._modeli[name] = modelisation

    def __getCode(self):
        """Return the code string"""
        return self._code
    code = property(__getCode)

    def __getModelisations(self):
        """Return the list of Modelisation objects"""
        return self._modeli
    modelisations = property(__getModelisations)


# some utilities
class AbstractEntityStore(object):

    """Helper class to give access to entities by name"""
    __slots__ = ('_entities', 'entityType', 'subTypes')
    entityType = None
    subTypes = None

    def __init__(self, package, ignore_names=[], only_mods=[]):
        """Initialisation: import all entities (of type `entityType`) available
        in the `package` under `cataelem`, objects of `subTypes` are named.
        `only_mods` is useful for the unittests."""
        assert self.entityType, "must be subclassed!"
        self.entityType = force_tuple(self.entityType)
        cataelemdir = osp.dirname(osp.dirname(__file__))
        l_mod = [osp.splitext(osp.basename(modname))[0]
                 for modname in glob(osp.join(cataelemdir, package, '*.py'))]
        l_mod = [modname for modname in l_mod \
                 if modname not in ('__init__', 'options', 'elements')]
        l_mod.sort()
        self._entities = OrderedDict()
        for modname in l_mod:
            if only_mods and modname not in only_mods:
                continue
            try:
                mod = __import__('cataelem.%s.%s' %
                                 (package, modname), globals(), locals(), [modname])
            except:
                error("ERROR during import of {0}".format(modname))
                raise
            # store the entities and name the parameters
            for name in dir(mod):
                if name in ignore_names:
                    continue
                obj = getattr(mod, name)
                self._register(name, obj)

    def _register(self, name, obj):
        """Register the object if its type is supported"""
        if type(obj) in self.entityType:
            self._entities[name] = obj
        elif (type(obj) is type and obj not in self.entityType
              and issubclass(obj, self.entityType)):
            try:
                self._entities[name] = obj()
            except AssertionError, exc:
                error("ERROR during initialisation of {0}".format(name))
                raise
        elif type(obj) in self.subTypes:
            obj.setName(name)
        elif type(obj) is dict:
            if name == '__builtins__':
                return
            for key, val in obj.items():
                self._register(key, val)

    def getDict(self):
        """Return the elements dict"""
        return self._entities

    def __getattr__(self, attr):
        """Return an element by its name"""
        return self._entities[attr]


def objects_from_context(dict_objects, filter_type, ignore_names=[]):
    """Build the list of all objects of the given type"""
    objects = dict([(name, obj) for name, obj in dict_objects.items()
                    if isinstance(obj, filter_type) and not name in ignore_names])
    return objects


def force_tuple(obj):
    """Return `obj` as a tuple."""
    if type(obj) not in (list, tuple):
        obj = [obj, ]
    return tuple(obj)


def check_type(sequence, types):
    """Raise an AssertionError exception if a value of the sequence is not
    of one of the given types"""
    types = force_tuple(types)
    for value in sequence:
        assert isinstance(value, types), type(value)
    return sequence

def checkAttr(attr, value):
    """Check the value of an attribute"""
    assert type(attr) is Attribute, type(attr)
    assert type(value) is str, type(value)
    assert attr.isValid(value), ("Attr {0}: unexpected value {1}"
                                 .format(attr.name, value))
    return attr

def error(message):
    """Print on stderr"""
    sys.stderr.write(message)
    sys.stderr.write(os.linesep)

#===============================================================================================
# utilitaires:
#-------------


def verif_identificateur(chaine, long):
#    -- verifie que:
#      * la chaine a une longueur <= long
#      * la chaine est en majuscules
    assert isinstance(chaine, types.StringType), chaine
    assert len(chaine) >= 1 and len(chaine) <= long, chaine
    for c in chaine:
        assert c in string.ascii_uppercase + string.digits + '_'


RECMP_DECL = re.compile('(.*?)\[([0-9]+)\]')


def expandComponents(lcmp):
    """Walk on the list of components and expand 'X[N]' by 'X1', 'X2'..., 'XN''"""
    lcmp_resu = []
    lcmp = force_tuple(lcmp)
    check_type(lcmp, str)
    for cmp in lcmp:
        mat = RECMP_DECL.search(cmp)
        if mat:
            for k in range(int(mat.group(2))):
                lcmp_resu.append(mat.group(1) + str(k + 1))
        else:
            lcmp_resu.append(cmp)
    return tuple(lcmp_resu)


def noduplicates(values):
    """Tell if there are not duplicated values"""
    return len(values) == len(set(values))


def a_creer_seulement_dans(obj, l_autorises):
#    -- Verifie que l'on cree l'objet obj dans 1 (ou plusieurs) "type" de fichier:
#       Les chaines autorisees dans l_autorises sont:
#         'physical_quantities', 'mesh_types', 'parameters', 'located_components', 'Options', 'Elements'

    # on provoque une erreur pour recuperer un objet de type Traceback:
    try:
        assert 0
    except:
        trace = sys.exc_info()

    # on remonte de 2 "crans" dans la trace:
    fram1 = trace[2].tb_frame.f_back.f_back
    filename = fram1.f_code.co_filename
    l1 = filename.split("/")

    OK = False
    for autor in l_autorises:
        if l1[-2] in ("cataelem", "Tools"):
            OK = True
            break
        if autor == 'physical_quantities':
            if l1[-1] == "physical_quantities.py":
                OK = True
                break
        elif autor == 'attributes':
            if l1[-1] == "attributes.py":
                OK = True
                break
        elif autor == 'mesh_types':
            if l1[-1] == "mesh_types.py":
                OK = True
                break
        elif autor == 'parameters':
            if l1[-1] == "parameters.py":
                OK = True
                break
        elif autor == 'located_components':
            if l1[-1] == "located_components.py":
                OK = True
                break
        elif autor == 'Options':
            if l1[-2] == "Options":
                OK = True
                break
        elif autor == 'Elements':
            if l1[-2] == "Elements":
                OK = True
                break
        else:
            assert 0, ("non autorise : ", autor)

    if not OK:
        print "l_autorises=", l_autorises
        print "l1=", l1
        assert 0, ("l'objet ", obj, "doit etre cree dans les fichiers de type:",
                   l_autorises)
