# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
This module defines the main objects that contains all the informations
about the elements, options and other low level types.
"""

from collections import OrderedDict


class CataElem(object):
    """Store of all the objects defined in the catalog:
    physical quantities, options, finite elements..."""

    __slots__ = ('_store', '_cache')

    def __init__(self):
        """Initialisation"""
        self._cache = None
        self._store = {
            'Phenomenon': OrderedDict(),
            'Attribute': OrderedDict(),
            'PhysicalQuantity': OrderedDict(),
            'ArrayOfQuantities': OrderedDict(),
            'MeshType': OrderedDict(),
            'Elrefe': OrderedDict(),
            'Option': OrderedDict(),
            'Element': OrderedDict(),
            'InputParameter': EmptyDict(),
            'OutputParameter': EmptyDict(),
            'LocatedComponents': EmptyDict(),
        }
        self._initCache()

    def _initCache(self):
        """Reinitiliase cached values"""
        self._cache = {
            'nbElements': None,
            'nbLocations': None,
        }

    def _getByName(self, klass, name):
        """Return a object by type and name"""
        return self._store[klass][name]

    def _getAll(self, klass):
        """Return a list of all the objects of a given type"""
        return self._store[klass].values()

    def _sortByName(self, klass):
        """Sort the store items by names to ease the comparison"""
        self._store[klass] = OrderedDict(sorted(self._store[klass].items(),
                                         key=lambda i: i[1].name))

    def _sortObjects(self):
        """Sort objects by names to ease the comparison"""
        for klass in ('Attribute', 'PhysicalQuantity', 'ArrayOfQuantities',
                      'Elrefe', 'Option', 'Element', 'Phenomenon'):
            self._sortByName(klass)

    def register(self, obj, name):
        """Register the object with 'name' if the name is valid.
        Raise ValueError if it is not possible."""
        klass = obj.__class__.__name__
        assert self._store.get(klass) is not None, ("unsupported type: "
            "'{0}'".format(klass))
        if self._store[klass].get(name) is not None:
            raise ValueError("name '{0}' already exists for type "
                             "'{1}'".format(name, klass))
        self._initCache()
        self._store[klass][name] = obj
        # print("DEBUG: registering '{0}' {1}".format(name, obj))
        return name

    def registerAll(self, objects):
        """Register (and name) all the objects passed as dict [name, object]"""
        sortedObjects = sorted(objects.items(), key=lambda item: item[1].idx)
        for name, obj in sortedObjects:
            try:
                obj.setName(self.register(obj, name))
                # print("DEBUG: naming '{0}' {1}".format(obj.name, obj))
            except:
                klass = obj.__class__.__name__
                print("ERROR for object {0} {1}".format(name, obj))
                raise

    # XXX "getByName": to remove if not use!
    def phenomenon(self, name):
        """Return a Phenomenon object by name"""
        return self._getByName('Phenomenon', name)

    def attribute(self, name):
        """Return a Attribute object by name"""
        return self._getByName('Attribute', name)

    def physicalQuantity(self, name):
        """Return a PhysicalQuantity object by name"""
        return self._getByName('PhysicalQuantity', name)

    def meshType(self, name):
        """Return a MeshType object by name"""
        return self._getByName('MeshType', name)

    def elrefe(self, name):
        """Return a Elrefe object by name"""
        return self._getByName('Elrefe', name)

    def option(self, name):
        """Return a Option object by name"""
        return self._getByName('Option', name)

    def element(self, name):
        """Return a Element object by name"""
        return self._getByName('Element', name)

    def getPhenomenons(self):
        """Return the ordered list of the phenomenons"""
        return self._getAll('Phenomenon')

    def getAttributes(self):
        """Return the ordered list of the attributes"""
        return self._getAll('Attribute')

    def getPhysicalQuantities(self):
        """Return the ordered list of the physical quantities"""
        return self._getAll('PhysicalQuantity')

    def getArrayOfQuantities(self):
        """Return the ordered list of the elementary quantities"""
        return self._getAll('ArrayOfQuantities')

    def getMeshTypes(self):
        """Return the ordered list of the mesh types"""
        return self._getAll('MeshType')

    def getElrefe(self):
        """Return the ordered list of the elrefe"""
        return self._getAll('Elrefe')

    def getOptions(self):
        """Return the ordered list of the options"""
        return self._getAll('Option')

    def getElements(self):
        """Return the ordered list of the elements"""
        return self._getAll('Element')

    def getNbElrefe(self):
        """Return the number of Elrefe referenced in elements.
        They are not all different!"""
        if self._cache['nbElements'] is not None:
            return self._cache['nbElements']
        size = 0
        for elt in self.getElements():
            size += len(elt.elrefe)
        self._cache['nbElements'] = size
        return size

    def getNbLocations(self):
        """Return the number of locations referenced in elements"""
        if self._cache['nbLocations'] is not None:
            return self._cache['nbLocations']
        size = 0
        for elt in self.getElements():
            for elrefe in elt.elrefe:
                size += len(elrefe.gauss.keys())
                if len(elrefe.mater) > 0:
                    size += 1
        self._cache['nbLocations'] = size
        return size

    def build(self):
        """Build the object"""
        from cataelem.Commons.attributes import ATTRS
        from cataelem.Commons.mesh_types import ELREFS, MESHTYPES
        from cataelem.Commons.physical_quantities import PHYSQUANTS, ELEMQUANTS
        from cataelem.Commons.located_components import MODES
        from cataelem.Commons.parameters import INPUTS, OUTPUTS
        from cataelem.Options.options import OP
        # for registering in Jeveux
        self.registerAll(ATTRS)
        self.registerAll(ELREFS)
        self.registerAll(MESHTYPES)
        self.registerAll(PHYSQUANTS)
        self.registerAll(ELEMQUANTS)
        self.registerAll(MODES)
        self.registerAll(INPUTS)
        self.registerAll(OUTPUTS)
        self.registerAll(OP.getDict())
        # subobjects must have been named
        from cataelem.Elements.elements import EL
        self.registerAll(EL.getDict())
        from cataelem.Commons.phenomenons_modelisations import PHEN
        self.registerAll(PHEN)
        # summary
        for klass in ('Attribute', 'PhysicalQuantity', 'ArrayOfQuantities',
                      'MeshType', 'Elrefe', 'Option', 'Element', 'Phenomenon'):
            print("INFO: {1:6} {0}".format(klass, len(self._store[klass])))
        # to ease debugging objects can be sorted
        self._sortObjects()


class EmptyDict(dict):
    """A dict that store nothing"""

    def __setitem__(self, key, value):
        """Never store anything"""
        pass

# singleton object that registers all the objects of the catalog
cel = CataElem()
