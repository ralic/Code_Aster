# coding=utf-8
# person_in_charge: mathieu.courtois at edf.fr
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

"""
Description des types de base aster
-----------------------------------

version 2 - réécrite pour essayer de simplifier
le problème des instances/types et instances/instances.

Le type de base `Type` permet de représenter une structure
de donnée. Une instance de `Type` comme attribut d'une classe
dérivée de `Type` représente une sous-structure nommée.

Une instance de `Type` 'libre' représente une instance de la
structure de donnée complète.

C'est ce comportement qui est capturé dans la classe BaseType

La classe `Type` hérite de BaseType et y associe la métaclasse MetaType.

"""

import cPickle

__docformat__ = "restructuredtext"




class MetaType(type):
    """Métaclasse d'un type représentant une structure de données.
    Les méthodes spéciales __new__ et __call__ sont réimplémentées
    """
    def __new__( mcs, name, bases, classdict ):
        """Création d'une nouvelle 'classe' dérivant de Type.

        Cette méthode permet de calculer certains attributs automatiquement:

         - L'attribut _subtypes qui contient la liste des sous-structures
           de type 'Type' attributs (directs ou hérités) de cette classe.

        Pour chaque attribut de classe héritant de Type, on recrée une nouvelle
        instance des attributs hérités pour pouvoir maintenir une structure de
        parentée entre l'attribut de classe et sa nouvelle classe.

        L'effet obtenu est que tous les attributs de classe ou des classes parentes
        de cette classe sont des attributs associés à la classe feuille. Ces attributs
        ont eux-meme un attribut parent qui pointe sur la classe qui les contient.
        """
        new_cls = type.__new__( mcs, name, bases, classdict )
        new_cls._subtypes = []
        for b in bases:
            if hasattr(b,'_subtypes'):
                new_cls._subtypes += b._subtypes
        # affecte la classe comme parent des attributs de classe
        # et donne l'occasion aux attributs de se renommer à partir
        # du nom utilisé.
        for k, v in classdict.items():
            if not isinstance( v, BaseType ):
                continue
            v.reparent( new_cls, k )
            new_cls._subtypes.append( k )
        return new_cls

    def dup_attr(cls, inst):
        """Duplique les attributs de la classe `cls` pour qu'ils deviennent
        des attributs de l'instance `inst`.
        """
        # reinstantiate and reparent subtypes
        for nam in cls._subtypes:
           obj = getattr( cls, nam )
           # permet de dupliquer completement l'instance
           cpy = cPickle.dumps(obj)
           newobj = cPickle.loads( cpy )
           newobj.reparent( inst, None )
           setattr( inst, nam, newobj )

    def __call__(cls, *args, **kwargs):
        """Instanciation d'un Type structuré.
        Lors de l'instanciation on effectue un travail similaire à la
        création de classe: Les attributs sont re-parentés à l'instance
        et réinstanciés pour obtenir une instanciation de toute la structure
        et de ses sous-structures.

        Les attributs de classe deviennent des attributs d'instance.
        """
        inst = cls.__new__(cls, *args, **kwargs)
        # reinstantiate and reparent subtypes
        cls.dup_attr( inst )
        type(inst).__init__(inst, *args, **kwargs)
        return inst

    def mymethod(cls):
       pass


class BaseType(object):
    # Le parent de la structure pour les sous-structures
    _parent = None
    _name = None

    def __init__(self, *args, **kwargs):
        self._initargs = args
        self._initkwargs = kwargs
        self._name = None
        self._parent = None

    def reparent( self, parent, new_name ):
        self._parent = parent
        self._name = new_name
        for nam in self._subtypes:
            obj = getattr( self, nam )
            obj.reparent( self, nam )

    def supprime(self, delete=False):
        """Permet de casser les boucles de références pour que les ASSD
        puissent être détruites.
        Si `delete` vaut True, on supprime l'objet lui-même et pas
        seulement les références remontantes."""
        self._parent = None
        self._name = None
        for nam in self._subtypes:
            obj = getattr(self, nam)
            obj.supprime(delete)
        #XXX MC : avec ce code, j'ai l'impression qu'on supprime aussi
        # des attributs de classe, ce qui pose problème pour une
        # instanciation future...
        # Supprimer les références remontantes devrait suffir.
        #if delete:
            #while len(self._subtypes):
                #nam = self._subtypes.pop(0)
                #try:
                    #delattr(self, nam)
                #except AttributeError:
                    #pass

    def base( self ):
        if self._parent is None:
            return self
        return self._parent.base()


class Type(BaseType):
    __metaclass__ = MetaType
