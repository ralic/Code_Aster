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
   Description des OJB jeveux
"""
from basetype import Type
from asnom import SDNom
from ascheckers import CheckLog
import traceback,sys

# pour utilisation dans eficas
try:
   import aster
   from Utilitai.Utmess import UTMESS
except:
   pass

# -----------------------------------------------------------------------------
class AsBase(Type):
    nomj = SDNom()
    optional = False

    def __init__(self, nomj=None, *args, **kwargs ):
        super(AsBase,self).__init__( nomj, *args, **kwargs )
        assert self.nomj is not self.__class__.nomj
        if isinstance( nomj, str ):
            self.nomj.nomj = nomj
        elif isinstance( nomj, SDNom ):
            self.nomj.update( nomj.__getstate__() )

    def set_name(self, nomj):
        """Positionne le nomj de self
        """
        assert isinstance( self.nomj.nomj, str ), "uniquement pour les concepts"
        self.nomj.nomj = nomj

    def check(self, checker=None):
        if checker is None:
            checker = CheckLog()

        # vérif déjà faite ? (en tenant compte du type)
        if checker.checkedAsBase(self):
            return checker
        checker.visitAsBase( self )

        # vérifie les enfants :
        optional = checker.optional
        checker.optional = checker.optional or self.optional
        for name in self._subtypes:
            v = getattr(self, name)
            if isinstance( v, (OJB,AsBase) ):
                v.check(checker)
        for name in dir(self):
            if name.startswith( 'check_' ):
                v = getattr(self, name)
                if callable(v):
                    try :
                        v( checker )
                    except :
                        mess=60*'-'+'\n'
                        mess=mess+'Erreur SDVERI (Attention : vérification incomplète)'+'\n'
                        mess=mess.join(traceback.format_tb(sys.exc_traceback))
                        checker.err(self,mess)

        checker.optional = optional
        return checker

    def members( self ):
        pass

    def dump(self, indent=""):
        import pydoc
        l = []
        checkers = []
        nomj = self.nomj()
        if self.optional:
            f = "(f)"
        else:
            f = "(o)"
        l.append( f+" "+nomj )
        #l.append( '-'*(len(nomj)+3) )
        for name in self._subtypes:
            obj = getattr(self, name)
            if isinstance(obj,(AsBase,OJB)):
                l.append( obj.dump(indent) )
        for name in dir(self):
            if name.startswith( 'check_' ):
                obj = getattr(self, name)
                if callable(obj) and name.startswith("check_"):
                    checkers.append( obj )

        indent = " "*len(nomj)
        for checker in checkers:
            doc = pydoc.text.document( checker )
            for line in doc.splitlines():
                l.append( indent + line )
        return "\n".join( l )

    def short_repr(self):
        return "<%s(%x,%r)>" % (self.__class__.__name__, id(self), self.nomj() )

    def long_repr(self):
        if not hasattr(self, "accessible") or not self.accessible():
           # hors Aster ou en par_lot='oui'
           return self.short_repr()
        else:
           from Cata.cata import IMPR_CO, _F
           IMPR_CO(CONCEPT=_F(NOM=self.nom), UNITE=6)
           return ''

    def __repr__(self):
        # par défaut, on fait court !
        return self.short_repr()


# -----------------------------------------------------------------------------
class JeveuxAttr(object):
    """Un attribut jeveux"""
    def __init__(self, name):
        self.name = name

    def __get__(self, obj, klass):
        raise NotImplementedError

    def check(self, attrname, obj, log ):
        checker = getattr(obj, "_"+attrname, None )
        if checker is None:
            return True
        val = self.__get__( obj, obj.__class__ )
        if callable( checker ):
            return checker( obj, attrname, val, log )
        else:
            test = val == checker
            if not test:
                log.err( obj, "Attribut incorrect %s %r!=%r" % (self.name, val, checker ) )
            return test

# -----------------------------------------------------------------------------
class JeveuxExists(JeveuxAttr):
    def __init__(self):
        pass

    def __get__(self, obj, klass):
        if obj is None:
            return self
        nomj = obj.nomj()
        if len(nomj)!=24:
            raise AssertionError(repr(nomj))
        return aster.jeveux_exists( nomj.ljust(24) )

# -----------------------------------------------------------------------------
class JeveuxIntAttr(JeveuxAttr):
    def __get__(self, obj, klass):
        if obj is None:
            return self
        nomj = obj.nomj()
        if aster.jeveux_exists( nomj ):
            return aster.jeveux_getattr( nomj, self.name )[0]
        else :
            return None

# -----------------------------------------------------------------------------
class JeveuxStrAttr(JeveuxAttr):
    def __get__(self, obj, klass):
        if obj is None:
            return self
        nomj = obj.nomj()
        if aster.jeveux_exists( nomj ):
            return aster.jeveux_getattr( nomj, self.name )[1].strip()
        else :
            return None

# -----------------------------------------------------------------------------
class OJB(AsBase):
    _clas = None
    _genr = None
    _type = None
    _ltyp = None
    _xous = None
    _docu = None
    _exists = True

    clas = JeveuxStrAttr("CLAS")
    genr = JeveuxStrAttr("GENR")
    type = JeveuxStrAttr("TYPE")
    ltyp = JeveuxIntAttr("LTYP")
    xous = JeveuxStrAttr("XOUS")
    docu = JeveuxStrAttr("DOCU")
    exists = JeveuxExists()
    #optional = False
    nomj = SDNom()

    def __init__(self, nomj=None, **attrs):
        super(OJB,self).__init__( nomj, **attrs )
        self.foreachattr( self.setattribute, attrs )
        self.optional = attrs.get('optional', False)

    def setattribute( self, name, prop, attrs ):
        _name = "_"+name
        if name in attrs:
            setattr( self, _name, attrs[name] )

    def get(self):
        nomj = self.nomj()
        if aster.jeveux_exists( nomj ):
            obj_simple = aster.jeveux_getattr( nomj, 'XOUS')[1].strip() == 'S'
            if obj_simple :
                return aster.getvectjev( nomj )
            else :
                return aster.getcolljev( nomj )
        else:
            return None

    def get_stripped(self):
        """Fonction utilitaire, renvoie une liste de chaines 'strippées'"""
        data = self.get()
        if data is not None:
            return [ x.strip() for x in data ]
        else:
            return []

    def foreachattr(self, callback, *args, **kwargs):
        klass = self.__class__
        for k in dir(klass):
            v = getattr( klass, k )
            if isinstance(v, JeveuxAttr):
                callback( k, v, *args, **kwargs )

    def check(self, checker=None):
        if checker is None:
            checker = CheckLog()
        # l'objet a déjà été vérifié, on ne fait rien
        if checker.checkedOJB(self):
           return checker
        checker.visitOJB( self )
        if self.exists:
            self.foreachattr( lambda k,v,obj,c: v.check(k, obj, c),
                              self, checker )
        else:
            if not self.optional and not checker.optional :
                checker.err( self, "n'existe pas (%r)" %self._parent )
        return checker

    def dump(self, indent=""):
        if self.optional:
            f = "(f)"
        else:
            f = "(o)"
        return f +" "+ self.nomj() +" "+ str(self.exists)

# -----------------------------------------------------------------------------
def Facultatif( ojb ):
    ojb.optional = True
    return ojb

# -----------------------------------------------------------------------------
class OJBVect(OJB):
    lonmax = JeveuxIntAttr("LONMAX")
    lonuti = JeveuxIntAttr("LONUTI")
    _xous = "S"
    _genr = "V"

# -----------------------------------------------------------------------------
class OJBPtnom(OJB):
    nommax = JeveuxIntAttr("NOMMAX")
    nomuti = JeveuxIntAttr("NOMUTI")
    _xous = "S"
    _genr = "N"
    _type = "K"

# -----------------------------------------------------------------------------
class OJBCollec(OJB):
    stockage = JeveuxStrAttr("STOCKAGE")
    nutioc = JeveuxIntAttr( "NUTIOC" )
    acces = JeveuxStrAttr( "ACCES" )
    modelong = JeveuxStrAttr( "MODELONG" )
    nmaxoc = JeveuxIntAttr( "NMAXOC" )

# -----------------------------------------------------------------------------
class AsVI(OJBVect):
    _type = "I"

# -----------------------------------------------------------------------------
class AsVS(OJBVect):
    _type = "S"

# -----------------------------------------------------------------------------
class AsVR(OJBVect):
    _type = "R"

# -----------------------------------------------------------------------------
class AsVC(OJBVect):
    _type = "C"

# -----------------------------------------------------------------------------
class AsVL(OJBVect):
    _type = "L"

# -----------------------------------------------------------------------------
class AsVK8(OJBVect):
    _type = "K"
    _ltyp = 8

# -----------------------------------------------------------------------------
class AsVK16(OJBVect):
    _type = "K"
    _ltyp = 16

# -----------------------------------------------------------------------------
class AsVK24(OJBVect):
    _type = "K"
    _ltyp = 24

# -----------------------------------------------------------------------------
class AsVK32(OJBVect):
    _type = "K"
    _ltyp = 32

# -----------------------------------------------------------------------------
class AsVK80(OJBVect):
    _type = "K"
    _ltyp = 80

# Pour compatibilite
AsObject = OJB
AsColl   = OJBCollec
AsPn     = OJBPtnom
AsVect   = OJBVect
