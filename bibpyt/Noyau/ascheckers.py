# coding=utf-8
# person_in_charge: mathieu.courtois at edf.fr
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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

class Parmi(object):
    """Classe qui exprime une contrainte multiple pour un attribut"""
    def __init__(self, *args):
        self.values = list(args)

    def add_value(self, value ):
        if value not in self.values:
            self.values.append( value )

    def __call__(self, obj, name, value, log ):
        if value not in self.values:
            log.err( obj, "l'attribut %s=%r n'est pas dans %r" % (name, value, self.values) )

    def __repr__(self):
        l = [ "Parmi(", ]
        g = [ repr(v) for v in self.values ]
        l.append( ", ".join(g) )
        l.append( ")" )
        return "".join( l )

class CheckLog(object):
    """Un validateur qui enregistre toutes les erreurs trouvées.
    checkedXXX répond True si la "marq" courante est inférieure ou égale
    à la celle de la dernière vérification.
    Si on incrémentait "marq" à chaque étape, on revérifie à chaque fois.
    """

    def __init__(self):
        self.msg       = []
        self.names     = {}
        self.cksums    = {}
        self.optional  = False
        self._marq     = 1
        self._lastmarq = self._marq
        self._debug    = False
        self._profond  = False # True pour forcer des vérifications plus profondes

    def log(self, level, obj, msg ):
        if obj :
            self.msg.append( (level, obj.nomj(), msg) )
        else :
            self.msg.append( (level, 'None', msg) )

    def err(self, obj, msg ):
        self.log( 0, obj, msg )

    def warn(self, obj, msg ):
        self.log( 1, obj, msg )

    def visitOJB(self, obj):
        key = obj.nomj()
        self.names[key] = self._marq

    def checkSumOJB(self, obj, sd, maj='non'):
        # vérifie que le checksum de obj n'a pas changé
        # sd : concept qui contient obj
        # maj='maj', l'opérateur a le droit de modifier ojb
        if obj.exists :
            import md5
            m=md5.new()
            m.update(str(obj.get()))
            cksum=m.digest()
            nom=obj.nomj()
            if not self.cksums.has_key(nom) :
                self.cksums[nom]=cksum
            else :
                if self.cksums[nom] != cksum :
                    self.cksums[nom] = cksum
                    #if maj.strip()=='maj' and nom[0:8].strip()==sd.nomj.nomj[0:8].strip() :
                    # Remarque : ne pas tester 'maj' premet de résoudre (un peu) le problème
                    #            posé par la commande DETRUIRE
                    if nom[0:8].strip()==sd.nomj.nomj[0:8].strip() :
                        pass
                    else :
                        self.err(obj,'Le checksum a changé')

    def visitAsBase(self, obj):
        key = (obj.nomj(), obj.__class__.__name__)
        self.names[key] = self._marq

    def force(self, force=False):
        if not force:
           self._marq = 1
        else:
           self._lastmarq += 1
           self._marq = self._lastmarq

    def checkedOJB(self, obj):
        key = obj.nomj()
        res = self.names.get(key, 0) >= self._marq
        self.help_dbg([key,], res)
        return res

    def checkedAsBase(self, obj):
        key = (obj.nomj(), obj.__class__.__name__)
        res = self.names.get(key, 0) >= self._marq
        self.help_dbg(key, res)
        return res

    def help_dbg(self, key, res):
        if self._debug:
            if res:
               s = 'ignore'
            else:
               s = 'check '
            print '#DBG %6d %s : %s' % (self._marq, s, ', '.join(key))

    def __str__(self):
        d = { 0: "E", 1:"W" }
        return "\n".join( [ "%s:%s: %s" % (d[l],n,m)
                            for l,n,m in self.msg ])

class CheckFail(CheckLog):
    """Un validateur qui lève une exception
    dès la première erreur"""
    def err(self, obj, msg ):
        raise AssertionError("%s: %s" % (obj.nomj(), msg) )
