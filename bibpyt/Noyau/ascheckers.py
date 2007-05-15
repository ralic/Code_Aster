#@ MODIF ascheckers Noyau  DATE 16/05/2007   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
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
    """Un validateur qui enregistre toutes
    les erreurs trouvées"""
    def __init__(self):
        self.msg = []
        self.names = {}
        self.optional = False

    def log(self, level, obj, msg ):
        self.msg.append( (level, obj.nomj(), msg) )

    def err(self, obj, msg ):
        self.log( 0, obj, msg )

    def warn(self, obj, msg ):
        self.log( 1, obj, msg )

    def visit(self, obj ):
        self.names[obj.nomj()] = 1

    def __str__(self):
        d = { 0: "E", 1:"W" }
        return "\n".join( [ "%s:%s: %s" % (d[l],n,m)
                            for l,n,m in self.msg ])

class CheckFail(CheckLog):
    """Un validateur qui lève une exception
    dès la première erreur"""
    def err(self, obj, msg ):
        raise AssertionError("%s: %s" % (obj.nomj(), msg) )
