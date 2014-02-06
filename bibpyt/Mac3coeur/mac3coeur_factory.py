# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: samuel.geniaut at edf.fr

"""
Module dédié à la macro MAC3COEUR.

On définit ici les factories qui permettent de lire les fichiers datg,
et produisent les objets Assemblages ou Coeurs correspondants.
"""

import os.path as osp

class Mac3Factory(object):
    """Classe chapeau des factories."""
    prefix = ''

    def __init__(self, datg):
        """Initialisation"""
        self.rep_datg = datg
        self.cata = {}

    def _get_obj_fname(self, fname):
        """Retourne le nom du fichier définissant l'objet à importer."""
        fname = osp.join(self.rep_datg, fname + ".datg")
        assert osp.exists(fname), 'file not found %s' % fname
        return fname

    def build_supported_types(self):
        """Construit la liste des types autorisés."""
        raise NotImplementedError

    def _context_init(self):
        """Retourne un context pour l'import des modules dans datg."""
        ctxt = self.build_supported_types()
        return ctxt

    def _import_obj(self, objname):
        """Importe la classe d'un type d'objet."""
        fname = self._get_obj_fname(objname)
        ctxt = self._context_init()
        execfile(fname, ctxt)
        obj = ctxt.get(objname)
        assert obj, "No object named '%s' has been defined in the " \
                    "catalog '%s'" % (objname, fname)
        self.cata[objname] = ctxt[objname]

    def get(self, objname):
        """Retourne l'objet nommé."""
        objname = self.prefix + objname
        if self.cata.get(objname) is None:
            self._import_obj(objname)
        return self.cata[objname]
