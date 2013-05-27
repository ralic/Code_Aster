# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# person_in_charge: mathieu.courtois at edf.fr

"""
Ce package contient la définition des comportements.
"""

#_debug = True
_debug = False

import os
from glob import glob

from cata_comportement import CataComportementError, LoiComportement, KIT, catalc

pkgdir = os.path.dirname(__file__)
pkg = os.path.basename(pkgdir)

#
l_mod = [os.path.splitext(os.path.basename(modname))[0] \
            for modname in glob(os.path.join(pkgdir, '*.py'))]
l_mod = [modname for modname in l_mod if modname not in ('__init__', 'cata_comportement')]

for modname in l_mod:
   try:
      mod = __import__('%s.%s' % (pkg, modname), globals(), locals(), [modname])
      # liste des lois de comportements définies dans le module
      for objname in dir(mod):
         obj = getattr(mod, objname)
         if type(obj) == LoiComportement:
            if _debug:
               print '<Comportement> Module "%s" - ajout objet "%s"' % (modname, objname)
            catalc.add(obj)
   except Exception, msg:
      raise CataComportementError, 'Erreur import de "%s" : %s' % (modname, str(msg))

if _debug:
   print catalc
