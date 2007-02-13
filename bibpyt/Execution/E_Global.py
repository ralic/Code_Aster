#@ MODIF E_Global Execution  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

# RESPONSABLE MCOURTOI M.COURTOIS

"""
   Module dans lequel on définit des fonctions pour la phase d'exécution.
   Ces fonctions sont indépendantes des étapes (sinon elles seraient dans
   B_ETAPE/E_ETAPE) et des concepts/ASSD, et elles sont destinées à etre
   appelées par le fortran via astermodule.c.

   Ce module sera accessible via la variable globale `static_module`
   de astermodule.c.
"""

# ------------------------------------------------------------------------
def utprin(typmess, unite, idmess, valk, vali, valr):
   """
      Cette methode permet d'imprimer un message venu d'U2MESG
   """
   from Messages import utprin
   utprin.utprin(typmess,unite,idmess,valk,vali,valr)

# ------------------------------------------------------------------------
def checksd(nomsd, typesd):
   """Vérifie la validité de la SD `nom_sd` (nom jeveux) de type `typesd`.
   Exemple : typesd = sd_maillage
   C'est le pendant de la "SD.checksd.check" à partir d'objets nommés.
   Code retour :
      0 : tout est ok
      1 : erreurs lors du checksd
      4 : on n'a meme pas pu tester
   """
   nomsd  = nomsd.strip()
   typesd = typesd.lower().strip()
   
   # import
   iret = 4
   try:
      sd_module = __import__('SD.%s' % typesd, globals(), locals(), [typesd])
   except ImportError, msg:
      UTMESS('F', 'checksd', "Impossible d'importer le catalogue de la SD '%s'" % typesd)
      return iret
   
   # on récupère la classe typesd
   clas = getattr(sd_module, typesd, None)
   if clas:
      objsd = clas(nomj=nomsd)
      chk = objsd.check()
      ichk = min([1,] + [level for level, obj, msg in chk.msg])
      if ichk == 0:
         iret = 1
      else:
         iret = 0
   
   return iret

