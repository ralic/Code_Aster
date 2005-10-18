#@ MODIF Utmess Utilitai  DATE 17/10/2005   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

import sys
import aster

def UTMESS(code, sprg, texte):
   """Utilitaire analogue à la routine fortran UTMESS.
      code  : 'A', 'E', 'S', 'F', 'I'
      sprg  : nom du module, classe ou fonction python où l'on se trouve
      texte : contenu du message
   """
   fmt='\n <%s> <%s> %s\n\n'
   sanscode='\n <%s> %s\n\n'
   UL=[
      'MESSAGE',
      'RESULTAT',
      #'ERREUR',
   ]
#
   # Comme l'UTMESS fortran, on supprime le code si on ne fait pas l'abort
   if aster.onFatalError()=='EXCEPTION':
      reason=sanscode % (sprg, texte)
   else:
      reason=fmt % (code, sprg, texte)
   
   for nom in UL:
      # écriture du message
      aster.affiche(nom,reason)

   if code=='S':
      raise aster.error, reason
   elif code=='F':
      raise aster.FatalError, reason
