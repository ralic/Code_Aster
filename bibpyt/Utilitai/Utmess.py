#@ MODIF Utmess Utilitai  DATE 30/11/2004   AUTEUR MCOURTOI M.COURTOIS 
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
      code  : 'A', 'E', 'S', 'F'
      sprg  : nom du module, classe ou fonction python où l'on se trouve
      texte : contenu du message
   """
   fmt='\n <%s> <%s> %s\n\n'
   UL={
      'MESSAGE' : 6,
      'RESULTAT' : 8,
      #'ERREUR' : 9,
   }
   # On importe la définition des commandes à utiliser dans la macro
#    if jdc:
#       DEFI_FICHIER     = jdc.get_cmd('DEFI_FICHIER')
#    else:
#       # on se limite au print !
#       UL={ 'MESSAGE' : 6, }
   try:
      from Cata.cata import DEFI_FICHIER
   except ImportError:
      # on se limite au print !
      UL={ 'MESSAGE' : 6, }

   reason=fmt % (code, sprg, texte)
   
   for nom,ul in UL.items():
      if ul<>6:
         DEFI_FICHIER(ACTION='LIBERER', UNITE=ul, )
         f=open('fort.'+str(ul),'a')
      else:
         f=sys.stdout
      # écriture du message
      f.write(reason)

      if ul<>6:
         f.close()
         DEFI_FICHIER(ACTION='ASSOCIER', UNITE=ul, TYPE='ASCII', ACCES='APPEND')

   if code=='S':
      raise aster.error, reason
   elif code=='F':
      raise aster.FatalError, reason
