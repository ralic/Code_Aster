#@ MODIF B_EVAL Build  DATE 06/09/2004   AUTEUR MCOURTOI M.COURTOIS 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
#                                                                       
#                                                                       
# ======================================================================

# -*- coding: iso-8859-1 -*-

"""
"""
# Modules Eficas
import B_utils
from B_CODE import CODE

class EVAL(CODE):
   """
   """
   def __call__(self,etape):
      """
         Cette methode retourne la valeur évaluée de l'objet EVAL.
         Pour ce faire elle fait appel au code de calcul (module codex)
      """
      if self.val is None:
        try:
           texte_a_evaluer=B_utils.ReorganisationDe(self.valeur,80)
           self.val=self.codex.myeval(etape,texte_a_evaluer)
        except:
           print "ERREUR a l'evaluation de : ",texte_a_evaluer
           raise
      return self.val

