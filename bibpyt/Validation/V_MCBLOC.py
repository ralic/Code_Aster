#@ MODIF V_MCBLOC Validation  DATE 07/09/2009   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
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


"""
   Ce module contient la classe mixin MCBLOC qui porte les méthodes
   nécessaires pour réaliser la validation d'un objet de type MCBLOC
   dérivé de OBJECT.

   Une classe mixin porte principalement des traitements et est
   utilisée par héritage multiple pour composer les traitements.
"""
# Modules Python
import string

# Modules EFICAS
import V_MCCOMPO

class MCBLOC(V_MCCOMPO.MCCOMPO):
   """
      Cette classe a un attribut de classe :

      - txt_nat qui sert pour les comptes-rendus liés à cette classe
   """

   txt_nat="Bloc :"

   def isvalid(self,sd='oui',cr='non'):
      """ 
         Methode pour verifier la validité du MCBLOC. Cette méthode
         peut etre appelée selon plusieurs modes en fonction de la valeur
         de sd et de cr.

         Si cr vaut oui elle crée en plus un compte-rendu
         sd est présent pour compatibilité de l'interface mais ne sert pas
      """
      if self.state == 'unchanged' :
        return self.valid
      else:
        valid = 1
        if hasattr(self,'valid'):
          old_valid = self.valid
        else:
          old_valid = None
        for child in self.mc_liste :
          if not child.isvalid():
            valid = 0
            break
        # Après avoir vérifié la validité de tous les sous-objets, on vérifie
        # la validité des règles
        text_erreurs,test_regles = self.verif_regles()
        if not test_regles :
          if cr == 'oui' : self.cr.fatal(string.join(("Règle(s) non respectée(s) :", text_erreurs)))
          valid = 0
        self.valid = valid
        self.state = 'unchanged'
        if not old_valid or old_valid != self.valid : 
           self.init_modif_up()
        return self.valid

