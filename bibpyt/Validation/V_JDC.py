#@ MODIF V_JDC Validation  DATE 13/03/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
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
#                                                                       
#                                                                       
# ======================================================================


"""
   Ce module contient la classe mixin JDC qui porte les méthodes
   nécessaires pour réaliser la validation d'un objet de type JDC
   dérivé de OBJECT.

   Une classe mixin porte principalement des traitements et est
   utilisée par héritage multiple pour composer les traitements.
"""
# Modules Python
import string,types

# Modules EFICAS
import V_MCCOMPO
from Noyau.N_Exception import AsException
from Noyau.N_utils import AsType

class JDC(V_MCCOMPO.MCCOMPO):
   """
   """

   def report(self):
      """ 
          Methode pour generation d un rapport de validite
      """
      self.cr.purge()
      self.cr.debut="DEBUT CR validation : "+self.nom
      self.cr.fin="FIN CR validation :"+self.nom
      for e in self.etapes :
        if e.isactif():
          self.cr.add(e.report())
      self.state = 'modified'
      self.isvalid(cr='oui')
      return self.cr

   def isvalid(self,cr='non'):
      """
        Méthode booléenne qui retourne 0 si le JDC est invalide, 1 sinon
      """
      # FR : on prend en compte l'état du JDC ('unchanged','modified','undetermined')
      # afin d'accélérer le test de validité du JDC 
      if self.state == 'unchanged':
        return self.valid
      else:
        valid = 1
        texte,test = self.verif_regles()
        if test == 0:
          if cr == 'oui': self.cr.fatal(string.strip(texte))
          valid = 0
        if valid :
          for e in self.etapes:
            if not e.isactif() : continue
            if not e.isvalid():
              valid = 0
              break
        self.state="unchanged"
        self.valid = valid
        return self.valid

   def verif_regles(self):
      """
      Effectue la vérification de validité des règles du jeu de commandes
      """
      noms_etapes = [etape.nom for etape in self.etapes]
      texte_global = ''
      test_global = 1
      for regle in self.regles:
        texte, test = regle.verif(noms_etapes)
        texte_global = texte_global + texte
        test_global = test_global*test
      return texte_global, test_global

