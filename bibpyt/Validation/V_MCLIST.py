#@ MODIF V_MCLIST Validation  DATE 29/05/2002   AUTEUR DURAND C.DURAND 
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
   Ce module contient la classe mixin MCList qui porte les méthodes
   nécessaires pour réaliser la validation d'un objet de type MCList
   dérivé de OBJECT.

   Une classe mixin porte principalement des traitements et est
   utilisée par héritage multiple pour composer les traitements.
"""
# Modules Python
import string
import traceback

# Modules EFICAS
from Noyau import N_CR
from Noyau.N_Exception import AsException

class MCList:
   """
      Cette classe a deux attributs de classe :

      - CR qui sert à construire l'objet compte-rendu

      - txt_nat qui sert pour les comptes-rendus liés à cette classe
   """

   CR=N_CR.CR
   txt_nat="Mot cle Facteur Multiple :"

   def isvalid(self,cr='non'):
      """ 
         Methode pour verifier la validité du MCList. Cette méthode
         peut etre appelée selon plusieurs modes en fonction de la valeur
         de cr.

         Si cr vaut oui elle crée en plus un compte-rendu.

         On n'utilise pas d'attribut pour stocker l'état et on ne remonte pas 
         le changement d'état au parent (pourquoi ??)
      """
      if len(self.data) == 0 : return 0
      num = 0
      test = 1
      for i in self.data:
        num = num+1
        if not i.isvalid():
          if cr=='oui':
            self.cr.fatal(string.join(["L'occurrence n",`num`," du mot-clé facteur :",self.nom," n'est pas valide"]))
          test = 0
      return test

   def report(self):
      """ 
          Génère le rapport de validation de self 
      """
      self.cr=self.CR( debut = "Mot-clé facteur multiple : "+self.nom,
                  fin = "Fin Mot-clé facteur multiple : "+self.nom)
      for i in self.data:
        self.cr.add(i.report())
      # XXX j'ai mis l'état en commentaire car il n'est utilisé ensuite
      #self.state = 'modified'
      try :
        self.isvalid(cr='oui')
      except AsException,e:
        if CONTEXT.debug : traceback.print_exc()
        self.cr.fatal(string.join(["Mot-clé facteur multiple : ",self.nom,str(e)]))
      return self.cr

