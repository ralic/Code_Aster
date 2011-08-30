#@ MODIF V_MCCOMPO Validation  DATE 30/08/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
#
#
# ======================================================================


"""
   Ce module contient la classe  de base MCCOMPO qui sert à factoriser
   les traitements des objets composites de type OBJECT
"""
# Modules Python
import string,types
import traceback

# Modules EFICAS
from Noyau import N_CR
from Noyau.N_Exception import AsException

class MCCOMPO:
   """
       L'attribut mc_liste a été créé par une classe dérivée de la
       classe MCCOMPO du Noyau
   """

   CR=N_CR.CR

   def __init__(self):
      self.state='undetermined'

   def init_modif_up(self):
      """
         Propage l'état modifié au parent s'il existe et n'est pas l'objet
         lui-meme
      """
      if self.parent and self.parent != self :
        self.parent.state = 'modified'

   def report(self):
      """
          Génère le rapport de validation de self
      """
      self.cr=self.CR()
      self.cr.debut = self.txt_nat+self.nom
      self.cr.fin = "Fin "+self.txt_nat+self.nom
      for child in self.mc_liste:
        self.cr.add(child.report())
      self.state = 'modified'
      try:
        self.isvalid(cr='oui')
      except AsException,e:
        if CONTEXT.debug : traceback.print_exc()
        self.cr.fatal(string.join((self.txt_nat,self.nom,str(e))))
      return self.cr

   def verif_regles(self):
      """
         A partir du dictionnaire des mots-clés présents, vérifie si les règles
         de self sont valides ou non.

         Retourne une string et un booléen :

           - texte = la string contient le message d'erreur de la (les) règle(s) violée(s) ('' si aucune)

           - testglob = booléen 1 si toutes les règles OK, 0 sinon
      """
      #dictionnaire=self.dict_mc_presents(restreint='oui')
      dictionnaire=self.dict_mc_presents(restreint='non') # On verifie les regles avec les defauts affectés
      texte='\n'
      testglob = 1
      for r in self.definition.regles:
        erreurs,test=r.verif(dictionnaire)
        testglob = testglob*test
        if erreurs != '':
          if len(texte) > 1 :
            texte=texte+'\n'+erreurs
          else :
            texte = texte + erreurs
      return texte,testglob

   def dict_mc_presents(self,restreint='non'):
      """
          Retourne le dictionnaire {mocle : objet} construit à partir de self.mc_liste
          Si restreint == 'non' : on ajoute tous les mots-clés simples du catalogue qui ont
          une valeur par défaut
          Si restreint == 'oui' : on ne prend que les mots-clés effectivement entrés par
          l'utilisateur (cas de la vérification des règles)
      """
      dico={}
      # on ajoute les couples {nom mot-clé:objet mot-clé} effectivement présents
      for v in self.mc_liste:
        if v == None : continue
        k=v.nom
        dico[k]=v
      if restreint == 'oui' : return dico
      # Si restreint != 'oui',
      # on ajoute les couples {nom mot-clé:objet mot-clé} des mots-clés simples
      # possibles pour peu qu'ils aient une valeur par défaut
      for k,v in self.definition.entites.items():
        if v.label != 'SIMP' : continue
        if not v.defaut : continue
        if not dico.has_key(k):
          dico[k]=v(nom=k,val=None,parent=self)
      #on ajoute l'objet detenteur de regles pour des validations plus sophistiquees (a manipuler avec precaution)
      dico["self"]=self
      return dico



