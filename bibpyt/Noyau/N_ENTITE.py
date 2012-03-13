#@ MODIF N_ENTITE Noyau  DATE 13/03/2012   AUTEUR COURTOIS M.COURTOIS 
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
    Ce module contient la classe ENTITE qui est la classe de base
    de toutes les classes de definition d'EFICAS.
"""

import re
import N_CR
import N_VALIDATOR

class ENTITE:
   """
      Classe de base pour tous les objets de definition : mots cles et commandes
      Cette classe ne contient que des methodes utilitaires
      Elle ne peut etre instanciee et doit d abord etre specialisee
   """
   CR=N_CR.CR
   factories={'validator':N_VALIDATOR.validatorFactory}

   def __init__(self,validators=None):
      """
         Initialise les deux attributs regles et entites d'une classe dérivée
         à : pas de règles et pas de sous-entités.

         L'attribut regles doit contenir la liste des regles qui s'appliquent
         sur ses sous-entités

         L'attribut entités doit contenir le dictionnaires des sous-entités
         (clé = nom, valeur=objet)
      """
      self.regles=()
      self.entites={}
      if validators:
         self.validators=self.factories['validator'](validators)
      else:
         self.validators=validators

   def affecter_parente(self):
      """
          Cette methode a pour fonction de donner un nom et un pere aux
          sous entités qui n'ont aucun moyen pour atteindre leur parent
          directement
          Il s'agit principalement des mots cles
      """
      for k,v in self.entites.items():
        v.pere = self
        v.nom = k

   def verif_cata(self):
      """
          Cette methode sert à valider les attributs de l'objet de définition
      """
      raise NotImplementedError("La méthode verif_cata de la classe %s doit etre implémentée"
                                % self.__class__.__name__)

   def __call__(self):
      """
          Cette methode doit retourner un objet dérivé de la classe OBJECT
      """
      raise NotImplementedError("La méthode __call__ de la classe %s doit etre implémentée"
                                % self.__class__.__name__)

   def report(self):
      """
         Cette méthode construit pour tous les objets dérivés de ENTITE un
         rapport de validation de la définition portée par cet objet
      """
      self.cr = self.CR()
      self.verif_cata()
      for k,v in self.entites.items() :
         try :
            cr = v.report()
            cr.debut = "Début "+v.__class__.__name__+ ' : ' + k
            cr.fin = "Fin "+v.__class__.__name__+ ' : ' + k
            self.cr.add(cr)
         except:
            self.cr.fatal("Impossible d'obtenir le rapport de %s %s" %(k,`v`))
            print "Impossible d'obtenir le rapport de %s %s" %(k,`v`)
            print "père =",self
      return self.cr

   def verif_cata_regles(self):
      """
         Cette méthode vérifie pour tous les objets dérivés de ENTITE que
         les objets REGLES associés ne portent que sur des sous-entités
         existantes
      """
      for regle in self.regles :
        l=[]
        for mc in regle.mcs :
          if not self.entites.has_key(mc) :
            l.append(mc)
        if l != [] :
          txt = str(regle)
          self.cr.fatal("Argument(s) non permis : %s pour la règle : %s" %(`l`,txt))

   def check_definition(self, parent):
      """Verifie la definition d'un objet composite (commande, fact, bloc)."""
      args = self.entites.copy()
      mcs = set()
      for nom, val in args.items():
         if val.label == 'SIMP':
            mcs.add(nom)
            #XXX
            #if val.max != 1 and val.type == 'TXM':
                #print "#CMD", parent, nom
         elif val.label == 'FACT':
            val.check_definition(parent)
            # CALC_SPEC !
            #assert self.label != 'FACT', \
               #'Commande %s : Mot-clef facteur present sous un mot-clef facteur : interdit !' \
               #% parent
         else:
            continue
         del args[nom]
      # seuls les blocs peuvent entrer en conflit avec les mcs du plus haut niveau
      for nom, val in args.items():
         if val.label == 'BLOC':
            mcbloc = val.check_definition(parent)
            #XXX
            #print "#BLOC", parent, re.sub('\s+', ' ', val.condition)
            assert mcs.isdisjoint(mcbloc), "Commande %s : Mot(s)-clef(s) vu(s) plusieurs fois : %s" \
               % (parent, tuple(mcs.intersection(mcbloc)))
      return mcs

