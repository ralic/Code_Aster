# coding=utf-8
# person_in_charge: mathieu.courtois at edf.fr
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
   Ce module contient la classe mixin MCSIMP qui porte les méthodes
   nécessaires pour réaliser la validation d'un objet de type MCSIMP
   dérivé de OBJECT.

   Une classe mixin porte principalement des traitements et est
   utilisée par héritage multiple pour composer les traitements.
"""
# Modules Python
import traceback

# Modules EFICAS
from Noyau import N_CR
from Noyau.N_Exception import AsException
from Noyau.N_VALIDATOR import ValError,TypeProtocol,CardProtocol,IntoProtocol
from Noyau.N_VALIDATOR import listProto
from Noyau.strfunc import ufmt

class MCSIMP:
   """
      COMMENTAIRE CCAR:
      Cette classe est quasiment identique à la classe originale d'EFICAS
      a part quelques changements cosmétiques et des chagements pour la
      faire fonctionner de facon plus autonome par rapport à l'environnement
      EFICAS

      A mon avis, il faudrait aller plus loin et réduire les dépendances
      amont au strict nécessaire.

          - Est il indispensable de faire l'évaluation de la valeur dans le contexte
            du jdc dans cette classe.

          - Ne pourrait on pas doter les objets en présence des méthodes suffisantes
            pour éviter les tests un peu particuliers sur GEOM, PARAMETRE et autres. J'ai
            d'ailleurs modifié la classe pour éviter l'import de GEOM
   """

   CR=N_CR.CR

   def __init__(self):
      self.state='undetermined'
      self.typeProto=TypeProtocol("type",typ=self.definition.type)
      self.intoProto=IntoProtocol("into",into=self.definition.into,val_min=self.definition.val_min,val_max=self.definition.val_max)
      self.cardProto=CardProtocol("card",min=self.definition.min,max=self.definition.max)

   def get_valid(self):
       if hasattr(self,'valid'):
          return self.valid
       else:
          self.valid=None
          return None

   def set_valid(self,valid):
       old_valid=self.get_valid()
       self.valid = valid
       self.state = 'unchanged'
       if not old_valid or old_valid != self.valid :
           self.init_modif_up()

   def isvalid(self,cr='non'):
      """
         Cette méthode retourne un indicateur de validité de l'objet de type MCSIMP

           - 0 si l'objet est invalide
           - 1 si l'objet est valide

         Le paramètre cr permet de paramétrer le traitement. Si cr == 'oui'
         la méthode construit également un comte-rendu de validation
         dans self.cr qui doit avoir été créé préalablement.
      """
      if self.state == 'unchanged':
        return self.valid
      else:
        valid = 1
        v=self.valeur
        #  verification presence
        if self.isoblig() and v == None :
          if cr == 'oui' :
            self.cr.fatal(_(u"Mot-clé : %s obligatoire non valorisé"), self.nom)
          valid = 0

        lval=listProto.adapt(v)
        if lval is None:
           valid=0
           if cr == 'oui' :
              self.cr.fatal(_(u"None n'est pas une valeur autorisée"))
        else:
           # type,into ...
           #typeProto=TypeProtocol("type",typ=self.definition.type)
           #intoProto=IntoProtocol("into",into=self.definition.into,val_min=self.definition.val_min,val_max=self.definition.val_max)
           #cardProto=CardProtocol("card",min=self.definition.min,max=self.definition.max)
           #typeProto=self.definition.typeProto
           #intoProto=self.definition.intoProto
           #cardProto=self.definition.cardProto
           typeProto=self.typeProto
           intoProto=self.intoProto
           cardProto=self.cardProto
           if cr == 'oui' :
               #un cr est demandé : on collecte tous les types d'erreur
               try:
                   for val in lval:
                       typeProto.adapt(val)
               except ValError,e:
                   valid=0
                   self.cr.fatal(*e)
               try:
                   for val in lval:
                       intoProto.adapt(val)
               except ValError,e:
                   valid=0
                   self.cr.fatal(*e)
               try:
                   cardProto.adapt(lval)
               except ValError,e:
                   valid=0
                   self.cr.fatal(*e)
               #
               # On verifie les validateurs s'il y en a et si necessaire (valid == 1)
               #
               if valid and self.definition.validators:
                   try:
                       self.definition.validators.convert(lval)
                   except ValError,e:
                       self.cr.fatal(_(u"Mot-clé %s invalide : %s\nCritère de validité: %s"),
                            self.nom, str(e), self.definition.validators.info())
                       valid=0
           else:
               #si pas de cr demande, on sort a la toute premiere erreur
               try:
                   for val in lval:
                       typeProto.adapt(val)
                       intoProto.adapt(val)
                   cardProto.adapt(lval)
                   if self.definition.validators:
                       if hasattr(self.definition.validators,'set_MCSimp'):
                          self.definition.validators.set_MCSimp(self)
                       self.definition.validators.convert(lval)
               except ValError,e:
                   valid=0

        self.set_valid(valid)
        return self.valid

   def isoblig(self):
      """ indique si le mot-clé est obligatoire
      """
      return self.definition.statut=='o'

   def init_modif_up(self):
      """
         Propage l'état modifié au parent s'il existe et n'est l'objet
         lui-meme
      """
      if self.parent and self.parent != self :
        self.parent.state = 'modified'

   def report(self):
      """ génère le rapport de validation de self """
      self.cr=self.CR()
      self.cr.debut = u"Mot-clé simple : "+self.nom
      self.cr.fin = u"Fin Mot-clé simple : "+self.nom
      self.state = 'modified'
      try:
        self.isvalid(cr='oui')
      except AsException,e:
        if CONTEXT.debug : traceback.print_exc()
        self.cr.fatal(_(u"Mot-clé simple : %s %s"), self.nom, e)
      return self.cr
