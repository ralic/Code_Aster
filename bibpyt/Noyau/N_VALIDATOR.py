#@ MODIF N_VALIDATOR Noyau  DATE 09/09/2003   AUTEUR DURAND C.DURAND 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
"""
   Ce module contient toutes les classes necessaires pour
   implanter le concept de validateur dans Accas
"""
import types,exceptions

class ValError ( exceptions.Exception ):
      pass

class Valid:
   """
        Cette classe est la classe mere des validateurs Accas
        Elle doit etre derivee 
        Elle ne presente que la signature des methodes
        indispensables pour son bon fonctionnement

        @ivar cata_info: raison de la validite ou de l'invalidite du validateur meme
        @type cata_info: C{string}
   """
   def __init__(self,*tup,**args):
       """ 
           Cette methode sert a initialiser les attributs du validateur 
       """
       self.cata_info=""
       raise "Must be implemented"

   def info(self):
       return "valeur valide" 

   def verif(self,valeur):
       """
           Cette methode sert a verifier si la valeur passee en argument est consideree
           comme valide ou non par le validateur. Dans le premier cas le validateur retourne 1
           (valide) sinon 0 (invalide).

           @type valeur: tout type python
           @param valeur: valeur du mot cle a valider
           @rtype: C{boolean}
           @return: indicateur de validite 1 (valide) ou 0 (invalide)
       """
       raise "Must be implemented"
   
   def error(self,valeur):
       return 0

   def verif_cata(self):
       """
           Cette methode sert a realiser des verifications du validateur lui meme.
           Elle est facultative et retourne 1 (valide) par defaut.
           Elle retourne 0 si le validateur est lui meme invalide si par exemple ses
           parametres de definition ne sont pas corrects.
           La raison de l'invalidite est stockee dans l'attribut cata_info.

           @rtype: C{boolean}
           @return: indicateur de validite 1 (valide) ou 0 (invalide)
       """
       return 1

class RangeVal(Valid):
      """
          Exemple de classe validateur : verification qu'une valeur
          est dans un intervalle.
          Pour une liste on verifie que tous les elements sont 
          dans l'intervalle
          Susceptible de remplacer les attributs "vale_min" "vale_max"
          dans les catalogues
      """
      def __init__(self,low,high):
          self.low=low
          self.high=high
          self.cata_info="%s doit etre inferieur a %s" %(low,high)

      def info(self):
          return "valeur dans l'intervalle %s , %s" %(self.low,self.high)

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             for val in valeur:
                if val < self.low :return 0
                if val > self.high:return 0
             return 1
          else:
             if valeur < self.low :return 0
             if valeur > self.high:return 0
             return 1

      def verif_cata(self):
          if self.low > self.high : return 0
          return 1

class CardVal(Valid):
      """
          Exemple de classe validateur : verification qu'une liste est
          d'une longueur superieur a un minimum (min) et inferieure
          a un maximum (max).
          Susceptible de remplacer les attributs "min" "max" dans les
          catalogues
      """
      def __init__(self,min='**',max='**'):
          self.min=min
          self.max=max  
          self.cata_info="%s doit etre inferieur a %s" % (min,max)

      def info(self):
          return "longueur comprise entre  %s et %s" % (self.min,self.max)

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             if self.max != '**' and len(valeur) > self.max:return 0
             if self.min != '**' and len(valeur) < self.min:return 0
             return 1
          else:
             if self.max != '**' and 1 > self.max:return 0
             if self.min != '**' and 1 < self.min:return 0
             return 1

      def verif_cata(self):
          if self.min != '**' and self.max != '**' and self.min > self.max : return 0
          return 1

class PairVal(Valid):
      """
          Exemple de classe validateur : verification qu'une valeur
          est paire.
          Pour une liste on verifie que tous les elements sont
          pairs
      """
      def __init__(self):
          self.cata_info=""

      def info(self):
          return "valeur paire"

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             for val in valeur:
                if val % 2 != 0:return 0
             return 1
          else:
             if valeur % 2 != 0:return 0
             return 1

class EnumVal(Valid):
      """
          Exemple de classe validateur : verification qu'une valeur
          est prise dans une liste de valeurs.
          Susceptible de remplacer l attribut "into" dans les catalogues
      """
      def __init__(self,into=()):
          if type(into) not in (types.ListType,types.TupleType): into=(into,)
          self.into=into
          self.cata_info=""

      def info(self):
          return "valeur dans %s" % `self.into`

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             for val in valeur:
                if val not in self.into:return 0
             return 1
          else:
             if valeur not in self.into:return 0
             return 1

class NoRepeat(Valid):
      """
          Verification d'absence de doublons dans la liste.
      """
      def __init__(self):
          self.cata_info=""

      def info(self):
          return ": présence de doublon dans la liste"

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             liste=list(valeur)
             for val in liste:
                if liste.count(val)!=1 : return 0
             return 1
          else:
             return 1

class LongStr(Valid):
      """
          Verification de la longueur d une chaine
      """
      def __init__(self,low,high):
          self.low=low
          self.high=high
          self.cata_info=""

      def info(self):
          return "longueur de la chaine entre %s et %s" %(self.low,self.high)

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             for val in valeur:
                if len(val) < self.low :return 0
                if len(val) > self.high:return 0
             return 1
          else:
             if len(valeur) < self.low :return 0
             if len(valeur) > self.high:return 0
             return 1

class OrdList(Valid):
      """
          Verification qu'une liste est croissante ou decroissante
      """
      def __init__(self,ord):
          self.ord=ord
          self.cata_info=""

      def info(self):
          return "liste %s" % self.ord

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             if self.ord=='croissant':
                var=valeur[0]
                for val in valeur[1:]:
                   if val<var:return 0
                   var=val
                return 1
             elif self.ord=='decroissant':
                var=valeur[0]
                for val in valeur[1:]:
                   if val>var:return 0
                   var=val
                return 1
          else:
             return 1


CoercableFuncs = { types.IntType:     int,
                   types.LongType:    long,
                   types.FloatType:   float,
                   types.ComplexType: complex,
                   types.UnicodeType: unicode }

class TypeVal(Valid):
      """
          Cette classe est un validateur qui controle qu'une valeur
          est bien du type Python attendu.
          Pour une liste on verifie que tous les elements sont du bon type.
      """
      def __init__(self, aType):
          if type(aType) != types.TypeType:
             aType=type(aType)
          self.aType=aType
          try:
             self.coerce=CoercableFuncs[ aType ]
          except:
             self.coerce = self.identity

      def info(self):
          return "valeur de %s" % self.aType

      def identity ( self, value ):
          if type( value ) == self.aType:
             return value
          raise ValError

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             for val in valeur:
                 try:
                    self.coerce(val)
                 except:
                    return 0
             return 1
          else:
             try:
                self.coerce(valeur)
             except:
                return 0
             return 1

class InstanceVal(Valid):
      """
          Cette classe est un validateur qui controle qu'une valeur est
          bien une instance (au sens Python) d'une classe
          Pour une liste on verifie chaque element de la liste
      """
      def __init__(self,aClass):
          if type(aClass) == types.InstanceType:
             aClass=aClass.__class__
          self.aClass=aClass

      def info(self):
          return "valeur d'instance de %s" % self.aClass.__name__

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             for val in valeur:
                 if not isinstance(val,self.aClass): return 0
             return 1
          if not isinstance(valeur,self.aClass): return 0
          return 1

def ImpairVal(valeur):
    """
        Cette fonction est un validateur. Elle verifie que la valeur passee
        est bien un nombre impair.
    """
    if type(valeur) in (types.ListType,types.TupleType):
       for val in valeur:
           if val % 2 != 1:return 0
       return 1
    else:
       if valeur % 2 != 1:return 0
       return 1

ImpairVal.info="valeur impaire"
    
class F1Val(Valid):
      """
          Cette classe est un validateur de dictionnaire (mot cle facteur ?). Elle verifie
          que la somme des cles A et B vaut une valeur donnee
          en parametre du validateur
      """
      def __init__(self,somme=10):
          self.somme=somme
          self.cata_info=""

      def info(self):
          return "valeur %s pour la somme des cles A et B " % self.somme

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             for val in valeur:
                if not val.has_key("A"):return 0
                if not val.has_key("B"):return 0
                if val["A"]+val["B"]  != self.somme:return 0
             return 1
          else:
             if not valeur.has_key("A"):return 0
             if not valeur.has_key("B"):return 0
             if valeur["A"]+valeur["B"]  != self.somme:return 0
             return 1

class FunctionVal(Valid):
      """
          Cette classe est un validateur qui est initialise avec une fonction
      """
      def __init__(self,function):
          self.function=function

      def info(self):
          return self.function.info

      def verif(self,valeur):
          return self.function(valeur)

class OrVal(Valid):
      """
          Cette classe est un validateur qui controle une liste de validateurs
          Elle verifie qu'au moins un des validateurs de la liste a valide la valeur
      """
      def __init__(self,validators=()):
          if type(validators) not in (types.ListType,types.TupleType): validators=(validators,)
          self.validators=[]
          for validator in validators:
              if type(validator) == types.FunctionType:
                 self.validators.append(FunctionVal(validator))
              else:
                 self.validators.append(validator)
          self.cata_info=""

      def info(self):
          return "\n ou ".join([v.info() for v in self.validators])

      def verif(self,valeur):
          for validator in self.validators:
              v=validator.verif(valeur)
              if v :
                 return 1
          return 0

      def verif_cata(self):
          infos=[]
          for validator in self.validators:
              v=validator.verif_cata()
              if not v :infos.append(validator.cata_info)
          if infos:
             self.cata_info="\n".join(infos)
             return 0
          self.cata_info=""
          return 1

class AndVal(Valid):
      """
          Cette classe est un validateur qui controle une liste de validateurs
          Elle verifie que tous les validateurs de la liste sont positifs
      """
      def __init__(self,validators=()):
          if type(validators) not in (types.ListType,types.TupleType): validators=(validators,)
          self.validators=[]
          for validator in validators:
              if type(validator) == types.FunctionType:
                 self.validators.append(FunctionVal(validator))
              else:
                 self.validators.append(validator)
          self.cata_info=""

      def info(self):
          return " et ".join([v.info() for v in self.validators])

      def verif(self,valeur):
          for validator in self.validators:
              v=validator.verif(valeur)
              if not v :
                 self.local_info=validator.info()
                 return 0
          return 1

      def verif_cata(self):
          infos=[]
          for validator in self.validators:
              v=validator.verif_cata()
              if not v :infos.append(validator.cata_info)
          if infos:
             self.cata_info="\n".join(infos)
             return 0
          self.cata_info=""
          return 1

def do_liste(validators):
    """ 
       Convertit une arborescence de validateurs en OrVal ou AndVal
       validators est une liste de validateurs ou de listes ou de tuples
    """
    valids=[]
    for validator in validators:
        if type(validator) == types.FunctionType:
           valids.append(FunctionVal(validator))
        elif type(validator) == types.TupleType:
           valids.append(OrVal(do_liste(validator)))
        elif type(validator) == types.ListType:
           valids.append(AndVal(do_liste(validator)))
        else:
           valids.append(validator)
    return valids

def validatorFactory(validator):
    if type(validator) == types.FunctionType:
       return FunctionVal(validator)
    elif type(validator) == types.TupleType:
       return OrVal(do_liste(validator))
    elif type(validator) == types.ListType:
       return AndVal(do_liste(validator))
    else:
       return validator
