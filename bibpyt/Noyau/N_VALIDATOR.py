#@ MODIF N_VALIDATOR Noyau  DATE 06/09/2004   AUTEUR MCOURTOI M.COURTOIS 
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

# -*- coding: iso-8859-1 -*-

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
        Elle presente la signature des methodes indispensables pour son bon 
        fonctionnement et dans certains cas leur comportement par défaut.

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
       """
          Cette methode retourne une chaine de caractères informative sur
          la validation demandée par le validateur. Elle est utilisée
          pour produire le compte-rendu de validité du mot clé associé.
       """
       return "valeur valide" 

   def aide(self):
       """
          Cette methode retourne une chaine de caractère qui permet 
          de construire un message d'aide en ligne.
          En général, le message retourné est le meme que celui retourné par la
          méthode info.
       """
       return self.info()

   def info_erreur_item(self):
       """
          Cette méthode permet d'avoir un message d'erreur pour un item
          dans une liste dans le cas ou le validateur fait des vérifications
          sur les items d'une liste. Si le validateur fait seulement des
          vérifications sur la liste elle meme et non sur ses items, la méthode
          doit retourner une chaine vide.
       """
       return " "

   def info_erreur_liste(self):
       """
          Cette méthode a un comportement complémentaire de celui de
          info_erreur_item. Elle retourne un message d'erreur lié uniquement
          aux vérifications sur la liste elle meme et pas sur ses items.
          Dans le cas où le validateur ne fait pas de vérification sur des
          listes, elle retourne une chaine vide
       """
       return " "

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
   
   def verif_item(self,valeur):
       """
          La methode verif du validateur effectue une validation complete de
          la valeur. valeur peut etre un scalaire ou une liste. Le validateur
          doit traiter les 2 aspects s'il accepte des listes (dans ce cas la
          methode is_list doit retourner 1).
          La methode valid_item sert pour effectuer des validations partielles
          de liste. Elle doit uniquement verifier la validite d'un item de
          liste mais pas les caracteristiques de la liste.
       """
       return 0

   def valide_liste_partielle(self,liste_courante):
       """
          Cette methode retourne un entier qui indique si liste_courante est partiellement valide (valeur 1)
          ou invalide (valeur 0). La validation partielle concerne les listes en cours de construction : on
          veut savoir si la liste en construction peut etre complétée ou si elle peut déjà etre considérée
          comme invalide.
          En général un validateur effectue la meme validation pour les listes partielles et les
          listes complètes.
       """
       return self.verif(liste_courante)

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

   def is_list(self):
       """
          Cette méthode retourne un entier qui indique si le validateur
          permet les listes (valeur 1) ou ne les permet pas (valeur 0).
          Par défaut, un validateur n'autorise que des scalaires.
       """
       return 0

   def has_into(self):
       """
          Cette méthode retourne un entier qui indique si le validateur
          propose une liste de choix (valeur 1) ou n'en propose pas.
          Par défaut, un validateur n'en propose pas.
       """
       return 0

   def get_into(self,liste_courante=None,into_courant=None):
       """
          Cette méthode retourne la liste de choix proposée par le validateur.
          Si le validateur ne propose pas de liste de choix, la méthode
          retourne None.
          L'argument d'entrée liste_courante, s'il est différent de None, donne
          la liste des choix déjà effectués par l'utilisateur. Dans ce cas, la
          méthode get_into doit calculer la liste des choix en en tenant
          compte. Par exemple, si le validateur n'autorise pas les répétitions,
          la liste des choix retournée ne doit pas contenir les choix déjà
          contenus dans liste_courante.
          L'argument d'entrée into_courant, s'il est différent de None, donne
          la liste des choix proposés par d'autres validateurs. Dans ce cas,
          la méthode get_into doit calculer la liste des choix à retourner
          en se limitant à cette liste initiale. Par exemple, si into_courant
          vaut (1,2,3) et que le validateur propose la liste de choix (3,4,5),
          la méthode ne doit retourner que (3,).

          La méthode get_into peut retourner une liste vide [], ce qui veut
          dire qu'il n'y a pas (ou plus) de choix possible. Cette situation
          peut etre normale : l''utilisateur a utilisé tous les choix, ou
          résulter d'une incohérence des validateurs :
          choix parmi (1,2,3) ET choix parmi (4,5,6). Il est impossible de
          faire la différence entre ces deux situations.
       """
       return into_courant

   def is_eval(self,valeur):
       """
           Cette méthode indique si valeur est un objet de type EVAL ou autre
           que l'on ne cherchera pas à evaluer et qui doit etre considere
           comme toujours valide. Si c'est un objet de ce type elle retourne
           la valeur 1 sinon la valeur 0
       """
       return type(valeur) == types.InstanceType and valeur.__class__.__name__ in ('EVAL',
                    'entier','reel','chaine', 'complexe','liste','PARAMETRE_EVAL')

   def is_param(self,valeur):
       """
           Cette méthode indique si valeur est un objet de type PARAMETRE
           dont on cherchera à evaluer la valeur (valeur.valeur)
       """
       return type(valeur) == types.InstanceType and valeur.__class__.__name__ in ('PARAMETRE',)

   def is_unknown(self,valeur):
       """
           Cette méthode indique si valeur est un objet de type inconnu
           c'est à dire ni de type EVAL ni de type PARAMETRE
       """
       return type(valeur) == types.InstanceType and valeur.__class__.__name__ not in ('EVAL',
                    'entier','reel','chaine', 'complexe','liste','PARAMETRE_EVAL','PARAMETRE')

class ListVal(Valid):
   """
       Cette classe sert de classe mère pour tous les validateurs qui acceptent
       des listes.
   """
   def is_list(self):
       return 1

   def get_into(self,liste_courante=None,into_courant=None):
       """
          Cette méthode get_into effectue un traitement général qui consiste
          a filtrer la liste de choix into_courant, si elle existe, en ne
          conservant que les valeurs valides (appel de la méthode valid).
       """
       if into_courant is None:
          return None
       else:
          liste_choix=[]
          for e in into_courant:
              if self.verif(e):
                 liste_choix.append(e)
          return liste_choix

   def verif(self,valeur):
       """
          Méthode verif pour les validateurs de listes. Cette méthode
          fait appel à la méthode verif_item sur chaque élément de la 
          liste. Si valeur est un paramètre, on utilise sa valeur effective
          valeur.valeur.
       """
       if self.is_param(valeur):
          valeur=valeur.valeur
       if type(valeur) in (types.ListType,types.TupleType):
          for val in valeur:
              if not self.verif_item(val):
                 return 0
          return 1
       else:
          return self.verif_item(valeur)

class RangeVal(ListVal):
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

      def verif_item(self,valeur):
          return valeur > self.low and valeur < self.high

      def info_erreur_item(self) :
          return "La valeur doit etre comprise entre %s et %s" % (self.low,
                                                                  self.high)

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
          return "longueur de liste comprise entre  %s et %s" % (self.min,self.max)

      def info_erreur_liste(self):
          return "Le cardinal de la liste doit etre compris entre %s et %s" % (self.min,self.max)

      def is_list(self):
          return self.max == '**' or self.max > 1

      def get_into(self,liste_courante=None,into_courant=None):
          if into_courant is None:
             return None
          elif liste_courante is None:
             return into_courant
          elif self.max == '**':
             return into_courant
          elif len(liste_courante) < self.max:
             return into_courant
          else:
             return []

      def verif_item(self,valeur):
          return 1

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

      def valide_liste_partielle(self,liste_courante=None):
          validite=1
          if liste_courante != None :
             if len(liste_courante) > self.max :
                validite=0
          return validite

class PairVal(ListVal):
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

      def info_erreur_item(self):
          return "La valeur saisie doit etre paire"

      def verif_item(self,valeur):
          if type(valeur) == types.InstanceType:
             if self.is_eval(valeur):
                return 1
             elif self.is_param(valeur):
                valeur=valeur.valeur
             else:
                return 0
          return valeur % 2 == 0

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             for val in valeur:
                if val % 2 != 0:return 0
             return 1
          else:
             if valeur % 2 != 0:return 0
             return 1

class EnumVal(ListVal):
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

      def verif_item(self,valeur):
          if valeur not in self.into:return 0
          return 1

      def has_into(self):
          return 1

      def get_into(self,liste_courante=None,into_courant=None):
          if into_courant is None:
             liste_choix= list(self.into)
          else:
             liste_choix=[]
             for e in into_courant:
                 if e in self.into:
                    liste_choix.append(e)
          return liste_choix

      def info_erreur_item(self):
          return "La valeur n'est pas dans la liste des choix possibles"

class NoRepeat(ListVal):
      """
          Verification d'absence de doublons dans la liste.
      """
      def __init__(self):
          self.cata_info=""

      def info(self):
          return ": pas de présence de doublon dans la liste"

      def info_erreur_liste(self):
          return "Les doublons ne sont pas permis"

      def verif_item(self,valeur):
          return 1

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             liste=list(valeur)
             for val in liste:
                if liste.count(val)!=1 : return 0
             return 1
          else:
             return 1

      def get_into(self,liste_courante=None,into_courant=None):
          """
          Methode get_into spécifique pour validateur NoRepeat, on retourne
          une liste de choix qui ne contient aucune valeur de into_courant
          déjà contenue dans liste_courante
          """
          if into_courant is None:
             liste_choix=None
          else:
             liste_choix=[]
             for e in into_courant:
                 if e in liste_choix: continue
                 if liste_courante is not None and e in liste_courante: continue
                 liste_choix.append(e)
          return liste_choix

class LongStr(ListVal):
      """
          Verification de la longueur d une chaine
      """
      def __init__(self,low,high):
          self.low=low
          self.high=high
          self.cata_info=""

      def info(self):
          return "longueur de la chaine entre %s et %s" %(self.low,self.high)

      def info_erreur_item(self):
          return "Longueur de la chaine incorrecte"

      def verif_item(self,valeur):
          low=self.low
          high=self.high
          if valeur[0]=="'" and valeur[-1]=="'" :
             low=low+2
             high=high+2
          if len(valeur) < low :return 0
          if len(valeur) > high:return 0
          return 1

class OrdList(ListVal):
      """
          Verification qu'une liste est croissante ou decroissante
      """
      def __init__(self,ord):
          self.ord=ord
          self.cata_info=""

      def info(self):
          return "liste %s" % self.ord

      def info_erreur_liste(self) :
          return "La liste doit etre en ordre "+self.ord

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

      def verif_item(self,valeur):
          return 1

      def get_into(self,liste_courante=None,into_courant=None):
          """
          Methode get_into spécifique pour validateur OrdList, on retourne
          une liste de choix qui ne contient aucune valeur de into_courant
          dont la valeur est inférieure à la dernière valeur de
          liste_courante, si elle est différente de None.
          """
          if into_courant is None:
             return None
          elif not liste_courante :
             return into_courant
          else:
             liste_choix=[]
             last_val=liste_choix[-1]
             for e in into_courant:
                 if self.ord=='croissant' and e <= last_val:continue
                 if self.ord=='decroissant' and e >= last_val:continue
                 liste_choix.append(e)
             return liste_choix

CoercableFuncs = { types.IntType:     int,
                   types.LongType:    long,
                   types.FloatType:   float,
                   types.ComplexType: complex,
                   types.UnicodeType: unicode }

class TypeVal(ListVal):
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

      def verif_item(self,valeur):
          try:
             self.coerce(valeur)
          except:
             return 0
          return 1

class InstanceVal(ListVal):
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

      def verif_item(self,valeur):
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
          Elle verifie qu'au moins un des validateurs de la liste valide la valeur
      """
      def __init__(self,validators=()):
          if type(validators) not in (types.ListType,types.TupleType): 
             validators=(validators,)
          self.validators=[]
          for validator in validators:
              if type(validator) == types.FunctionType:
                 self.validators.append(FunctionVal(validator))
              else:
                 self.validators.append(validator)
          self.cata_info=""

      def info(self):
          return "\n ou ".join([v.info() for v in self.validators])

      def info_erreur_item(self):
          l=[]
          for v in self.validators:
              err=v.info_erreur_item()
              if err != " " : l.append(err)
          chaine=" \n ou ".join(l)
          return chaine

      def info_erreur_liste(self):
          l=[]
          for v in self.validators:
              err=v.info_erreur_liste()
              if err != " " : l.append(err)
          chaine=" \n ou ".join(l)
          return chaine

      def is_list(self):
          """
             Si plusieurs validateurs sont reliés par un OU
             il suffit qu'un seul des validateurs attende une liste
             pour qu'on considère que leur union attend une liste.
          """
          for validator in self.validators:
              v=validator.is_list()
              if v :
                 return 1
          return 0

      def verif(self,valeur):
          for validator in self.validators:
              v=validator.verif(valeur)
              if v :
                 return 1
          return 0

      def verif_item(self,valeur):
          for validator in self.validators:
              v=validator.verif_item(valeur)
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

      def has_into(self):
          """
          Dans le cas ou plusieurs validateurs sont reliés par un OU
          il faut que tous les validateurs proposent un choix pour
          qu'on considère que leur union propose un choix.
          Exemple : Enum(1,2,3) OU entier pair, ne propose pas de choix
          En revanche, Enum(1,2,3) OU Enum(4,5,6) propose un choix (1,2,3,4,5,6)
          """
          for validator in self.validators:
              v=validator.has_into()
              if not v :
                 return 0
          return 1

      def get_into(self,liste_courante=None,into_courant=None):
          """
          Dans le cas ou plusieurs validateurs sont reliés par un OU
          tous les validateurs doivent proposer un choix pour
          qu'on considère que leur union propose un choix. Tous les choix
          proposés par les validateurs sont réunis (opérateur d'union).
          Exemple : Enum(1,2,3) OU entier pair, ne propose pas de choix
          En revanche, Enum(1,2,3) OU Enum(4,5,6) propose un 
          choix (1,2,3,4,5,6)       
          """
          validator_into=[]
          for validator in self.validators:
              v_into=validator.get_into(liste_courante,into_courant)
              if v_into is None:
                 return v_into
              validator_into.extend(v_into)
          return validator_into

      def valide_liste_partielle(self,liste_courante=None):
          """
           Méthode de validation de liste partielle pour le validateur Or.
           Si un des validateurs gérés par le validateur Or considère la
           liste comme valide, le validateur Or la considère comme valide.
          """
          for validator in self.validators:
              v=validator.valide_liste_partielle(liste_courante)
              if v :
                 return 1
          return 0

class AndVal(Valid):
      """
          Cette classe est un validateur qui controle une liste de validateurs
          Elle verifie que tous les validateurs de la liste valident la valeur
      """
      def __init__(self,validators=()):
          if type(validators) not in (types.ListType,types.TupleType): 
             validators=(validators,)
          self.validators=[]
          for validator in validators:
              if type(validator) == types.FunctionType:
                 self.validators.append(FunctionVal(validator))
              else:
                 self.validators.append(validator)
          self.cata_info=""

      def info(self):
          return "\n et ".join([v.info() for v in self.validators])

      def info_erreur_item(self):
          chaine=""
          a=1
          for v in self.validators:
              if v.info_erreur_item() != " " :
                 if a==1:
                    chaine=v.info_erreur_item()
                    a=0
                 else:
                    chaine=chaine+" \n et "+v.info_erreur_item()
          return chaine

      def info_erreur_liste(self):
          a=1
          for v in self.validators:
              if v.info_erreur_liste() != " " :
                 if a==1:
                    chaine=v.info_erreur_liste()
                    a=0
                 else:
                    chaine=chaine+" \n et "+v.info_erreur_liste()
          return chaine

      def verif(self,valeur):
          for validator in self.validators:
              v=validator.verif(valeur)
              if not v :
                 self.local_info=validator.info()
                 return 0
          return 1

      def verif_item(self,valeur):
          for validator in self.validators:
              v=validator.verif_item(valeur)
              if not v :
                 # L'info n'est probablement pas la meme que pour verif ???
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

      def valide_liste_partielle(self,liste_courante=None):
          """
           Méthode de validation de liste partielle pour le validateur And.
           Tous les validateurs gérés par le validateur And doivent considérer
           la liste comme valide, pour que le validateur And la considère
           comme valide.
          """
          for validator in self.validators:
              v=validator.valide_liste_partielle(liste_courante)
              if not v :
                 return 0
          return 1

      def is_list(self):
          """
          Si plusieurs validateurs sont reliés par un ET
          il faut que tous les validateurs attendent une liste
          pour qu'on considère que leur intersection attende une liste.
          Exemple Range(2,5) ET Card(1) n'attend pas une liste
          Range(2,5) ET Pair attend une liste
          """
          for validator in self.validators:
              v=validator.is_list()
              if v == 0 :
                 return 0
          return 1

      def has_into(self):
          """
          Dans le cas ou plusieurs validateurs sont reliés par un ET
          il suffit qu'un seul validateur propose un choix pour
          qu'on considère que leur intersection propose un choix.
          Exemple : Enum(1,2,3) ET entier pair, propose un choix
          En revanche, entier pair ET superieur à 10 ne propose pas de choix
          """
          for validator in self.validators:
              v=validator.has_into()
              if v :
                 return 1
          return 0

      def get_into(self,liste_courante=None,into_courant=None):
          """
          Dans le cas ou plusieurs validateurs sont reliés par un ET
          il suffit qu'un seul validateur propose un choix pour
          qu'on considère que leur intersection propose un choix. Tous les
          choix proposés par les validateurs sont croisés (opérateur
          d'intersection)
          Exemple : Enum(1,2,3) ET entier pair, propose un choix (2,)
          En revanche, Enum(1,2,3) ET Enum(4,5,6) ne propose pas de choix.
          """
          for validator in self.validators:
              into_courant=validator.get_into(liste_courante,into_courant)
              if into_courant in ([],None):break
          return into_courant

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
