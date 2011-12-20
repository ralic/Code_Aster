#@ MODIF N_VALIDATOR Noyau  DATE 20/12/2011   AUTEUR COURTOIS M.COURTOIS 
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
# ======================================================================

"""
   Ce module contient toutes les classes necessaires pour
   implanter le concept de validateur dans Accas
"""
import types
import traceback
from N_ASSD import ASSD
from N_types import is_int, is_float_or_int, is_complex, is_number, is_str, is_enum


class ValError(Exception):pass

def cls_mro(cls):
    if hasattr(cls,"__mro__"):return cls.__mro__
    mro=[cls]
    for base in cls.__bases__:
        mro.extend(cls_mro(base))
    return mro

class Protocol:
    def __init__(self,name):
        self.registry = {}
        self.name = name
        self.args={}

    def register(self, T, A):
        self.registry[T] = A

    def adapt(self, obj):
        # (a) verifier si l'objet peut s'adapter au protocole
        adapt = getattr(obj, '__adapt__', None)
        if adapt is not None:
            # on demande à l'objet obj de réaliser lui-meme l'adaptation
            return adapt(self)

        # (b) verifier si un adapteur est enregistré (si oui l'utiliser)
        if self.registry:
            for T in cls_mro(obj.__class__):
                if T in self.registry:
                    return self.registry[T](obj,self,**self.args)

        # (c) utiliser l'adapteur par defaut
        return self.default(obj,**self.args)

    def default(self,obj,**args):
        raise TypeError("Can't adapt %s to %s" %
                        (obj.__class__.__name__, self.name))

class PProtocol(Protocol):
    """Verificateur de protocole paramétré (classe de base)"""
    #Protocole paramétré. Le registre est unique pour toutes les instances. La methode register est une methode de classe
    registry={}
    def __init__(self,name,**args):
        self.name = name
        self.args=args
    def register(cls, T, A):
        cls.registry[T] = A
    register=classmethod(register)

class ListProtocol(Protocol):
    """Verificateur de protocole liste : convertit un objet quelconque en liste pour validation ultérieure"""
    def default(self,obj):
        if type(obj) is tuple:
            if len(obj) > 0 and obj[0] in ('RI','MP'):
                #il s'agit d'un complexe ancienne mode. La cardinalite vaut 1
                return (obj,)
            else:
                return obj
        elif type(obj) is list:
            return obj
        elif obj == None:
            # pas de valeur affecte. La cardinalite vaut 0
            return obj
        elif is_str(obj):
            #il s'agit d'une chaine. La cardinalite vaut 1
            return (obj,)
        else:
            try:
                # si l'objet supporte len, on a la cardinalite
                length=len(obj)
                return obj
            except:
                # sinon elle vaut 1
                return (obj,)

listProto=ListProtocol("list")


class TypeProtocol(PProtocol):
    """Verificateur de type parmi une liste de types possibles"""
    #pas de registre par instance. Registre unique pour toutes les instances de TypeProtocol
    registry={}
    def __init__(self,name,typ=None):
        PProtocol.__init__(self,name,typ=typ)
        self.typ=typ

    def default(self,obj,typ):

        help = ""
        for type_permis in typ:
            if type_permis == 'R':
                if is_float_or_int(obj): return obj
            elif type_permis == 'I':
                if is_int(obj): return obj
            elif type_permis == 'C':
                if self.is_complexe(obj): return obj
            elif type_permis == 'TXM':
                if is_str(obj): return obj
            elif type_permis == 'shell':
                if is_str(obj): return obj
            elif type_permis == 'Fichier' :
                 import os
                 if (len(typ) > 2 and typ[2] == "Sauvegarde") or os.path.isfile(obj):
                     return obj
                 else : raise ValError("%s n'est pas un fichier valide" % repr(obj))
            elif type_permis == 'FichierNoAbs' :
                 import os
                 if (len(typ) > 2 and typ[2] == "Sauvegarde") or isinstance(obj, type("")):
                     return obj
                 else : raise ValError("%s n'est pas un fichier valide" % repr(obj))
            elif type(type_permis) == types.ClassType or isinstance(type_permis,type):
                try:
                    if self.is_object_from(obj,type_permis): return obj
                except Exception, err:
                    help = str(err)
            elif type(type_permis) == types.InstanceType or isinstance(type_permis,object):
                try:
                    if type_permis.__convert__(obj): return obj
                except Exception, err:
                    help = str(err)
            else:
                print "Type non encore géré %s" %`type_permis`
        raise ValError("%s (de type %s) n'est pas d'un type autorisé: %s %s" % (repr(obj),type(obj),typ, help))

    def is_complexe(self,valeur):
        """ Retourne 1 si valeur est un complexe, 0 sinon """
        if is_number(valeur):
            # Pour permettre l'utilisation de complexes Python (accepte les entiers et réels)
            return 1
        elif type(valeur) != tuple :
            # On n'autorise pas les listes pour les complexes
            return 0
        elif len(valeur) != 3:
            return 0
        else:
            # Un complexe doit etre un tuple de longueur 3 avec 'RI' ou 'MP' comme premiere
            # valeur suivie de 2 reels.
            if valeur[0].strip() in ('RI','MP'):
                try:
                    v1=reelProto.adapt(valeur[1]),reelProto.adapt(valeur[2])
                    return 1
                except:
                    return 0
            else:
                return 0

    def is_object_from(self,objet,classe):
        """
           Retourne 1 si objet est une instance de la classe classe, 0 sinon
        """
        convert = getattr(classe, '__convert__', None)
        if convert is not None:
            # classe verifie les valeurs
            try:
                v=  convert(objet)
                return v is not None
            except ValueError, err:
                raise
            except:
                return 0
        # On accepte les instances de la classe et des classes derivees
        return isinstance(objet,classe)

reelProto=TypeProtocol("reel",typ=('R',))

class CardProtocol(PProtocol):
    """Verificateur de cardinalité """
    #pas de registre par instance. Registre unique pour toutes les instances
    registry={}
    def __init__(self,name,min=1,max=1):
        PProtocol.__init__(self,name,min=min,max=max)

    def default(self,obj,min,max):
        length=len(obj)
        if length < min or length >max:
            raise ValError("Nombre d'arguments de %s incorrect (min = %s, max = %s)" % (repr(obj),min,max) )
        return obj

class IntoProtocol(PProtocol):
    """Verificateur de choix possibles : liste discrète ou intervalle"""
    #pas de registre par instance. Registre unique pour toutes les instances
    registry={}
    def __init__(self,name,into=None,val_min='**',val_max='**'):
        PProtocol.__init__(self,name,into=into,val_min=val_min,val_max=val_max)
        self.val_min=val_min
        self.val_max=val_max

    def default(self,obj,into,val_min,val_max):
        if into:
            if obj not in into:
                raise ValError("La valeur : %s  ne fait pas partie des choix possibles %s" % (repr(obj),into) )
        else:
            #on est dans le cas d'un ensemble continu de valeurs possibles (intervalle)
            if is_float_or_int(obj):
                if val_min == '**': val_min = obj -1
                if val_max == '**': val_max = obj +1
                if obj < val_min or obj > val_max :
                    raise ValError("La valeur : %s est en dehors du domaine de validité [ %s , %s ]" % (repr(obj),self.val_min,self.val_max) )
        return obj

class MinStr:
    #exemple de classe pour verificateur de type
    #on utilise des instances de classe comme type (typ=MinStr(3,6), par exemple)
    def __init__(self,min,max):
        self.min=min
        self.max=max

    def __convert__(self,valeur):
        if is_str(valeur) and self.min <= len(valeur) <= self.max:return valeur
        raise ValError("%s n'est pas une chaine de longueur comprise entre %s et %s" % (valeur,self.min,self.max))

    def __repr__(self):
        return "TXM de longueur entre %s et %s" %(self.min,self.max)

class Valid(PProtocol):
   """
        Cette classe est la classe mere des validateurs Accas
        Elle doit etre derivee
        Elle presente la signature des methodes indispensables pour son bon
        fonctionnement et dans certains cas leur comportement par défaut.

        @ivar cata_info: raison de la validite ou de l'invalidite du validateur meme
        @type cata_info: C{string}
   """
   registry={}
   def __init__(self,**args):
       PProtocol.__init__(self,"valid",**args)

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

   def convert(self,valeur):
       """
          Méthode convert pour les validateurs de listes. Cette méthode
          fait appel à la méthode convert_item sur chaque élément de la
          liste.
       """
       if is_enum(valeur):
          for val in valeur:
              self.convert_item(val)
          return valeur
       else:
          return self.convert_item(valeur)

   def verif(self,valeur):
       """
          Méthode verif pour les validateurs de listes. Cette méthode
          fait appel à la méthode verif_item sur chaque élément de la
          liste. Si valeur est un paramètre, on utilise sa valeur effective
          valeur.valeur.
       """
       if is_enum(valeur):
          for val in valeur:
              if not self.verif_item(val):
                 return 0
          return 1
       else:
          return self.verif_item(valeur)

class Compulsory(ListVal):
      """
          Validateur operationnel
          Verification de la présence obligatoire d'un élément dans une liste
      """
      registry={}
      def __init__(self,elem=()):
          if not is_enum(elem): elem=(elem,)
          Valid.__init__(self,elem=elem)
          self.elem=elem
          self.cata_info=""

      def info(self):
          return "valeur %s obligatoire" % `self.elem`

      def default(self,valeur,elem):
          return valeur

      def verif_item(self,valeur):
          return 1

      def convert(self,valeur):
          elem=list(self.elem)
          for val in valeur:
              v=self.adapt(val)
              if v in elem:elem.remove(v)
          if elem:
              raise ValError("%s ne contient pas les elements obligatoires : %s " %(valeur,elem))
          return valeur

      def has_into(self):
          return 1

      def verif(self,valeur):
          if not is_enum(valeur):
             liste=list(valeur)
          else:
             liste=valeur
          for val in self.elem :
             if val not in liste : return 0
          return 1

      def info_erreur_item(self):
          return "La valeur n'est pas dans la liste des choix possibles"

class NoRepeat(ListVal):
      """
          Validateur operationnel
          Verification d'absence de doublons dans la liste.
      """
      def __init__(self):
          Valid.__init__(self)
          self.cata_info=""

      def info(self):
          return ": pas de présence de doublon dans la liste"

      def info_erreur_liste(self):
          return "Les doublons ne sont pas permis"

      def default(self,valeur):
          if valeur in self.liste : raise ValError("%s est un doublon" % valeur)
          return valeur

      def convert(self,valeur):
          self.liste=[]
          for val in valeur:
              v=self.adapt(val)
              self.liste.append(v)
          return valeur

      def verif_item(self,valeur):
          return 1

      def verif(self,valeur):
          if is_enum(valeur):
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
          Validateur operationnel
          Verification de la longueur d une chaine
      """
      def __init__(self,low,high):
          ListVal.__init__(self,low=low,high=high)
          self.low=low
          self.high=high
          self.cata_info=""

      def info(self):
          return "longueur de la chaine entre %s et %s" %(self.low,self.high)

      def info_erreur_item(self):
          return "Longueur de la chaine incorrecte"

      def convert(self,valeur):
          for val in valeur:
              v=self.adapt(val)
          return valeur

      def verif_item(self,valeur):
          try:
             self.adapt(valeur)
             return 1
          except:
             return 0

      def default(self,valeur,low,high):
          if not is_str(valeur):
             raise ValError("%s n'est pas une string" % repr(valeur))
          if valeur[0]=="'" and valeur[-1]=="'" :
             low=low+2
             high=high+2
          if len(valeur) < low or len(valeur) > high :
             raise ValError("%s n'est pas de la bonne longueur" % repr(valeur))
          return valeur

class OnlyStr(ListVal):
      """
          Validateur operationnel
          Valide que c'est une chaine
      """
      def __init__(self):
          ListVal.__init__(self)
          self.cata_info=""

      def info(self):
          return "regarde si c'est une chaine"

      def info_erreur_item(self):
          return "Ce n'est pas une chain"

      def convert(self,valeur):
          for val in valeur:
              v=self.adapt(val)
          return valeur

      def verif_item(self,valeur):
          try:
             self.adapt(valeur)
             return 1
          except:
             return 0

      def default(self,valeur):
          if not is_str(valeur):
             raise ValError("%s n'est pas une string" % repr(valeur))
          return valeur

class OrdList(ListVal):
      """
          Validateur operationnel
          Verification qu'une liste est croissante ou decroissante
      """
      def __init__(self,ord):
          ListVal.__init__(self,ord=ord)
          self.ord=ord
          self.cata_info=""

      def info(self):
          return "liste %s" % self.ord

      def info_erreur_liste(self) :
          return "La liste doit etre en ordre "+self.ord

      def convert(self,valeur):
          self.val=None
          self.liste=valeur
          for v in valeur:
              self.adapt(v)
          return valeur

      def default(self,valeur,ord):
          if self.ord=='croissant':
              if self.val is not None and valeur<self.val:
                  raise ValError("%s n'est pas par valeurs croissantes" % repr(self.liste))
          elif self.ord=='decroissant':
              if self.val is not None and valeur > self.val:
                  raise ValError("%s n'est pas par valeurs decroissantes" % repr(self.liste))
          self.val=valeur
          return valeur

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

class OrVal(Valid):
      """
          Validateur operationnel
          Cette classe est un validateur qui controle une liste de validateurs
          Elle verifie qu'au moins un des validateurs de la liste valide la valeur
      """
      def __init__(self,validators=()):
          if not is_enum(validators):
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

      def convert(self,valeur):
          for validator in self.validators:
              try:
                 return validator.convert(valeur)
              except:
                 pass
          raise ValError("%s n'est pas du bon type" % repr(valeur))

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
          Validateur operationnel
          Cette classe est un validateur qui controle une liste de validateurs
          Elle verifie que tous les validateurs de la liste valident la valeur
      """
      def __init__(self,validators=()):
          if not is_enum(validators):
             validators=(validators,)
          self.validators=[]
          for validator in validators:
              if type(validator) == types.FunctionType:
                 self.validators.append(FunctionVal(validator))
              else:
                 self.validators.append(validator)
              if hasattr(validator,'fonctions'):
                 for fonction in validator.fonctions :
                    f=getattr(validator,fonction)
                    setattr(self,fonction,f)
          self.cata_info=""

      def info(self):
          return "\n et ".join([v.info() for v in self.validators])

      def convert(self,valeur):
          for validator in self.validators:
              valeur=validator.convert(valeur)
          return valeur

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
        elif type(validator) is tuple:
           valids.append(OrVal(do_liste(validator)))
        elif type(validator) is list:
           valids.append(AndVal(do_liste(validator)))
        else:
           valids.append(validator)
    return valids

def validatorFactory(validator):
    if type(validator) == types.FunctionType:
       return FunctionVal(validator)
    elif type(validator) is tuple:
       return OrVal(do_liste(validator))
    elif type(validator) is list:
       return AndVal(do_liste(validator))
    else:
       return validator

# Ci-dessous : exemples de validateur (peu testés)

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

      def convert_item(self,valeur):
          if valeur > self.low and valeur < self.high:return valeur
          raise ValError("%s devrait etre comprise entre %s et %s" %(valeur,self.low,self.high))

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

      def convert(self,valeur):
          if is_enum(valeur):
             l=len(valeur)
          elif valeur is None:
             l=0
          else:
             l=1
          if self.max != '**' and l > self.max:raise ValError("%s devrait etre de longueur inferieure a %s" %(valeur,self.max))
          if self.min != '**' and l < self.min:raise ValError("%s devrait etre de longueur superieure a %s" %(valeur,self.min))
          return valeur

      def verif_item(self,valeur):
          return 1

      def verif(self,valeur):
          if is_enum(valeur):
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
          ListVal.__init__(self)
          self.cata_info=""

      def info(self):
          return "valeur paire"

      def info_erreur_item(self):
          return "La valeur saisie doit etre paire"

      def convert(self,valeur):
          for val in valeur:
             v=self.adapt(val)
             if v % 2 != 0:raise ValError("%s contient des valeurs non paires" % repr(valeur))
          return valeur

      def default(self,valeur):
          return valeur

      def verif_item(self,valeur):
          if type(valeur) not in (int,long):
             return 0
          return valeur % 2 == 0

      def verif(self,valeur):
          if is_enum(valeur):
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
          if not is_enum(into): 
              into=(into,)
          self.into=into
          self.cata_info=""

      def info(self):
          return "valeur dans %s" % `self.into`

      def convert_item(self,valeur):
          if valeur in self.into:return valeur
          raise ValError("%s contient des valeurs hors des choix possibles: %s " %(valeur,self.into))

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

def ImpairVal(valeur):
    """
          Exemple de validateur
        Cette fonction est un validateur. Elle verifie que la valeur passee
        est bien un nombre impair.
    """
    if is_enum(valeur):
       for val in valeur:
           if val % 2 != 1:return 0
       return 1
    else:
       if valeur % 2 != 1:return 0
       return 1

ImpairVal.info="valeur impaire"

class F1Val(Valid):
      """
          Exemple de validateur
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
          if is_enum(valeur):
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
          Exemple de validateur
          Cette classe est un validateur qui est initialise avec une fonction
      """
      def __init__(self,function):
          self.function=function

      def info(self):
          return self.function.info

      def verif(self,valeur):
          return self.function(valeur)

#MC ca ne devrait plus servir !
CoercableFuncs = { types.IntType:     int,
                   types.LongType:    long,
                   types.FloatType:   float,
                   types.ComplexType: complex,
                   types.UnicodeType: unicode }

class TypeVal(ListVal):
      """
          Exemple de validateur
          Cette classe est un validateur qui controle qu'une valeur
          est bien du type Python attendu.
          Pour une liste on verifie que tous les elements sont du bon type.
          Semblable a InstanceVal mais ici on fait le test par tentative de conversion
          alors qu'avec InstanceVal on ne teste que si isinstance est vrai.
      """
      def __init__(self, aType):
          #Si aType n'est pas un type, on le retrouve a l'aide de la fonction type
          #type(1) == int;type(0.2)==float;etc.
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

      def convert_item(self,valeur):
          return   self.coerce(valeur)

      def verif_item(self,valeur):
          try:
             self.coerce(valeur)
          except:
             return 0
          return 1

class InstanceVal(ListVal):
      """
          Exemple de validateur
          Cette classe est un validateur qui controle qu'une valeur est
          bien une instance (au sens Python) d'une classe
          Pour une liste on verifie chaque element de la liste
      """
      def __init__(self,aClass):
          #Si aClass est une classe on la memorise dans self.aClass
          #sinon c'est une instance dont on memorise la classe
          if type(aClass) == types.InstanceType:
             #instance ancienne mode
             aClass=aClass.__class__
          elif type(aClass) == types.ClassType:
             #classe ancienne mode
             aClass=aClass
          elif type(aClass) == type:
             #classe nouvelle mode
             aClass=aClass
          elif isinstance(aClass,object):
             #instance nouvelle mode
             aClass=type(aClass)
          else:
             raise ValError("type non supporte")

          self.aClass=aClass

      def info(self):
          return "valeur d'instance de %s" % self.aClass.__name__

      def verif_item(self,valeur):
          if not isinstance(valeur,self.aClass): return 0
          return 1

class VerifTypeTuple(Valid,ListVal) :
      def __init__(self,typeDesTuples):
          self.typeDesTuples=typeDesTuples
          Valid.__init__(self)
          self.cata_info=""

      def info(self):
          return ": verifie les types dans un tuple"

      def info_erreur_liste(self):
          return "Les types entres  ne sont pas permis"

      def default(self,valeur):
          #if valeur in self.liste : raise ValError("%s est un doublon" % valeur)
          return valeur

      def is_list(self) :
          return 1

      def convert_item(self,valeur):
          if len(valeur) != len(self.typeDesTuples):
             raise ValError("%s devrait etre de type  %s " %(valeur,self.typeDesTuples))
          for i in range(len(valeur)) :
              ok=self.verifType(valeur[i],self.typeDesTuples[i])
              if ok!=1 : 
                 raise ValError("%s devrait etre de type  %s " %(valeur,self.typeDesTuples))
          return valeur

      def verif_item(self,valeur):
          try :
             if len(valeur) != len(self.typeDesTuples):
                return 0
             for i in range(len(valeur)) :
                ok=self.verifType(valeur[i],self.typeDesTuples[i])
                if ok!=1:
                   return 0
          except :
             return 0
          return 1

      def verifType(self,valeur,type_permis):
          if type_permis == 'R':
             if type(valeur) in (types.IntType,types.FloatType,types.LongType):return 1
          elif type_permis == 'I':
             if type(valeur) in (types.IntType,types.LongType):return 1
          elif type_permis == 'C':
             if self.is_complexe(valeur):return 1
          elif type_permis == 'TXM':
             if type(valeur)==types.StringType:return 1
          return 0

      def verif(self,valeur):
          if type(valeur) in (types.ListType,types.TupleType):
             liste=list(valeur)
             for val in liste:
                if self.verif_item(val)!=1 : return 0
             return 1
 
class VerifExiste(ListVal) :
      """
         fonctionne avec into
         Met une liste à jour selon les mot clefs existant
         exemple si into = ("A","B","C")
         si au niveau N du JDC les objets "A" et "C" existe
         alors la liste des into deviendra ( "A","C")

         niveauVerif est le niveau du JDC dans lequel va s effectuer la verification
         niveauVerif est defini par rapport au Noeud :
         exemple niveauVerif = 1 : on verifie les freres
                 niveauVerif = 2 : on verifie les oncles..
      """
      def __init__(self,niveauVerif):
          ListVal.__init__(self)
          self.niveauVerif=niveauVerif
          self.MCSimp=None
          self.listeDesFreres=()
          self.fonctions=('verifie_liste','set_MCSimp')

      def is_list(self):
          return 1

      def verifie_liste(self,liste):
          self.set_MCSimp(self.MCSimp)
          for item in liste :
            if not( item in self.listeDesFreres) : return 0
          return 1

      def verif_item(self,valeur):
          self.set_MCSimp(self.MCSimp)
          if valeur in self.listeDesFreres : return 1
          return 0

      def set_MCSimp(self, MCSimp) :
          self.MCSimp=MCSimp
          k=self.niveauVerif
          mc=MCSimp
          while (k != 0) :
             parent=mc.parent
             mc=parent
             k=k-1
         #on met la liste à jour
          parent.forceRecalcul=self.niveauVerif
          self.listeDesFreres=parent.liste_mc_presents()

      def convert_item(self,valeur):
          if valeur in self.listeDesFreres : return valeur
          raise ValError(str(valeur)+" n est pas dans " + str(self.listeDesFreres))

