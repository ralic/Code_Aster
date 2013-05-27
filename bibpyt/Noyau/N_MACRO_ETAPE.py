# -*- coding: iso-8859-1 -*-
# person_in_charge: mathieu.courtois at edf.fr
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    Ce module contient la classe MACRO_ETAPE qui sert à vérifier et à exécuter
    une commande
"""

# Modules Python
import types,sys,string
import traceback
from warnings import warn

# Modules EFICAS
import N_MCCOMPO
import N_ETAPE
from N_Exception import AsException
import N_utils
from N_utils import AsType
from N_CO import CO
from N_ASSD import ASSD
from N_info import message, SUPERV

class MACRO_ETAPE(N_ETAPE.ETAPE):
   """

   """
   nature = "COMMANDE"
   typeCO=CO
   def __init__(self,oper=None,reuse=None,args={}):
      """
      Attributs :
         - definition : objet portant les attributs de définition d'une étape
           de type macro-commande. Il est initialisé par
           l'argument oper.
         - reuse : indique le concept d'entrée réutilisé. Il se trouvera donc
           en sortie si les conditions d'exécution de l'opérateur
           l'autorise
         - valeur : arguments d'entrée de type mot-clé=valeur. Initialisé
           avec l'argument args.
      """
      N_ETAPE.ETAPE.__init__(self, oper, reuse, args, niveau=5)
      self.g_context = {}
      # Contexte courant
      self.current_context = {}
      self.macro_const_context = {}
      self.index_etape_courante = 0
      self.etapes = []
      self.index_etapes = {}
      #  Dans le cas d'une macro écrite en Python, l'attribut Outputs est un
      #  dictionnaire qui contient les concepts produits de sortie
      #  (nom : ASSD) déclarés dans la fonction sd_prod
      self.Outputs = {}
      self.sdprods = []
      self.UserError = "UserError"
      # permet de stocker le nom du dernier concept nommé dans la macro
      self.last = None

   def make_register(self):
      """
      Initialise les attributs jdc, id, niveau et réalise les enregistrements
      nécessaires
      """
      N_ETAPE.ETAPE.make_register(self)
      if self.parent :
         self.UserError=self.jdc.UserError
      else:
         self.UserError="UserError"

   def Build_sd(self,nom):
      """
         Construit le concept produit de l'opérateur. Deux cas
         peuvent se présenter :

           - le parent n'est pas défini. Dans ce cas, l'étape prend en charge
             la création et le nommage du concept.

           - le parent est défini. Dans ce cas, l'étape demande au parent la
             création et le nommage du concept.

      """
      #message.debug(SUPERV, "%s", self.nom)
      self.sdnom=nom
      try:
         # On positionne la macro self en tant que current_step pour que les
         # étapes créées lors de l'appel à sd_prod et à op_init aient la macro
         #  comme parent
         self.set_current_step()
         if self.parent:
            sd= self.parent.create_sdprod(self,nom)
            if type(self.definition.op_init) == types.FunctionType:
               apply(self.definition.op_init,(self,self.parent.g_context))
         else:
            sd=self.get_sd_prod()
            if sd != None and self.reuse == None:
               # On ne nomme le concept que dans le cas de non reutilisation
               # d un concept
               sd.set_name(nom)
         self.reset_current_step()
      except AsException,e:
         self.reset_current_step()
         raise AsException("Etape ",self.nom,'ligne : ',self.appel[0],
                              'fichier : ',self.appel[1],e)
      except (EOFError, self.UserError):
         # Le retablissement du step courant n'est pas strictement necessaire. On le fait pour des raisons de coherence
         self.reset_current_step()
         raise
      except :
         self.reset_current_step()
         l=traceback.format_exception(sys.exc_info()[0],sys.exc_info()[1],sys.exc_info()[2])
         raise AsException("Etape ",self.nom,'ligne : ',self.appel[0],
                           'fichier : ',self.appel[1]+'\n',
                            string.join(l))

      self.Execute()
      return sd

   def get_sd_prod(self):
      """
        Retourne le concept résultat d'une macro étape
        La difference avec une etape ou une proc-etape tient a ce que
        le concept produit peut exister ou pas

        Si sd_prod == None le concept produit n existe pas on retourne None

        Deux cas :
         - cas 1 : sd_prod  n'est pas une fonction
                 il s'agit d'une sous classe de ASSD
                 on construit le sd à partir de cette classe
                 et on le retourne
         - cas 2 : sd_prod est une fonction
                 on l'évalue avec les mots-clés de l'étape (mc_liste)
                 on construit le sd à partir de la classe obtenue
                 et on le retourne
      """
      sd_prod=self.definition.sd_prod
      self.typret=None

      if type(self.definition.sd_prod) == types.FunctionType:
        d=self.cree_dict_valeurs(self.mc_liste)
        try:
          # la sd_prod d'une macro a l'objet macro_etape lui meme en premier argument
          # Comme sd_prod peut invoquer la méthode type_sdprod qui ajoute
          # les concepts produits dans self.sdprods, il faut le mettre à zéro avant de l'appeler
          self.sdprods=[]
          sd_prod= apply(sd_prod,(self,),d)
        except (EOFError,self.UserError):
          raise
        except:
          if CONTEXT.debug: traceback.print_exc()
          l=traceback.format_exception(sys.exc_info()[0],sys.exc_info()[1],sys.exc_info()[2])
          raise AsException("impossible d affecter un type au resultat\n",string.join(l[2:]))

      # on teste maintenant si la SD est réutilisée ou s'il faut la créer
      if self.definition.reentrant != 'n' and self.reuse:
        # Le concept produit est specifie reutilise (reuse=xxx). C'est une erreur mais non fatale.
        # Elle sera traitee ulterieurement.
        self.sd=self.reuse
      else:
        if sd_prod == None:
          self.sd=None
        else:
          self.sd= sd_prod(etape=self)
          self.typret=sd_prod
          # Si la commande est obligatoirement reentrante et reuse n'a pas ete specifie, c'est une erreur.
          # On ne fait rien ici. L'erreur sera traitee par la suite.
      # précaution
      if self.sd is not None and not isinstance(self.sd, ASSD):
         raise AsException("""
Impossible de typer le résultat !
Causes possibles :
   Utilisateur : Soit la valeur fournie derrière "reuse" est incorrecte,
                 soit il y a une "," à la fin d'une commande précédente.
   Développeur : La fonction "sd_prod" retourne un type invalide.""")
      return self.sd

   def get_type_produit(self,force=0):
      try:
          return self.get_type_produit_brut(force)
      except:
          #traceback.print_exc()
          return None

   def get_type_produit_brut(self,force=0):
      """
           Retourne le type du concept résultat de l'étape et eventuellement type
           les concepts produits "à droite" du signe égal (en entrée)

           Deux cas :
             - cas 1 : sd_prod de oper n'est pas une fonction
                    il s'agit d'une sous classe de ASSD
                    on retourne le nom de la classe
             - cas 2 : il s'agit d'une fonction
                    on l'évalue avec les mots-clés de l'étape (mc_liste)
                    et on retourne son résultat
      """
      if not force and hasattr(self,'typret'):
          return self.typret

      if type(self.definition.sd_prod) == types.FunctionType:
        d=self.cree_dict_valeurs(self.mc_liste)
        # Comme sd_prod peut invoquer la méthode type_sdprod qui ajoute
        # les concepts produits dans self.sdprods, il faut le mettre à zéro
        self.sdprods=[]
        sd_prod= apply(self.definition.sd_prod,(self,),d)
      else:
        sd_prod=self.definition.sd_prod
      return sd_prod

   def get_contexte_avant(self,etape):
      """
          Retourne le dictionnaire des concepts connus avant etape
          pour les commandes internes a la macro
          On tient compte des commandes qui modifient le contexte
          comme DETRUIRE ou les macros
      """
      # L'étape courante pour laquelle le contexte a été calculé est
      # mémorisée dans self.index_etape_courante
      #message.debug(SUPERV, "g_context : %s", [k for k, v in self.g_context.items() if isinstance(v, ASSD)])
      #message.debug(SUPERV, "current_context : %s", [k for k, v in self.current_context.items() if isinstance(v, ASSD)])
      d = self.current_context = self.g_context.copy()
      if etape is None:
          return d
      # retirer les sd produites par 'etape'
      sd_names = [sd.nom for sd in etape.get_created_sd()]
      #message.debug(SUPERV, "etape: %s, reuse : %s, sdprods de %s : %s",
                    #self.nom, etape.reuse, etape.nom, sd_names)
      for nom in sd_names:
         try:
             del d[nom]
         except KeyError:
             pass
             # Exemple avec INCLUDE_MATERIAU appelé dans une macro.
             # Les fonctions restent uniquement dans le contexte de INCLUDE_MATERIAU,
             # elles ne sont donc pas dans le contexte de la macro appelante.
             #from warnings import warn
             #warn("concept '%s' absent du contexte de %s" % (nom, self.nom),
                  #RuntimeWarning, stacklevel=2)
      return d

   def supprime(self):
      """
         Méthode qui supprime toutes les références arrières afin que
         l'objet puisse etre correctement détruit par le garbage collector
      """
      N_MCCOMPO.MCCOMPO.supprime(self)
      self.jdc=None
      self.appel=None
      if self.sd : self.sd.supprime()
      for concept in self.sdprods:
         concept.supprime()
      for etape in self.etapes:
         etape.supprime()

   def clean(self, netapes):
      """Nettoie les `netapes` dernières étapes de la liste des étapes."""
      if self.jdc.hist_etape:
          return
      for i in xrange(netapes):
        e=self.etapes.pop()
        jdc=e.jdc
        parent=e.parent
        e.supprime()
        e.parent=parent
        e.jdc=jdc
        #message.debug(SUPERV, "MACRO.clean - etape = %s - refcount(e) = %d",
                      #e.nom, sys.getrefcount(e))
        del self.index_etapes[e]

   def type_sdprod(self,co,t):
      """
           Cette methode a pour fonction de typer le concept co avec le type t
           dans les conditions suivantes :
            1. co est un concept produit de self
            2. co est un concept libre : on le type et on l attribue à self

           Elle enregistre egalement les concepts produits (on fait l hypothese
           que la liste sdprods a été correctement initialisee, vide probablement)
      """
      if not hasattr(co,'etape'):
         # Le concept vaut None probablement. On ignore l'appel
         return
      #
      # On cherche a discriminer les differents cas de typage d'un concept
      # produit par une macro qui est specifie dans un mot cle simple.
      # On peut passer plusieurs fois par type_sdprod ce qui explique
      # le nombre important de cas.
      #
      # Cas 1 : Le concept est libre. Il vient d'etre cree par CO(nom)
      # Cas 2 : Le concept est produit par la macro. On est deja passe par type_sdprod.
      #         Cas semblable a Cas 1.
      # Cas 3 : Le concept est produit par la macro englobante (parent). On transfere
      #         la propriete du concept de la macro parent a la macro courante (self)
      #         en verifiant que le type est valide
      # Cas 4 : La concept est la propriete d'une etape fille. Ceci veut dire qu'on est
      #         deja passe par type_sdprod et que la propriete a ete transfere a une
      #         etape fille. Cas semblable a Cas 3.
      # Cas 5 : Le concept est produit par une etape externe a la macro.
      #
      if co.etape == None:
         # Cas 1 : le concept est libre
         # On l'attache a la macro et on change son type dans le type demande
         # Recherche du mot cle simple associe au concept
         mcs=self.get_mcs_with_co(co)
         if len(mcs) != 1:
            raise AsException("""Erreur interne.
Il ne devrait y avoir qu'un seul mot cle porteur du concept CO (%s)""" % co)
         mcs=mcs[0]
         if not self.typeCO in mcs.definition.type:
            raise AsException("""Erreur interne.
Impossible de changer le type du concept (%s). Le mot cle associe ne supporte pas CO mais seulement (%s)""" %(co,mcs.definition.type))
         co.etape = self
         # affectation du bon type du concept
         #message.debug(SUPERV, "MACRO.type_sdprod : changement de type de %s --> %s", co, t)
         co.change_type(t)
         self.sdprods.append(co)

      elif co.etape == self:
         # Cas 2 : le concept est produit par la macro (self)
         # On est deja passe par type_sdprod (Cas 1 ou 3).
         #XXX Peut-il être créer par une autre macro ?
         #    On vérifie juste que c'est un vrai CO non déjà typé
         #if co.etape == co._etape: 
         if co.is_typco() == 1:
           #Le concept a été créé par la macro (self)
           #On peut changer son type
           co.change_type(t)
         else:
           #Le concept a été créé par une macro parente
           # Le type du concept doit etre coherent avec le type demande (seulement derive)
           if not isinstance(co,t):
             raise AsException("""Erreur interne.
Le type demande (%s) et le type du concept (%s) devraient etre derives""" %(t,co.__class__))

         self.sdprods.append(co)

      elif co.etape== self.parent:
         # Cas 3 : le concept est produit par la macro parente (self.parent)
         # on transfere la propriete du concept a la macro fille
         # et on change le type du concept comme demande
         # Au prealable, on verifie que le concept existant (co) est une instance
         # possible du type demande (t)
         # Cette règle est normalement cohérente avec les règles de vérification des mots-clés
         if not isinstance(co,t):
            raise AsException("""
Impossible de changer le type du concept produit (%s) en (%s).
Le type actuel (%s) devrait etre une classe derivee du nouveau type (%s)""" % (co,t,co.__class__,t))
         mcs=self.get_mcs_with_co(co)
         if len(mcs) != 1:
            raise AsException("""Erreur interne.
Il ne devrait y avoir qu'un seul mot cle porteur du concept CO (%s)""" % co)
         mcs=mcs[0]
         if not self.typeCO in mcs.definition.type:
            raise AsException("""Erreur interne.
Impossible de changer le type du concept (%s). Le mot cle associe ne supporte pas CO mais seulement (%s)""" %(co,mcs.definition.type))
         co.etape=self
         # On ne change pas le type car il respecte la condition isinstance(co,t)
         #co.__class__ = t
         self.sdprods.append(co)

      elif self.issubstep(co.etape):
         # Cas 4 : Le concept est propriété d'une sous etape de la macro (self).
         # On est deja passe par type_sdprod (Cas 3 ou 1).
         # Il suffit de le mettre dans la liste des concepts produits (self.sdprods)
         # Le type du concept et t doivent etre derives.
         # Il n'y a aucune raison pour que la condition ne soit pas verifiee.
         if not isinstance(co,t):
            raise AsException("""Erreur interne.
Le type demande (%s) et le type du concept (%s) devraient etre derives""" %(t,co.__class__))
         self.sdprods.append(co)

      else:
         # Cas 5 : le concept est produit par une autre étape
         # On ne fait rien
         return

   def issubstep(self,etape):
      """
          Cette methode retourne un entier indiquant si etape est une
          sous etape de la macro self ou non
          1 = oui
          0 = non
      """
      if etape in self.etapes:return 1
      for etap in self.etapes:
        if etap.issubstep(etape):return 1
      return 0

   def register(self,etape):
      """
          Enregistrement de etape dans le contexte de la macro : liste etapes
          et demande d enregistrement global aupres du JDC
      """
      self.etapes.append(etape)
      self.index_etapes[etape] = len(self.etapes) - 1
      idetape=self.jdc.g_register(etape)
      return idetape

   def reg_sd(self,sd):
      """
           Methode appelee dans l __init__ d un ASSD a sa creation pour
           s enregistrer (reserve aux ASSD créés au sein d'une MACRO)
      """
      return self.jdc.o_register(sd)

   def create_sdprod(self,etape,nomsd):
      """
          Cette methode doit fabriquer le concept produit retourne
          par l'etape etape et le nommer.

          Elle est appelée à l'initiative de l'etape
          pendant le processus de construction de cette etape : methode __call__
          de la classe CMD (OPER ou MACRO)
          Ce travail est réalisé par le contexte supérieur (etape.parent)
          car dans certains cas, le concept ne doit pas etre fabriqué mais
          l'etape doit simplement utiliser un concept préexistant.
                  - Cas 1 : etape.reuse != None : le concept est réutilisé
                  - Cas 2 : l'étape appartient à une macro qui a déclaré un concept
                    de sortie qui doit etre produit par cette etape.
      """
      if self.Outputs.has_key(nomsd):
         # Il s'agit d'un concept de sortie de la macro. Il ne faut pas le créer
         # Il faut quand meme appeler la fonction sd_prod si elle existe.
         # get_type_produit le fait et donne le type attendu par la commande pour verification ultérieure.
         sdprod=etape.get_type_produit_brut()
         sd=self.Outputs[nomsd]
         # On verifie que le type du concept existant sd.__class__ est un sur type de celui attendu
         # Cette règle est normalement cohérente avec les règles de vérification des mots-clés
         if not issubclass(sdprod,sd.__class__):
            raise AsException("Le type du concept produit %s devrait etre une sur classe de %s" %(sd.__class__,sdprod))
         # La propriete du concept est transferee a l'etape avec le type attendu par l'étape
         etape.sd=sd
         sd.etape=etape
         if self.reuse == sd and etape.reuse != sd \
                and getattr(sd, "executed", 0) == 1: # n'a pas été pas détruit
            raise AsException("Le concept '%s' est réentrant dans la macro-commande %s. " \
                              "Il devrait donc l'être dans %s (produit sous le nom '%s')." \
                                % (sd.nom, self.nom, etape.nom, nomsd))
         # On donne au concept le type produit par la sous commande.
         # Le principe est le suivant : apres avoir verifie que le type deduit par la sous commande
         # est bien coherent avec celui initialement affecte par la macro (voir ci dessus)
         # on affecte au concept ce type car il peut etre plus precis (derive, en general)
         sd.__class__=sdprod
         # On force également le nom stocké dans l'attribut sdnom : on lui donne le nom
         # du concept associé à nomsd
         etape.sdnom=sd.nom
         # pour l'ajouter au contexte de la macro
         self.g_context[sd.nom] = sd
      elif etape.definition.reentrant != 'n' and etape.reuse != None:
         # On est dans le cas d'une commande avec reutilisation d'un concept existant
         # get_sd_prod fait le necessaire : verifications, associations, etc. mais ne cree
         # pas un nouveau concept. Il retourne le concept reutilise
         sd= etape.get_sd_prod()
         # Dans le cas d'un concept nomme automatiquement : _xxx, __xxx,
         # On force le nom stocke dans l'attribut sdnom  de l'objet etape : on lui donne le nom
         # du concept  reutilise (sd ou etape.reuse c'est pareil)
         # Ceci est indispensable pour eviter des erreurs lors des verifications des macros
         # En effet une commande avec reutilisation d'un concept verifie que le nom de
         # la variable a gauche du signe = est le meme que celui du concept reutilise.
         # Lorsqu'une telle commande apparait dans une macro, on supprime cette verification.
         if (etape.sdnom == '' or etape.sdnom[0] == '_'):
            etape.sdnom=sd.nom
      else:
         # On est dans le cas de la creation d'un nouveau concept
         sd= etape.get_sd_prod()
         if sd != None :
            self.NommerSdprod(sd,nomsd)
      return sd

   def NommerSdprod(self,sd,sdnom,restrict='non'):
        """
          Cette méthode est appelée par les etapes internes de la macro.
          La macro appelle le JDC pour valider le nommage.
          On considère que l'espace de nom est unique et géré par le JDC.
          Si le nom est déjà utilisé, l'appel lève une exception.
          Si restrict=='non', on insère le concept dans le contexte du parent de la macro.
          Si restrict=='oui', on insère le concept uniquement dans le contexte de la macro.
        """
        # Normalement, lorsqu'on appelle cette methode, on ne veut nommer que des concepts nouvellement crees.
        # Le filtrage sur les concepts a creer ou a ne pas creer est fait dans la methode
        # create_sdprod. La seule chose a verifier apres conversion eventuelle du nom
        # est de verifier que le nom n'est pas deja attribue. Ceci est fait en delegant
        # au JDC par l'intermediaire du parent.
        #message.debug(SUPERV, "macro results = %s, (sdnom: %r, restrict: %r)",
                      #self.Outputs.keys(), sdnom, restrict)
        if self.Outputs.has_key(sdnom):
            # Il s'agit d'un concept de sortie de la macro produit par une sous commande
            sdnom = self.Outputs[sdnom].nom
        elif len(sdnom) > 0:
            if sdnom[0] in ('_', '.') and sdnom[1:].isdigit():
                # il est déjà de la forme _9000012 ou .9000017
                pass
            elif sdnom[0] == '_':
                # Si le nom du concept commence par le caractère '_', on lui attribue
                # un identificateur JEVEUX construit par gcncon.
                # nom commençant par __ : il s'agit de concepts qui seront détruits
                # nom commençant par _ : il s'agit de concepts intermediaires qui seront gardés
                if len(sdnom) > 1 and sdnom[1] == '_':
                    sdnom = self.gcncon('.')
                else:
                    sdnom = self.gcncon('_')
            elif self.nom in ('INCLUDE', 'MACR_RECAL'):
                # dans le cas d'INCLUDE, on passe
                # MACR_RECAL fonctionne comme INCLUDE
                pass
            else:
                # On est dans le cas d'un nom de concept global
                #XXX à voir, création de CO() dans CALC_ESSAI (sdls139a)
                if not sd.is_typco():
                    raise AsException("Résultat non déclaré par la macro %s : %s" % (self.nom, sdnom))
        self.last = sdnom
        if restrict == 'non':
            # On demande le nommage au parent mais sans ajout du concept dans le contexte du parent
            # car on va l'ajouter dans le contexte de la macro
            self.parent.NommerSdprod(sd,sdnom,restrict='oui')
            # On ajoute dans le contexte de la macro les concepts nommes
            # Ceci est indispensable pour les CO (macro) dans un INCLUDE
            self.g_context[sdnom]=sd
            #message.debug(SUPERV, "g_context[%s] = %s", sdnom, sd)
        else:
            # La demande de nommage vient probablement d'une macro qui a mis
            # le concept dans son contexte. On ne traite plus que le nommage (restrict="oui")
            #message.debug(SUPERV, "restrict=oui  co[%s] = %s", sdnom, sd)
            self.parent.NommerSdprod(sd,sdnom,restrict='oui')

   def delete_concept_after_etape(self,etape,sd):
      """
          Met à jour les étapes de la MACRO  qui sont après etape suite à
          la disparition du concept sd
      """
      # Cette methode est définie dans le noyau mais ne sert que pendant la phase de creation
      # des etapes et des concepts. Il n'y a aucun traitement particulier à réaliser
      # Dans d'autres conditions, il faudrait surcharger cette méthode.
      return

   def get_created_sd(self):
      """Retourne la liste des sd réellement produites par l'étape.
      Si reuse est présent, `self.sd` a été créée avant, donc n'est pas dans
      cette liste."""
      sdprods = self.sdprods[:]
      if not self.reuse and self.sd:
          sdprods.append(self.sd)
      return sdprods

   def get_last_concept(self):
       """Retourne le dernier concept produit dans la macro.
       Peut-être utile pour accéder au contenu 'fortran' dans une
       clause 'except'."""
       return self.g_context.get(self.last, None)

   def accept(self,visitor):
      """
         Cette methode permet de parcourir l'arborescence des objets
         en utilisant le pattern VISITEUR
      """
      visitor.visitMACRO_ETAPE(self)

   def update_context(self,d):
      """
         Met à jour le contexte contenu dans le dictionnaire d
         Une MACRO_ETAPE peut ajouter plusieurs concepts dans le contexte
         Une fonction enregistree dans op_init peut egalement modifier le contexte
      """
      if type(self.definition.op_init) == types.FunctionType:
        apply(self.definition.op_init,(self,d))
      if self.sd != None:d[self.sd.nom]=self.sd
      for co in self.sdprods:
        d[co.nom]=co

   def make_include(self, unite=None, fname=None):
      """Inclut un fichier dont l'unite logique est `unite` ou de nom `fname`"""
      if unite is not None:
         warn("'unite' is deprecated, please use 'fname' instead",
              DeprecationWarning, stacklevel=2)
         fname = 'fort.%s' % unite
      if not fname:
         return
      f, text = self.get_file(fic_origine=self.parent.nom, fname=fname)
      self.fichier_init = f
      if f == None:
         return
      self.make_contexte(f, text)

   def make_poursuite(self):
      """Inclut un fichier poursuite"""
      raise NotImplementedError('this method must be derivated (in Eficas)')

   def make_contexte(self,f,text):
      """
          Interprete le texte fourni (text) issu du fichier f
          dans le contexte du parent.
          Cette methode est utile pour le fonctionnement des
          INCLUDE
      """
      # on execute le texte fourni dans le contexte forme par
      # le contexte de l etape pere (global au sens Python)
      # et le contexte de l etape (local au sens Python)
      code = compile(text,f,'exec')
      d = self.g_context = self.macro_const_context
      globs = self.get_global_contexte()
      d.update(globs)
      exec code in globs, d
      # pour ne pas conserver des références sur tout
      self.macro_const_context = {}

   def get_global_contexte(self):
      """
          Cette methode retourne le contexte global fourni
          par le parent(self) a une etape fille (l'appelant) pour
          realiser des evaluations de texte Python (INCLUDE,...)
      """
      # Le contexte global est forme par concatenation du contexte
      # du parent de self et de celui de l'etape elle meme (self)
      # Pour les concepts, cela ne doit rien changer. Mais pour les constantes,
      # les valeurs de get_contexte_avant sont moins récentes que dans
      # get_global_contexte. On prend donc la précaution de ne pas écraser
      # ce qui y est déjà.
      d = self.parent.get_global_contexte()
      d.update( self.g_context )
      d.update( [(k, v) for k, v in self.parent.get_contexte_avant(self).items()
                        if d.get(k) is None] )
      return d

   def get_contexte_courant(self, etape_fille_du_jdc=None):
      """
         Retourne le contexte tel qu'il est au moment de l'exécution de
         l'étape courante.
      """
      ctx = {}
      # update car par ricochet on modifierait jdc.current_context
      ctx.update( self.parent.get_contexte_courant(self) )
      # on peut mettre None car toujours en PAR_LOT='NON', donc la dernière
      ctx.update( self.get_contexte_avant(None) )
      return ctx

   def get_concept(self, nomsd):
      """
          Méthode pour recuperer un concept à partir de son nom
          dans le contexte du jdc connu avant l'exécution de la macro courante.
      """
      # chercher dans self.get_contexte_avant, puis si non trouve
      # self.parent.get_concept est peut-etre plus performant
      co = self.get_contexte_courant().get(nomsd.strip(), None)
      if not isinstance(co, ASSD):
          co = None
      return co

   def get_concept_by_type(self, nomsd, typesd, etape=None):
      """
          Méthode pour récuperer un concept à partir de son nom et de son type.
          Il aura comme père 'etape' (ou la macro courante si etape est absente).
      """
      return self.parent.get_concept_by_type(nomsd, typesd, etape=etape or self)
      
   def copy(self):
      """ Méthode qui retourne une copie de self non enregistrée auprès du JDC
          et sans sd
          On surcharge la methode de ETAPE pour exprimer que les concepts crees
          par la MACRO d'origine ne sont pas crees par la copie mais eventuellement
          seulement utilises
      """
      etape=N_ETAPE.ETAPE.copy(self)
      etape.sdprods=[]
      return etape

   def copy_intern(self,etape):
      """ Cette méthode effectue la recopie des etapes internes d'une macro
          passée en argument (etape)
      """
      self.etapes=[]
      self.index_etapes={}
      for etp in etape.etapes:
          new_etp=etp.copy()
          new_etp.copy_reuse(etp)
          new_etp.copy_sdnom(etp)
          new_etp.reparent(self)
          if etp.sd:
             new_sd = etp.sd.__class__(etape=new_etp)
             new_etp.sd = new_sd
             if etp.reuse:
                new_sd.set_name(etp.sd.nom)
             else:
                self.NommerSdprod(new_sd,etp.sd.nom)
          new_etp.copy_intern(etp)
          self.etapes.append(new_etp)
          self.index_etapes[new_etp] = len(self.etapes) - 1


   def reset_jdc(self,new_jdc):
       """
          Reinitialise l'etape avec un nouveau jdc parent new_jdc
       """
       if self.sd and self.reuse == None :
           self.parent.NommerSdprod(self.sd,self.sd.nom)
       for concept in self.sdprods:
           self.parent.NommerSdprod(concept,concept.nom)

   def reparent(self,parent):
       """
         Cette methode sert a reinitialiser la parente de l'objet
       """
       N_ETAPE.ETAPE.reparent(self,parent)
       #on ne change pas la parenté des concepts. On s'assure uniquement que le jdc en référence est le bon
       for concept in self.sdprods:
           concept.jdc=self.jdc
       for e in self.etapes:
           e.reparent(self)

   def update_const_context(self, d):
      """
         Met à jour le contexte des constantes pour l'évaluation de
         formules dans la macro.
      """
      # Dans le jdc, const_context est mis à jour par exec_compile
      # Dans la macro, on n'a pas le code à compiler pour récupèrer les
      # constantes locales à la macro. On demande donc explicitement de
      # définir les constantes "locales".
      self.macro_const_context.update(d)

   def sd_accessible(self):
      """On peut acceder aux "valeurs" (jeveux) des ASSD dans
      les macro-commandes qui sont localement en PAR_LOT="NON"
      sauf pour INCLUDE.
      """
      if CONTEXT.debug: print ' `- MACRO sd_accessible :', self.nom
      return self.parent.sd_accessible() or not self.is_include()
