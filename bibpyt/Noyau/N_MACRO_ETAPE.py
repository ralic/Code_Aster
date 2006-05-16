#@ MODIF N_MACRO_ETAPE Noyau  DATE 10/05/2006   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
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
    Ce module contient la classe MACRO_ETAPE qui sert à vérifier et à exécuter
    une commande
"""

# Modules Python
import types,sys,string
import traceback

# Modules EFICAS
import N_MCCOMPO
import N_ETAPE
from N_Exception import AsException
import N_utils
from N_utils import AsType
from N_CO import CO

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
      self.definition=oper
      self.reuse=reuse
      self.valeur=args
      self.nettoiargs()
      self.parent=CONTEXT.get_current_step()
      self.etape = self
      self.nom=oper.nom
      self.idracine=oper.label
      self.appel=N_utils.callee_where()
      self.mc_globaux={}
      self.g_context={}
      # Contexte courant
      self.current_context={}
      self.index_etape_courante=0
      self.etapes=[]
      self.sds=[]
      #  Dans le cas d'une macro écrite en Python, l'attribut Outputs est un 
      #  dictionnaire qui contient les concepts produits de sortie 
      #  (nom : ASSD) déclarés dans la fonction sd_prod
      self.Outputs={}
      self.sd=None
      self.actif=1
      self.sdprods=[]
      self.make_register()
      self.UserError="UserError"

   def make_register(self):
      """
         Initialise les attributs jdc, id, niveau et réalise les enregistrements
         nécessaires
      """
      if self.parent :
         self.jdc = self.parent.get_jdc_root()
         self.id=self.parent.register(self)
         self.niveau=None
         self.UserError=self.jdc.UserError
      else:
         self.jdc = self.parent =None
         self.id=None
         self.niveau=None
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
               sd.nom=nom
         self.reset_current_step()
      except AsException,e:
         self.reset_current_step()
         raise AsException("Etape ",self.nom,'ligne : ',self.appel[0],
                              'fichier : ',self.appel[1],e)
      except (EOFError,self.UserError):
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

   def mark_CO(self):
      """
         Marquage des concepts CO d'une macro-commande
      """
      # On marque les concepts CO pour verification ulterieure de leur bonne utilisation
      l=self.get_all_co()
      for c in l:
          #if not hasattr(c,"_etape") or c._etape is not c.etape: 
             c._etape=self
      return l

   def get_sd_prod(self):
      """
        Retourne le concept résultat d'une macro étape
        La difference avec une etape ou une proc-etape tient a ce que
         le concept produit peut exister ou pas
        Si sd_prod == None le concept produit n existe pas on retourne None
        Deux cas :
         cas 1 : sd_prod  n'est pas une fonction
                 il s'agit d'une sous classe de ASSD
                 on construit le sd à partir de cette classe
                 et on le retourne
         cas 2 : sd_prod est une fonction
                  on l'évalue avec les mots-clés de l'étape (mc_liste)
                 on construit le sd à partir de la classe obtenue
                 et on le retourne
      """
      sd_prod=self.definition.sd_prod
      self.typret=None
      # On marque les concepts CO pour verification ulterieure de leur bonne utilisation
      self.mark_CO()

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
            cas 1 : sd_prod de oper n'est pas une fonction
                    il s'agit d'une sous classe de ASSD
                    on retourne le nom de la classe
            cas 2 : il s'agit d'une fonction
                    on l'évalue avec les mots-clés de l'étape (mc_liste)
                    et on retourne son résultat
      """
      if not force and hasattr(self,'typret'): return self.typret
      # On marque les concepts CO pour verification ulterieure de leur bonne utilisation
      self.mark_CO()

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
      # Si on insère des commandes (par ex, dans EFICAS), il faut
      # préalablement remettre ce pointeur à 0
      if etape:
         index_etape=self.etapes.index(etape)
      else:
         index_etape=len(self.etapes)

      if index_etape >= self.index_etape_courante:
         # On calcule le contexte en partant du contexte existant
         d=self.current_context
         liste_etapes=self.etapes[self.index_etape_courante:index_etape]
      else:
         d=self.current_context={}
         liste_etapes=self.etapes

      for e in liste_etapes:
        if e is etape:
           break
        if e.isactif():
           e.update_context(d)
      self.index_etape_courante=index_etape
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

   def type_sdprod(self,co,t):
      """
           Cette methode a pour fonction de typer le concept co avec le type t
            dans les conditions suivantes
            1- co est un concept produit de self
            2- co est un concept libre : on le type et on l attribue à self
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
         co.etape=self
         co.__class__ = t
         self.sdprods.append(co)

      elif co.etape== self:
         # Cas 2 : le concept est produit par la macro (self)
         # On est deja passe par type_sdprod (Cas 1 ou 3).
         # Il suffit de le mettre dans la liste des concepts produits (self.sdprods)
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
      idetape=self.jdc.g_register(etape)
      return idetape

   def reg_sd(self,sd):
      """ 
           Methode appelee dans l __init__ d un ASSD a sa creation pour
           s enregistrer (reserve aux ASSD créés au sein d'une MACRO)
      """
      self.sds.append(sd)
      return self.jdc.o_register(sd)

   def create_sdprod(self,etape,nomsd):
      """ 
          Intention : Cette methode doit fabriquer le concept produit retourne
                  par l'etape etape et le nommer.
                  Elle est appelée à l'initiative de l'etape
                  pendant le processus de construction de cette etape : methode __call__
                  de la classe CMD (OPER ou MACRO)
                  Ce travail est réalisé par le contexte supérieur (etape.parent)
                  car dans certains cas, le concept ne doit pas etre fabriqué mais
                  l'etape doit simplement utiliser un concept préexistant.
                  Cas 1 : etape.reuse != None : le concept est réutilisé
                  Cas 2 : l'étape appartient à une macro qui a déclaré un concept
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
         # On donne au concept le type produit par la sous commande.
         # Le principe est le suivant : apres avoir verifie que le type deduit par la sous commande
         # est bien coherent avec celui initialement affecte par la macro (voir ci dessus)
         # on affecte au concept ce type car il peut etre plus precis (derive, en general)
         sd.__class__=sdprod
         # On force également le nom stocké dans l'attribut sdnom : on lui donne le nom 
         # du concept associé à nomsd
         etape.sdnom=sd.nom
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
          Cette methode est appelee par les etapes internes de la macro
          La macro appelle le JDC pour valider le nommage
          On considere que l espace de nom est unique et géré par le JDC
          Si le nom est deja utilise, l appel leve une exception
          Si restrict=='non', on insere le concept dans le contexte de la macro
          Si restrict=='oui', on n'insere pas le concept dans le contexte de la macro
      """
      # Normalement, lorsqu'on appelle cette methode, on ne veut nommer que des concepts nouvellement crees.
      # Le filtrage sur les concepts a creer ou a ne pas creer est fait dans la methode
      # create_sdprod. La seule chose a verifier apres conversion eventuelle du nom
      # est de verifier que le nom n'est pas deja attribue. Ceci est fait en delegant
      # au JDC par l'intermediaire du parent.

      #XXX attention inconsistence : prefix et gcncon ne sont pas 
      # définis dans le package Noyau. La methode NommerSdprod pour
      # les macros devrait peut etre etre déplacée dans Build ???

      if CONTEXT.debug : print "MACRO.NommerSdprod: ",sd,sdnom

      if hasattr(self,'prefix'):
        # Dans le cas de l'include_materiau on ajoute un prefixe au nom du concept
        if sdnom != self.prefix:sdnom=self.prefix+sdnom

      if self.Outputs.has_key(sdnom):
        # Il s'agit d'un concept de sortie de la macro produit par une sous commande
        sdnom=self.Outputs[sdnom].nom
      elif sdnom != '' and sdnom[0] == '_':
        # Si le nom du concept commence par le caractere _ on lui attribue
        # un identificateur JEVEUX construit par gcncon et respectant
        # la regle gcncon legerement adaptee ici
        # nom commencant par __ : il s'agit de concepts qui seront detruits
        # nom commencant par _ : il s'agit de concepts intermediaires qui seront gardes
        # ATTENTION : il faut traiter différemment les concepts dont le nom
        # commence par _ mais qui sont des concepts nommés automatiquement par
        # une éventuelle sous macro.
        # Le test suivant n'est pas tres rigoureux mais permet de fonctionner pour le moment (a améliorer)
        if sdnom[1] in string.digits:
          # Ce concept provient probablement d'une macro appelee par self
          pass
        elif sdnom[1] == '_':
          sdnom=self.gcncon('.')
        else:
          sdnom=self.gcncon('_')
      else:
        # On est dans le cas d'un nom de concept global. 
        pass

      if restrict == 'non':
         # On demande le nommage au parent mais sans ajout du concept dans le contexte du parent
         # car on va l'ajouter dans le contexte de la macro
         self.parent.NommerSdprod(sd,sdnom,restrict='oui')
         # On ajoute dans le contexte de la macro les concepts nommes
         # Ceci est indispensable pour les CO (macro) dans un INCLUDE
         self.g_context[sdnom]=sd
      else:
         # La demande de nommage vient probablement d'une macro qui a mis
         # le concept dans son contexte. On ne traite plus que le nommage (restrict="oui")
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

   def make_include(self,unite=None):
      """
          Inclut un fichier dont l'unite logique est unite
      """
      if not unite : return
      f,text=self.get_file(unite=unite,fic_origine=self.parent.nom)
      self.fichier_init = f
      if f == None:return
      self.make_contexte(f,text)

   def make_poursuite(self):
      """
          Inclut un fichier poursuite
      """
      try:
         f,text=self.get_file(fic_origine=self.parent.nom)
      except:
         raise AsException("Impossible d'ouvrir la base pour une poursuite")
      self.fichier_init=f
      if f == None:return
      self.make_contexte(f,text)

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
      code=compile(text,f,'exec')
      d={}
      self.g_context = d
      self.contexte_fichier_init = d
      globs=self.parent.get_global_contexte()
      exec code in globs,d

   def get_global_contexte(self):
      """
          Cette methode retourne le contexte global fourni
          par le parent(self) a une etape fille (l'appelant) pour
          realiser des evaluations de texte Python (INCLUDE,...)
      """
      # Le contexte global est forme par concatenation du contexte
      # du parent de self et de celui de l'etape elle meme (self)
      d=self.parent.get_global_contexte()
      d.update(self.g_context)
      return d

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
      for etp in etape.etapes:
          new_etp=etp.copy()
          new_etp.copy_reuse(etp)
          new_etp.copy_sdnom(etp)
          new_etp.reparent(self)
          if etp.sd:
             new_sd = etp.sd.__class__(etape=new_etp)
             new_etp.sd = new_sd
             if etp.reuse:
                new_sd.nom = etp.sd.nom
             else:
                self.NommerSdprod(new_sd,etp.sd.nom)
          new_etp.copy_intern(etp)
          self.etapes.append(new_etp)

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
