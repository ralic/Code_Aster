# coding=utf-8
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
   Ce module contient la classe JDC qui sert à interpréter un jeu de commandes
"""

# Modules Python
import os,string,traceback
import types,sys,linecache

# Modules EFICAS
import N_OBJECT
import N_CR
from N_Exception import AsException
from N_ASSD import ASSD
from N_info import message, SUPERV


MemoryErrorMsg = """MemoryError :

En général, cette erreur se produit car la mémoire utilisée hors du fortran
(jeveux) est importante.

Causes possibles :
   - le calcul produit de gros objets Python dans une macro-commande ou
     dans le jeu de commande lui-même,
   - le calcul appelle un solveur (MUMPS par exemple) ou un outil externe
     qui a besoin de mémoire hors jeveux,
   - utilisation de jeveux dynamique,
   - ...

Solution :
   - distinguer la mémoire limite du calcul (case "Mémoire totale" de astk)
     de la mémoire réservée à jeveux (case "dont Aster"), le reste étant
     disponible pour les allocations dynamiques.
"""



class JDC(N_OBJECT.OBJECT):
   """
      Cette classe interprete un jeu de commandes fourni sous
      la forme d'une chaine de caractères

      Attributs de classe :

      Attributs d'instance :

   """
   nature = "JDC"
   CR=N_CR.CR
   exec_init="""
import Accas
from Accas import _F
from Accas import *
NONE = None
"""

   from N_utils import SEP

   def __init__(self,definition=None,procedure=None,cata=None,
                     cata_ord_dico=None,parent=None,
                     nom='SansNom',appli=None,context_ini=None,**args):
      self.procedure=procedure
      self.definition = definition
      self.cata=cata
      if type(self.cata) != types.TupleType and cata != None:
         self.cata=(self.cata,)
      self._build_reserved_kw_list()
      self.cata_ordonne_dico=cata_ord_dico
      self.nom = nom
      self.appli=appli
      self.parent=parent
      self.context_ini=context_ini
      # On conserve les arguments supplémentaires. Il est possible de passer
      # des informations globales au JDC par ce moyen. Il pourrait etre plus
      # sur de mettre en place le mecanisme des mots-cles pour verifier la
      # validité des valeurs passées.
      # Ceci reste à faire
      # On initialise avec les parametres de la definition puis on
      # update avec ceux du JDC
      self.args=self.definition.args
      self.args.update(args)
      self.nstep=0
      self.nsd=0
      self.par_lot='OUI'
      self.par_lot_user = None
      if definition:
         self.regles=definition.regles
         self.code = definition.code
      else:
         self.regles=()
         self.code = "CODE"
      #
      #  Creation de l objet compte rendu pour collecte des erreurs
      #
      self.cr = self.CR(debut = "CR phase d'initialisation",
                        fin = "fin CR phase d'initialisation")
      # on met le jdc lui-meme dans le context global pour l'avoir sous
      # l'etiquette "jdc" dans le fichier de commandes
      self.g_context={ 'jdc' : self }
      #message.debug(SUPERV, "g_context : %s - %s", self.g_context, id(self.g_context))
      # Dictionnaire pour stocker tous les concepts du JDC (acces rapide par le nom)
      self.sds_dict={}
      self.etapes=[]
      self.index_etapes = {}
      self.mc_globaux={}
      self.current_context={}
      self.condition_context={}
      self.index_etape_courante=0
      self.UserError="UserError"
      self.alea = None
      # permet transitoirement de conserver la liste des étapes
      self.hist_etape = False

   def compile(self):
      """
         Cette methode compile la chaine procedure
         Si des erreurs se produisent, elles sont consignées dans le
         compte-rendu self.cr
      """
      try:
         if self.appli != None :
            self.appli.affiche_infos('Compilation du fichier de commandes en cours ...')
         self.proc_compile=compile(self.procedure,self.nom,'exec')
      except SyntaxError, e:
         if CONTEXT.debug : traceback.print_exc()
         l=traceback.format_exception_only(SyntaxError,e)
         self.cr.exception("Compilation impossible : "+string.join(l))
      except MemoryError, e:
         self.cr.exception(MemoryErrorMsg)
      except SystemError, e:
         erreurs_connues = """
Causes possibles :
 - offset too large : liste trop longue derrière un mot-clé.
   Solution : liste = (valeurs, ..., )
              MOT_CLE = *liste,
"""
         l=traceback.format_exception_only(SystemError,e)
         l.append(erreurs_connues)
         self.cr.exception("Compilation impossible : " + ''.join(l))
      return

   def exec_compile(self):
      """
         Cette méthode execute le jeu de commandes compilé dans le contexte
         self.g_context de l'objet JDC
      """
      CONTEXT.set_current_step(self)
      # Le module nommage utilise le module linecache pour accéder
      # au source des commandes du jeu de commandes.
      # Dans le cas d'un fichier, on accède au contenu de ce fichier
      # Dans le cas d'une chaine de caractères il faut accéder
      # aux commandes qui sont dans la chaine
      import linecache
      linecache.cache[self.nom]=0,0,string.split(self.procedure,'\n'),self.nom
      try:
         exec self.exec_init in self.g_context
         #message.debug(SUPERV, "JDC.exec_compile_1 - len(g_context) = %d", len(self.g_context.keys()))
         for obj_cata in self.cata:
            if type(obj_cata) == types.ModuleType :
               init2 = "from "+obj_cata.__name__+" import *"
               exec init2 in self.g_context
         #message.debug(SUPERV, "JDC.exec_compile_2 - len(g_context) = %d", len(self.g_context.keys()))

         # Initialisation du contexte global pour l'évaluation des conditions de BLOC
         # On utilise une copie de l'initialisation du contexte du jdc
         self.condition_context=self.g_context.copy()

         # Si l'attribut context_ini n'est pas vide, on ajoute au contexte global
         # le contexte initial (--> permet d'évaluer un JDC en récupérant un contexte
         # d'un autre par exemple)
         if self.context_ini :
            self.g_context.update(self.context_ini)
            # Update du dictionnaire des concepts
            for sdnom,sd in self.context_ini.items():
               if isinstance(sd,ASSD):self.sds_dict[sdnom]=sd

         if self.appli != None :
            self.appli.affiche_infos('Interprétation du fichier de commandes en cours ...')
         # On sauve le contexte pour garder la memoire des constantes
         # En mode edition (EFICAS) ou lors des verifications le contexte
         # est recalculé
         # mais les constantes sont perdues
         self.const_context=self.g_context
         #message.debug(SUPERV, "pass")
         exec self.proc_compile in self.g_context
         #message.debug(SUPERV, "JDC.exec_compile_3 - len(g_context) = %d", len(self.g_context.keys()))

         CONTEXT.unset_current_step()
         if self.appli != None : self.appli.affiche_infos('')

      except EOFError:
        # Exception utilise pour interrompre un jeu
        # de commandes avant la fin
        # Fonctionnement normal, ne doit pas etre considere comme une erreur
        CONTEXT.unset_current_step()
        self.affiche_fin_exec()
        self.traiter_fin_exec('commande')

      except AsException,e:
        # une erreur a ete identifiee
        if CONTEXT.debug :
          traceback.print_exc()
        # l'exception a été récupérée avant (où, comment ?),
        # donc on cherche dans le texte
        txt = str(e)
        if txt.find('MemoryError') >= 0:
           txt = MemoryErrorMsg
        self.cr.exception(txt)
        CONTEXT.unset_current_step()

      except NameError,e:
        etype, value, tb = sys.exc_info()
        l= traceback.extract_tb(tb)
        s= traceback.format_exception_only("Erreur de nom",e)[0][:-1]
        msg = "erreur de syntaxe,  %s ligne %d" % (s,l[-1][1])
        if CONTEXT.debug :
          traceback.print_exc()
        self.cr.exception(msg)
        CONTEXT.unset_current_step()

      except self.UserError,exc_val:
        self.traiter_user_exception(exc_val)
        CONTEXT.unset_current_step()
        self.affiche_fin_exec()
        self.traiter_fin_exec('commande')

      except :
        # erreur inattendue
        # sys_exc_typ,sys_exc_value,sys_exc_frame = sys_exc.info()
        # (tuple de 3 éléments)
        if CONTEXT.debug : traceback.print_exc()

        exc_typ,exc_val,exc_fr=sys.exc_info()
        l=traceback.format_exception(exc_typ,exc_val,exc_fr)
        self.cr.exception("erreur non prevue et non traitee prevenir la maintenance "+
                           self.nom+'\n'+ string.join(l))
        del exc_typ,exc_val,exc_fr
        CONTEXT.unset_current_step()

   def affiche_fin_exec(self):
       """
          Cette methode realise l'affichage final des statistiques de temps
          apres l'execution de toutes
          les commandes en mode commande par commande ou par lot
          Elle doit etre surchargee pour en introduire un
       """
       return

   def traiter_fin_exec(self,mode,etape=None):
       """
          Cette methode realise un traitement final apres l'execution de toutes
          les commandes en mode commande par commande ou par lot
          Par defaut il n'y a pas de traitement. Elle doit etre surchargee
          pour en introduire un
       """
       message.info(SUPERV, "FIN D'EXECUTION %s %s", mode, etape)

   def traiter_user_exception(self,exc_val):
       """Cette methode realise un traitement sur les exceptions utilisateur
          Par defaut il n'y a pas de traitement. La méthode doit etre
          surchargée pour en introduire un.
       """
       return

   def register(self,etape):
      """
         Cette méthode ajoute etape dans la liste des etapes : self.etapes
         et retourne un numéro d'enregistrement
      """
      self.etapes.append(etape)
      self.index_etapes[etape] = len(self.etapes) - 1
      #message.debug(SUPERV, "#%d %s", self.index_etapes[etape], etape.nom)
      return self.g_register(etape)

   def o_register(self,sd):
      """
         Retourne un identificateur pour concept
      """
      self.nsd=self.nsd+1
      nom=sd.idracine + self.SEP + `self.nsd`
      return nom

   def g_register(self,etape):
      """
          Retourne un identificateur pour etape
      """
      self.nstep=self.nstep+1
      idetape=etape.idracine + self.SEP + `self.nstep`
      return idetape

   def create_sdprod(self,etape,nomsd):
      """
          Cette methode doit fabriquer le concept produit retourne
          par l'etape etape et le nommer.

          Elle est appelée à l'initiative de l'etape
          pendant le processus de construction de cette etape :
          methode __call__ de la classe CMD (OPER ou MACRO)

          Ce travail est réalisé par le contexte supérieur
          (etape.parent) car dans certains cas, le concept ne doit
          pas etre fabriqué mais l'etape doit simplement utiliser
          un concept préexistant.

          Deux cas possibles :
                  - Cas 1 : etape.reuse != None : le concept est réutilisé
                  - Cas 2 : l'étape appartient à une macro qui a déclaré un
                          concept de sortie qui doit etre produit par cette
                          etape.
          Dans le cas du JDC, le deuxième cas ne peut pas se produire.
      """
      sd= etape.get_sd_prod()
      if sd != None and (etape.definition.reentrant == 'n' or etape.reuse is None) :
         # ATTENTION : On ne nomme la SD que dans le cas de non reutilisation
         # d un concept. Commande non reentrante ou reuse absent.
         self.NommerSdprod(sd,nomsd)
      return sd

   def NommerSdprod(self,sd,sdnom,restrict='non'):
      """
          Nomme la SD apres avoir verifie que le nommage est possible : nom
          non utilise
          Si le nom est deja utilise, leve une exception
          Met le concept créé dans le concept global g_context
      """
      o=self.sds_dict.get(sdnom,None)
      if isinstance(o,ASSD):
         raise AsException("Nom de concept deja defini : %s" % sdnom)
      if sdnom in self._reserved_kw:
         raise AsException("Nom de concept invalide. '%s' est un mot-clé réservé." % sdnom)

      # Ajoute a la creation (appel de reg_sd).
      self.sds_dict[sdnom]=sd
      sd.set_name(sdnom)

      # En plus si restrict vaut 'non', on insere le concept dans le contexte du JDC
      if restrict == 'non':
         self.g_context[sdnom]=sd
         #message.debug(SUPERV, "g_context[%r] = %s", sdnom, sd)

   def reg_sd(self,sd):
      """
          Methode appelee dans l __init__ d un ASSD lors de sa creation
          pour s enregistrer
      """
      return self.o_register(sd)

   def delete_concept_after_etape(self,etape,sd):
      """
          Met à jour les étapes du JDC qui sont après etape suite à
          la disparition du concept sd
      """
      # Cette methode est définie dans le noyau mais ne sert que pendant
      # la phase de creation des etapes et des concepts. Il n'y a aucun
      # traitement particulier à réaliser.
      # Dans d'autres conditions, il faut surcharger cette méthode
      return

   def supprime(self):
      N_OBJECT.OBJECT.supprime(self)
      for etape in self.etapes:
         etape.supprime()

   def clean(self, netapes):
      """Nettoie les `netapes` dernières étapes de la liste des étapes."""
      if self.hist_etape:
          return
      for i in xrange(netapes):
        e=self.etapes.pop()
        jdc=e.jdc
        parent=e.parent
        e.supprime()
        e.parent=parent
        e.jdc=jdc
        #message.debug(SUPERV, "JDC.clean - etape = %r - refcount(e) = %d",
                      #e.nom, sys.getrefcount(e))
        del self.index_etapes[e]


   def get_file(self, unite=None, fic_origine='', fname=None):
      """
          Retourne le nom du fichier correspondant à un numero d'unité
          logique (entier) ainsi que le source contenu dans le fichier
      """
      if self.appli :
         # Si le JDC est relié à une application maitre, on délègue la recherche
         return self.appli.get_file(unite, fic_origine)
      else:
         if unite != None:
            if os.path.exists("fort."+str(unite)):
               fname= "fort."+str(unite)
         if fname == None :
            raise AsException("Impossible de trouver le fichier correspondant"
                               " a l unite %s" % unite)
         if not os.path.exists(fname):
            raise AsException("%s n'est pas un fichier existant" % fname)
         fproc = open(fname, 'r')
         text=fproc.read()
         fproc.close()
         text = text.replace('\r\n', '\n')
         linecache.cache[fname] = 0, 0, text.split('\n'), fname
         return fname, text

   def set_par_lot(self, par_lot, user_value=False):
      """
      Met le mode de traitement a PAR LOT
      ou a COMMANDE par COMMANDE
      en fonction de la valeur du mot cle PAR_LOT et
      du contexte : application maitre ou pas
      
      En PAR_LOT='NON', il n'y a pas d'ambiguité.
      En PAR_LOT='OUI', E_SUPERV positionne l'attribut à 'NON' après la phase
      d'analyse et juste avant la phase d'exécution.
      `user_value` : permet de stocker la valeur choisie par l'utilisateur
      pour l'interroger plus tard (par exemple dans `get_contexte_avant`).
      """
      #message.debug(SUPERV, "set par_lot = %r", par_lot)
      if user_value:
          self.par_lot_user = par_lot
      if self.appli == None:
        # Pas d application maitre
        self.par_lot=par_lot
      else:
        # Avec application maitre
        self.par_lot='OUI'

   def accept(self,visitor):
      """
         Cette methode permet de parcourir l'arborescence des objets
         en utilisant le pattern VISITEUR
      """
      visitor.visitJDC(self)

   def interact(self):
      """
          Cette methode a pour fonction d'ouvrir un interpreteur
          pour que l'utilisateur entre des commandes interactivement
      """
      CONTEXT.set_current_step(self)
      try:
         # Le module nommage utilise le module linecache pour accéder
         # au source des commandes du jeu de commandes.
         # Dans le cas d'un fichier, on accède au contenu de ce fichier
         # Dans le cas de la console interactive, il faut pouvoir accéder
         # aux commandes qui sont dans le buffer de la console
         import linecache,code
         console= code.InteractiveConsole(self.g_context,filename="<console>")
         linecache.cache["<console>"]=0,0,console.buffer,"<console>"
         banner="""***********************************************
*          Interpreteur interactif %s
***********************************************""" % self.code
         console.interact(banner)
      finally:
         console=None
         CONTEXT.unset_current_step()

   def get_contexte_avant(self,etape):
      """
         Retourne le dictionnaire des concepts connus avant etape
         On tient compte des commandes qui modifient le contexte
         comme DETRUIRE ou les macros
         Si etape == None, on retourne le contexte en fin de JDC
      """
      # L'étape courante pour laquelle le contexte a été calculé est
      # mémorisée dans self.index_etape_courante
      # XXX on pourrait faire mieux dans le cas PAR_LOT="NON" : en
      # mémorisant l'étape
      # courante pendant le processus de construction des étapes.
      # Si on insère des commandes (par ex, dans EFICAS), il faut préalablement
      # remettre ce pointeur à 0
      #message.debug(SUPERV, "g_context : %s", [k for k, v in self.g_context.items() if isinstance(v, ASSD)])
      #message.debug(SUPERV, "current_context : %s", [k for k, v in self.current_context.items() if isinstance(v, ASSD)])
      if self.par_lot_user == 'NON':
          d = self.current_context = self.g_context.copy()
          if etape is None:
              return d
          # retirer les sd produites par 'etape'
          sd_names = [sd.nom for sd in etape.get_created_sd()]
          #message.debug(SUPERV, "reuse : %s, sdprods : %s", etape.reuse, sd_names)
          for nom in sd_names:
             try:
                  del d[nom]
             except KeyError:
                 from warnings import warn
                 warn("concept '%s' absent du contexte de %s" % (nom, self.nom),
                      RuntimeWarning, stacklevel=2)
          return d
      if etape:
         index_etape = self.index_etapes[etape]
      else:
         index_etape=len(self.etapes)
      if index_etape >= self.index_etape_courante:
         # On calcule le contexte en partant du contexte existant
         d=self.current_context
         if self.index_etape_courante==0 and self.context_ini:
            d.update(self.context_ini)
         liste_etapes=self.etapes[self.index_etape_courante:index_etape]
      else:
         d=self.current_context={}
         if self.context_ini:
            d.update(self.context_ini)
         liste_etapes=self.etapes

      for e in liste_etapes:
         if e is etape:
            break
         if e.isactif():
            e.update_context(d)
      self.index_etape_courante=index_etape
      #message.debug(SUPERV, "returns : %s", [k for k, v in d.items() if isinstance(v, ASSD)])
      return d

   def get_global_contexte(self):
      """Retourne "un" contexte global ;-)"""
      # N'est utilisé que par INCLUDE (sauf erreur).
      # g_context est remis à {} en PAR_LOT='OUI'. const_context permet
      # de retrouver ce qui y a été mis par exec_compile.
      # Les concepts n'y sont pas en PAR_LOT='OUI'. Ils sont ajoutés
      # par get_global_contexte de la MACRO.
      d = self.const_context.copy()
      d.update(self.g_context)
      return d


   def get_contexte_courant(self, etape_courante=None):
      """
         Retourne le contexte tel qu'il est (ou 'sera' si on est en phase
         de construction) au moment de l'exécution de l'étape courante.
      """
      if etape_courante is None:
         etape_courante = CONTEXT.get_current_step()
      return self.get_contexte_avant(etape_courante)


   def get_concept(self, nomsd):
      """
          Méthode pour récuperer un concept à partir de son nom
      """
      co = self.get_contexte_courant().get(nomsd.strip(), None)
      if not isinstance(co, ASSD):
          co = None
      return co

   def get_concept_by_type(self, nomsd, typesd, etape):
      """
          Méthode pour récuperer un concept à partir de son nom et de son type.
          Il aura comme père 'etape'.
      """
      assert issubclass(typesd, ASSD), typesd
      co = typesd(etape=etape)
      co.set_name(nomsd)
      co.executed = 1
      return co

   def del_concept(self, nomsd):
      """
         Méthode pour supprimer la référence d'un concept dans le sds_dict.
         Ne détruire pas le concept (différent de supprime).
      """
      try:
         del self.sds_dict[nomsd.strip()]
      except:
         pass


   def get_cmd(self,nomcmd):
      """
          Méthode pour recuperer la definition d'une commande
          donnee par son nom dans les catalogues declares
          au niveau du jdc
      """
      for cata in self.cata:
          if hasattr(cata,nomcmd):
             return getattr(cata,nomcmd)

   def append_reset(self,etape):
       """
          Ajoute une etape provenant d'un autre jdc a la liste des etapes
          et remet à jour la parenté de l'étape et des concepts
       """
       self.etapes.append(etape)
       self.index_etapes[etape] = len(self.etapes) - 1
       etape.reparent(self)
       etape.reset_jdc(self)

   def sd_accessible(self):
      """On peut acceder aux "valeurs" (jeveux) des ASSD si le JDC est en PAR_LOT="NON".
      """
      if CONTEXT.debug: print ' `- JDC sd_accessible : PAR_LOT =', self.par_lot
      return self.par_lot == 'NON'


   def _build_reserved_kw_list(self):
       """Construit la liste des mots-clés réservés (interdits pour le
       nommage des concepts)."""
       self._reserved_kw = set()
       for cat in self.cata:
           self._reserved_kw.update([kw for kw in dir(cat) if len(kw) <= 8 and kw == kw.upper()])
       self._reserved_kw.difference_update(['OPER', 'MACRO', 'BLOC', 'SIMP', 'FACT', 'FORM',
                                            'GEOM', 'MCSIMP', 'MCFACT'])
