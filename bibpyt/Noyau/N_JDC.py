#@ MODIF N_JDC Noyau  DATE 30/05/2007   AUTEUR COURTOIS M.COURTOIS 
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
      self.g_context={}
      # Liste pour stocker tous les concepts produits dans le JDC
      self.sds=[]
      # Dictionnaire pour stocker tous les concepts du JDC (acces rapide par le nom)
      self.sds_dict={}
      self.etapes=[]
      self.mc_globaux={}
      self.current_context={}
      self.condition_context={}
      self.index_etape_courante=0
      self.UserError="UserError"
      self.alea = None

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
      except SyntaxError,e:
        if CONTEXT.debug : traceback.print_exc()
        l=traceback.format_exception_only(SyntaxError,e)
        self.cr.exception("Compilation impossible : "+string.join(l))
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
         for obj_cata in self.cata:
            if type(obj_cata) == types.ModuleType :
               init2 = "from "+obj_cata.__name__+" import *"
               exec init2 in self.g_context

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
         exec self.proc_compile in self.g_context

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
        self.cr.exception(str(e))
        CONTEXT.unset_current_step()

      except NameError,e:
        etype, value, tb = sys.exc_info()
        l= traceback.extract_tb(tb)
        s= traceback.format_exception_only("Erreur de nom",e)[0][:-1]
        message = "erreur de syntaxe,  %s ligne %d" % (s,l[-1][1])
        if CONTEXT.debug :
          traceback.print_exc()
        self.cr.exception(message)
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
       print "FIN D'EXECUTION",mode,etape

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
      if CONTEXT.debug : print "JDC.NommerSdprod ",sd,sdnom

      o=self.sds_dict.get(sdnom,None)
      if isinstance(o,ASSD):
         raise AsException("Nom de concept deja defini : %s" % sdnom)

      # ATTENTION : Il ne faut pas ajouter sd dans sds car il s y trouve deja.
      # Ajoute a la creation (appel de reg_sd).
      self.sds_dict[sdnom]=sd
      sd.set_name(sdnom)

      # En plus si restrict vaut 'non', on insere le concept dans le contexte du JDC
      if restrict == 'non':
         self.g_context[sdnom]=sd

   def reg_sd(self,sd):
      """ 
          Methode appelee dans l __init__ d un ASSD lors de sa creation 
          pour s enregistrer
      """
      self.sds.append(sd)
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

   def get_file(self,unite=None,fic_origine=''):
      """
          Retourne le nom du fichier correspondant à un numero d'unité 
          logique (entier) ainsi que le source contenu dans le fichier
      """
      if self.appli :
         # Si le JDC est relié à une application maitre, on délègue la recherche
         file,text= self.appli.get_file(unite,fic_origine)
      else:
         file = None
         if unite != None:
            if os.path.exists("fort."+str(unite)):
               file= "fort."+str(unite)
         if file == None :
            raise AsException("Impossible de trouver le fichier correspondant"
                               " a l unite %s" % unite)
         if not os.path.exists(file):
            raise AsException("%s n'est pas un fichier existant" % unite)
         fproc=open(file,'r')
         text=fproc.read()
         fproc.close()
      if file == None : return None,None
      text=string.replace(text,'\r\n','\n')
      linecache.cache[file]=0,0,string.split(text,'\n'),file
      return file,text

   def set_par_lot(self,par_lot):
      """ 
          Met le mode de traitement a PAR LOT 
          ou a COMMANDE par COMMANDE
          en fonction de la valeur du mot cle PAR_LOT et 
          du contexte : application maitre ou pas
      """
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
      if etape:
         index_etape=self.etapes.index(etape)
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
         if self.context_ini:d.update(self.context_ini)
         liste_etapes=self.etapes

      for e in liste_etapes:
         if e is etape:
            break
         if e.isactif():
            e.update_context(d)
      self.index_etape_courante=index_etape
      return d

   def get_global_contexte(self):
      return self.g_context.copy()

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
       etape.reparent(self)
       etape.reset_jdc(self)
