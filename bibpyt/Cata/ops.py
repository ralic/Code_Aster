#@ MODIF ops Cata  DATE 14/02/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
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
# ======================================================================
# RESPONSABLE COURTOIS M.COURTOIS

# Modules Python
import os
import traceback
import cPickle as pickle
import re
from math import sqrt, pi, atan2, tan, log, exp

# Modules Eficas
import Accas
from Accas import ASSD
from Noyau.ascheckers     import CheckLog
from Noyau.N_info import message, SUPERV

try:
   import aster
   aster_exists = True
   # Si le module aster est présent, on le connecte
   # au JDC
   import Build.B_CODE
   Build.B_CODE.CODE.codex=aster

   from Utilitai.Utmess   import UTMESS, MessageLog
   from Build.B_SENSIBILITE_MEMO_NOM_SENSI import MEMORISATION_SENSIBILITE
except:
   aster_exists = False



def commun_DEBUT_POURSUITE(jdc, PAR_LOT, IMPR_MACRO, CODE, DEBUG, IGNORE_ALARM, LANG, INFO):
   """Fonction sdprod partie commune à DEBUT et POURSUITE.
   (on stocke un entier au lieu du logique)
   """
   jdc.par_lot    = PAR_LOT
   jdc.impr_macro = int(IMPR_MACRO == 'OUI')
   jdc.jxveri     = int(CODE != None or (DEBUG != None and DEBUG['JXVERI'] == 'OUI'))
   jdc.sdveri     = int(DEBUG != None and DEBUG['SDVERI'] == 'OUI')
   jdc.fico       = None
   jdc.sd_checker = CheckLog()
   jdc.info_level = INFO
   if CODE != None:
      jdc.fico = CODE['NOM']
      jdc.init_ctree()
   if LANG:
       from Execution.i18n import localization
       localization.install(LANG)
   if aster_exists:
      # pb en cas d'erreur dans FIN : appeler reset_print_function dans traiter_fin_exec ?
      #from functools import partial
      #asprint = partial(aster.affiche, 'MESSAGE')
      #message.register_print_function(asprint)
      # en POURSUITE, ne pas écraser la mémorisation existante.
      if not hasattr(jdc, 'memo_sensi'):
         jdc.memo_sensi = MEMORISATION_SENSIBILITE()
      jdc.memo_sensi.reparent(jdc)
      # ne faire qu'une fois
      if not hasattr(jdc, 'msg_init'):
         # messages d'alarmes désactivés
         if IGNORE_ALARM:
            if not type(IGNORE_ALARM) in (list, tuple):
               IGNORE_ALARM = [IGNORE_ALARM]
            for idmess in IGNORE_ALARM:
               MessageLog.disable_alarm(idmess)
      # en POURSUITE, conserver le catalogue de comportement picklé
      if not hasattr(jdc, 'catalc'):
         from Comportement import catalc
         jdc.catalc = catalc
      jdc.msg_init = True


def DEBUT(self, PAR_LOT, IMPR_MACRO, CODE, DEBUG, IGNORE_ALARM, LANG, INFO, **args):
   """
       Fonction sdprod de la macro DEBUT
   """
   # La commande DEBUT ne peut exister qu'au niveau jdc
   if self.jdc is not self.parent :
      raise Accas.AsException("La commande DEBUT ne peut exister qu'au niveau jdc")
   commun_DEBUT_POURSUITE(self.jdc, PAR_LOT, IMPR_MACRO, CODE, DEBUG, IGNORE_ALARM, LANG, INFO)


def build_debut(self,**args):
   """
   Fonction ops pour la macro DEBUT
   """
   self.jdc.UserError=self.codex.error

   if self.jdc.par_lot == 'NON' :
      self.jdc._Build()
   # On execute la fonction debut pour initialiser les bases
   # Cette execution est indispensable avant toute autre action sur ASTER
   # op doit être un entier car la fonction debut appelle GCECDU qui demande
   # le numéro de l'operateur associé (getoper)
   self.definition.op=0
   self.set_icmd(1)
   self.codex.debut(self)
   # On remet op a None juste apres pour eviter que la commande DEBUT
   # ne soit executée dans la phase d'execution
   self.definition.op=None
   return 0

def POURSUITE(self, PAR_LOT, IMPR_MACRO, CODE, DEBUG, IGNORE_ALARM, LANG, INFO, **args):
   """
       Fonction sdprod de la macro POURSUITE
   """
   # La commande POURSUITE ne peut exister qu'au niveau jdc
   if self.jdc is not self.parent :
      raise Accas.AsException("La commande POURSUITE ne peut exister qu'au niveau jdc")

   commun_DEBUT_POURSUITE(self.jdc, PAR_LOT, IMPR_MACRO, CODE, DEBUG, IGNORE_ALARM, LANG, INFO)

   if (self.codex and os.path.isfile("glob.1") or os.path.isfile("bhdf.1")):
     # Le module d'execution est accessible et glob.1 est present
     # Pour eviter de rappeler plusieurs fois la sequence d'initialisation
     # on memorise avec l'attribut fichier_init que l'initialisation
     # est réalisée
     if hasattr(self,'fichier_init'):return
     self.fichier_init='glob.1'
     self.jdc.initexec()
     # le sous programme fortran appelé par self.codex.poursu demande le numéro
     # de l'operateur (GCECDU->getoper), on lui donne la valeur 0
     self.definition.op=0
     lonuti,concepts = self.codex.poursu(self)
     # Par la suite pour ne pas executer la commande pendant la phase
     # d'execution on le remet à None
     self.definition.op=None
     # On demande la numérotation de la commande POURSUITE avec l'incrément
     # lonuti pour qu'elle soit numérotée à la suite des commandes existantes.
     # self.set_icmd(lonuti)    Non : on repart à zéro
     pos=0
     d={}
     while pos+80 < len(concepts)+1:
       nomres=concepts[pos:pos+8]
       concep=concepts[pos+8:pos+24]
       nomcmd=concepts[pos+24:pos+40]
       statut=concepts[pos+40:pos+48]
       message.info(SUPERV, '%s %s %s %s', nomres, concep, nomcmd, statut)
       if nomres[0] not in (' ', '.', '&') and statut != '&DETRUIT':
          exec nomres+'='+concep.lower()+'()' in self.parent.g_context,d
       elif statut == '&DETRUIT':
          self.jdc.nsd = self.jdc.nsd+1
       pos=pos+80
     # ces ASSD seront écrasées par le pick.1,
     # on vérifiera la cohérence de type entre glob.1 et pick.1
     for k,v in d.items():
       self.parent.NommerSdprod(v,k)
     self.g_context=d

     # Il peut exister un contexte python sauvegardé sous forme  pickled
     # On récupère ces objets après la restauration des concepts pour que
     # la récupération des objets pickled soit prioritaire.
     # On vérifie que les concepts relus dans glob.1 sont bien tous
     # presents sous le même nom et du même type dans pick.1
     # Le contexte est ensuite updaté (surcharge) et donc enrichi des
     # variables qui ne sont pas des concepts.
     # On supprime du pickle_context les concepts valant None, ca peut
     # être le cas des concepts non executés, placés après FIN.
     UTMESS('I', 'SUPERVIS2_1', valk='pick.1')
     pickle_context = get_pickled_context()
     if pickle_context == None:
        UTMESS('F', 'SUPERVIS_86')
        return
     self.jdc.restore_pickled_attrs(pickle_context)
     from Cata.cata  import entier
     from Noyau.N_CO import CO
     for elem in pickle_context.keys():
         if isinstance(pickle_context[elem], ASSD):
            pickle_class = pickle_context[elem].__class__
            # on rattache chaque assd au nouveau jdc courant (en poursuite)
            pickle_context[elem].jdc = self.jdc
            pickle_context[elem].parent = self.jdc
            # le marquer comme 'executed'
            pickle_context[elem].executed = 1
            # pour que sds_dict soit cohérent avec g_context
            self.jdc.sds_dict[elem] = pickle_context[elem]
            if elem != pickle_context[elem].nom:
               name = re.sub('_([0-9]+)$', '[\\1]', pickle_context[elem].nom)
               if self.jdc.info_level > 1:
                  UTMESS('I', 'SUPERVIS2_3',
                         valk=(elem, type(pickle_context[elem]).__name__.upper()))
               UTMESS('A', 'SUPERVIS_93', valk=(elem, "del %s" % name))
               del pickle_context[elem]
               continue
            # détecte les incohérences
            if elem in self.g_context.keys():
               poursu_class = self.g_context[elem].__class__
               if poursu_class != pickle_class :
                  UTMESS('F','SUPERVIS_87',valk=[elem])
                  return
            elif pickle_class not in (CO, entier) :
            # on n'a pas trouvé le concept dans la base et sa classe est ASSD : ce n'est pas normal
            # sauf dans le cas de CO : il n'a alors pas été typé et c'est normal qu'il soit absent de la base
            # même situation pour le type 'entier' produit uniquement par DEFI_FICHIER
               UTMESS('F','SUPERVIS_88', valk=[elem,str(pickle_class)])
               return
         if pickle_context[elem]==None:
            del pickle_context[elem]
     self.g_context.update(pickle_context)
     return

   else:
     # Si le module d'execution n est pas accessible ou glob.1 absent on
     # demande un fichier (EFICAS)
     # Il faut éviter de réinterpréter le fichier à chaque appel de
     # POURSUITE
     if hasattr(self,'fichier_init'):
        return
     if aster_exists:
        UTMESS('F','SUPERVIS_89')
     self.make_poursuite()

def get_pickled_context():
    """
       Cette fonction permet de réimporter dans le contexte courant du jdc (jdc.g_context)
       les objets python qui auraient été sauvegardés, sous forme pickled, lors d'une
       précédente étude. Un fichier pick.1 doit être présent dans le répertoire de travail
    """
    fpick = 'pick.1'
    if not os.path.isfile(fpick):
       return None

    # Le fichier pick.1 est présent. On essaie de récupérer les objets python sauvegardés
    context={}
    try:
       file=open(fpick,'r')
       # Le contexte sauvegardé a été picklé en une seule fois. Il est seulement
       # possible de le récupérer en bloc. Si cette opération echoue, on ne récupère
       # aucun objet.
       context=pickle.load(file)
       file.close()
    except:
       # En cas d'erreur on ignore le contenu du fichier
       traceback.print_exc()
       return None

    return context

def POURSUITE_context(self,d):
   """
       Fonction op_init de la macro POURSUITE
   """
   # self représente la macro POURSUITE ...
   d.update(self.g_context)
   # Une commande POURSUITE n'est possible qu'au niveau le plus haut
   # On ajoute directement les concepts dans le contexte du jdc
   # XXX est ce que les concepts ne sont pas ajoutés plusieurs fois ??
   for v in self.g_context.values():
      if isinstance(v, ASSD) :
         self.jdc.sds.append(v)

def build_poursuite(self,**args):
   """
   Fonction ops pour la macro POURSUITE
   """
   # Pour POURSUITE on ne modifie pas la valeur initialisee dans ops.POURSUITE
   # Il n y a pas besoin d executer self.codex.poursu (c'est deja fait dans
   # la fonction sdprod de la commande (ops.POURSUITE))
   self.set_icmd(1)
   self.jdc.UserError = self.codex.error
   return 0

def INCLUDE(self,UNITE,**args):
   """
       Fonction sd_prod pour la macro INCLUDE
   """
   if not UNITE : return
   if hasattr(self,'unite'):return
   self.unite=UNITE

   if self.jdc and self.jdc.par_lot == 'NON':
      # On est en mode commande par commande, on appelle la methode speciale
      self.Execute_alone()

   self.make_include(unite=UNITE)

def INCLUDE_context(self,d):
   """
       Fonction op_init pour macro INCLUDE
   """
   ctxt = self.g_context
   d.update(ctxt)

def build_include(self,**args):
   """
   Fonction ops de la macro INCLUDE appelée lors de la phase de Build
   """
   # Pour presque toutes les commandes (sauf FORMULE et POURSUITE)
   # le numéro de la commande n est pas utile en phase de construction
   # La macro INCLUDE ne sera pas numérotée (incrément=None)
   ier=0
   self.set_icmd(None)
   icmd=0
   # On n'execute pas l'ops d'include en phase BUILD car il ne sert a rien.
   #ier=self.codex.opsexe(self,1)
   return ier

def build_detruire(self,d):
   """Fonction op_init de DETRUIRE."""
   list_co = set()
   sd = []
   # par nom de concept (typ=assd)
   if self["CONCEPT"] != None:
      for mc in self["CONCEPT"]:
         mcs = mc["NOM"]
         if type(mcs) not in (list, tuple):
            mcs = [mcs]
         list_co.update(mcs)
   # par chaine de caractères (typ='TXM')
   if self["OBJET"] != None:
      for mc in self["OBJET"]:
         mcs = mc["CHAINE"]
         if type(mcs) not in (list, tuple):
            mcs = [mcs]
         # longueur <= 8, on cherche les concepts existants
         for nom in mcs:
            assert type(nom) in (str, unicode), 'On attend une chaine de caractères : %s' % nom
            if len(nom.strip()) <= 8:
               if self.jdc.sds_dict.get(nom) != None:
                  list_co.add(self.jdc.sds_dict[nom])
               elif d.get(nom) != None:
                  list_co.add(d[nom])
            #else uniquement destruction des objets jeveux

   for co in list_co:
      assert isinstance(co, ASSD), 'On attend un concept : %s (type=%s)' % (co, type(co))
      nom = co.nom
      # traitement particulier pour les listes de concepts, on va mettre à None
      # le terme de l'indice demandé dans la liste :
      # nomconcept_i est supprimé, nomconcept[i]=None
      i = nom.rfind('_')
      if i > 0 and not nom.endswith('_'):
         concept_racine = nom[:i]
         if d.has_key(concept_racine) and type(d[concept_racine]) is list:
            try:
               num = int(nom[i+1:])
               d[concept_racine][num] = None
            except (ValueError, IndexError):
               # cas : RESU_aaa ou (RESU_8 avec RESU[8] non initialisé)
               pass
      # pour tous les concepts :
      if d.has_key(nom):
         del d[nom]
      if self.jdc.sds_dict.has_key(nom):
         del self.jdc.sds_dict[nom]
      #XXX/memoire suppression du concept et de sa partie SD
      #co.supprime()
      # On signale au parent que le concept n'existe plus après l'étape self
      self.parent.delete_concept_after_etape(self, co)
      # marque comme détruit == non executé
      co.executed = 0


def build_procedure(self,**args):
    """
    Fonction ops de la macro PROCEDURE appelée lors de la phase de Build
    """
    ier=0
    # Pour presque toutes les commandes (sauf FORMULE et POURSUITE)
    # le numéro de la commande n est pas utile en phase de construction
    # On ne numérote pas une macro PROCEDURE (incrément=None)
    self.set_icmd(None)
    icmd=0
    #ier=self.codex.opsexe(self,3)
    return ier

def build_DEFI_FICHIER(self,**args):
    """
    Fonction ops de la macro DEFI_FICHIER
    """
    ier=0
    self.set_icmd(1)
    icmd=0
    ier=self.codex.opsexe(self,26)
    return ier

def build_formule(self, d):
    """Fonction ops de FORMULE."""
    NOM_PARA = self.etape['NOM_PARA']
    VALE = self.etape['VALE']
    VALE_C = self.etape['VALE_C']
    if type(NOM_PARA) not in (list, tuple):
        NOM_PARA = [NOM_PARA, ]
    for para in NOM_PARA:
        if para.strip() != para:
            raise Accas.AsException("nom de paramètre invalide (contient des blancs)" \
               " : %s" % repr(para))
    if self.sd == None:
        return
    if VALE     != None :
        texte = ''.join(VALE.splitlines())
    elif VALE_C != None :
        texte = ''.join(VALE_C.splitlines())
    self.sd.setFormule(NOM_PARA, texte.strip())

def build_gene_vari_alea(self, d):
    """Fonction ops de la macro GENE_VARI_ALEA."""
    from Utilitai.Utmess import UTMESS
    a = self.etape['BORNE_INF']
    moyen = self.etape['VALE_MOY' ]
    TYPE = self.etape['TYPE']
    if self['INIT_ALEA'] is not None:
        jump = self.etape['INIT_ALEA' ]
        self.iniran(jump)
    if TYPE == 'EXP_TRONQUEE':
        b = self.etape['BORNE_SUP']
        if a >= b:
            UTMESS('F', 'PROBA0_1', valr=[a, b])
        elif moyen <= a or moyen >= b:
            UTMESS('F', 'PROBA0_2', valr=[a, moyen, b])
        k = 1. / (moyen - a)
        if exp(-b * k) < 1.e-12:
            UTMESS('F', 'PROBA0_3')
        # résolution par point fixe
        eps = 1.E-4
        nitmax = 100000
        test = 0.
        while abs((test - k) / k) > eps:
            test = k
            k = 1. / (moyen - (a * exp(-a * k) - b * exp(-b * k)) / \
                               (exp(-a * k) - exp(-b * k)))
        # génération de la variable aléatoire
        alpha = exp(-a * k) - exp(-b * k)
        self.sd.valeur = -(log(exp(-a * k) - alpha * self.getran()[0])) / k
    elif TYPE == 'EXPONENTIELLE':
       if moyen <= a:
          UTMESS('F', 'PROBA0_4', valr=[moyen, a])
       v = moyen - a
       u = self.getran()[0]
       x = -log(1 - u)
       self.sd.valeur = a + v * x
    elif TYPE == 'GAMMA':
       delta = self.etape['COEF_VAR' ]
       if moyen <= a:
          UTMESS('F', 'PROBA0_4', valr=[moyen, a])
       v = moyen - a
       alpha = 1. / delta**2
       if alpha <= 1.:
          UTMESS('F', 'PROBA0_5')
       gamma2 = alpha - 1.
       gamm1 = 1. / gamma2
       beta = sqrt(2. * alpha - 1.)
       beta2 = 1. / beta**2
       f0 = 0.5 + (1. / pi) * atan2(-gamma2 / beta, 1.)
       c1 = 1. - f0
       c2 = f0 - 0.5
       vref = 0.
       vv     = -1.
       while -vv > vref:
          u = self.getran()[0]
          gamdev = beta * tan(pi * (u * c1 + c2)) + gamma2
          unif = self.getran()[0]
          if unif < 0.:
             UTMESS('F', 'PROBA0_6')
          vv = -log(unif)
          vref = log(1 + beta2 * ((gamdev - gamma2)**2)) \
               + gamma2 * log(gamdev * gamm1) - gamdev + gamma2
       if vv <= 0.:
          UTMESS('F', 'PROBA0_7')
       self.sd.valeur = a + v * delta**2 * gamdev
