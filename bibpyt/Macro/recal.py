# coding=utf-8
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


#___________________________________________________________________________
#
#           MODULE DE CALCUL DISTRIBUE POUR MACR_RECAL
#
#  Utilisable en mode EXTERNE, voir les flags avec "python recal.py -h"
#___________________________________________________________________________


import os
import sys
import shutil
import tempfile
import glob
import copy
import re
import platform
from math import log10, sqrt

import numpy as NP


# Importation de commandes Aster
try:
   import aster_core
   import aster
   import Macro
   from Accas import _F
except ImportError:
   pass

include_pattern = "# -->INCLUDE<--"
debug = False

# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
def get_absolute_path(path):
   """Retourne le chemin absolu en suivant les liens éventuels.
   """
   if os.path.islink(path):
      path = os.path.realpath(path)
   res = os.path.normpath(os.path.abspath(path))
   return res

# -------------------------------------------------------------------------------
#if os.environ.has_key('bibpytdir'): sys.path.append( os.environ['bibpytdir'] )

# recupere "bibpyt" à partir de "bibpyt/Macro/recal.py"
sys.path.append(get_absolute_path(os.path.join(sys.argv[0], '..', '..')))

from Utilitai.Utmess import UTMESS
# try:
#    from Utilitai.Utmess import UTMESS
# except Exception, e:
#    print e
#    def UTMESS(code='I', txt='',valk='', vali='', valr=''):
#        print txt, valk, vali, valr
#        if code=='F': sys.exit()


# -------------------------------------------------------------------------------
def affiche(unity, filename, label='', filetype='stderr'):
   """ Affiche un fichier dans l'output courant (methode utilisee pour l'affichage
       du stdout et/ou du stderr
   """
   try:
       f=open(filename, 'r')
       txt = f.read()
       txt = """

============================ %s (%s) =============================


%s


======================================================================
======================================================================

""" % (label, filetype, txt)

       f.close()

       if unity:
           fw=open('fort.%s' % str(unity), 'a')
           fw.write( txt )
           fw.close()
       else:
           print txt
   except Exception, e:
       print e
   return


# # -------------------------------------------------------------------------------
# def find_parameter(content, param):
#    """
#    Return the lowest index in content where param is found and
#    the index of the end of the command.
#    """
#    pos, endpos = -1, -1
#    re_start = re.compile('^ *%s *\=' % re.escape(param), re.M)
#    mat_start = re_start.search(content)
#    if mat_start is not None:
#       pos = mat_start.start()
#       endpos = search_enclosed(content, pos)
#    return pos, endpos



# -------------------------------------------------------------------------------
def find_parameter(content, param):
    """
    Supprime les parametres du fichier de commande
    """
    re_start = re.compile('^ *%s *\=' % re.escape(param), re.M)
    l=[]
    for line in content.split('\n'):
       mat_start = re_start.search(line)
       if mat_start is None: l.append(line)
    return '\n'.join(l)


# -------------------------------------------------------------------------------
def Affiche_Param(para, val):
    """Affiche les parametres
    """
    t = []
    for p, v in zip(para, val):
        t.append( "     %s : %s" % ( p.ljust(9), v) )
    return '\n'.join(t)


# -------------------------------------------------------------------------------
def make_include_files(UNITE_INCLUDE, calcul, parametres):
   """  Module permettant de generer les fichiers a inclure (mode INCLUSION)
   """

#    # Importation de commandes Aster
#    try:
#       import aster
#       import Macro
#       from Accas import _F
#       from Cata.cata import *
#    except ImportError:
#       raise Exception("Le mode INCLUSION doit etre lance depuis Aster")

   try:
       ASTER_ROOT = os.path.join(aster_core.get_option('repout'), '..')
       sys.path.append(os.path.join(ASTER_ROOT, 'ASTK', 'ASTK_SERV', 'lib'))
       sys.path.append(os.path.join(ASTER_ROOT, 'lib', 'python%s.%s' % (sys.version_info[0], sys.version_info[1] ) , 'site-packages'))
   except: pass
   try:
       from asrun.common.utils import find_command, search_enclosed
   except Exception, e:
       print e
       UTMESS('F','RECAL0_99')


   # ----------------------------------------------------------------------------
   # Preparation des fichiers
   # ----------------------------------------------------------------------------
   liste_reponses = []
   for reponse in [ x[0] for x in calcul ]:
      if not reponse in liste_reponses: liste_reponses.append(reponse)

   try:
       old = "fort.%s"     % UNITE_INCLUDE
       pre = "fort.%s.pre" % UNITE_INCLUDE
       new = "fort.%s.new" % UNITE_INCLUDE

       # Lecture du fichier
       f=open(old, 'r')
       newtxt = f.read()
       f.close()

       # On retire la commande DEBUT
       pos, endpos = find_command(newtxt, "DEBUT")
       if endpos!=-1: newtxt = newtxt[endpos+1:]
       if newtxt[0]==';': newtxt = newtxt[1:]  # Bug dans find_command si la commande se termine par un ";"

       # On retire les parametres
       list_params = [x[0] for x in parametres]
       for param in list_params:
           newtxt = find_parameter(newtxt, param)

       # Isole la partie a inclure si elle est specifiee
       n = newtxt.find(include_pattern)
       pretxt = None
       if n!=-1:
           pretxt = newtxt[:n]
           pretxt = "# -*- coding: iso-8859-1 -*-\n" + pretxt
           # Ecriture du nouveau fichier
           fw=open(pre, 'w')
           fw.write(pretxt)
           fw.close()
           newtxt = newtxt[n+len(include_pattern):]

       # Retire la commande FIN
       pos, endpos = find_command(newtxt, "FIN")
       if pos!=-1: newtxt = newtxt[:pos]

       # Ajoute un global pour ramener les courbes dans l'espace Aster
       newtxt = "global %s\n" % ','.join(liste_reponses) + newtxt

       # Ajoute un encodage pour eviter les erreurs dues aux accents (ssna110a par exemple)
       newtxt = "# -*- coding: iso-8859-1 -*-\n" + newtxt

       # Ecriture du nouveau fichier
       fw=open(new, 'w')
       fw.write(newtxt)
       fw.close()
   except Exception, e:
       raise e

   return


# -------------------------------------------------------------------------------
def mes_concepts(list_concepts=[],base=None):
   """ Fonction qui liste les concepts créés """
   for e in base.etapes:
      if e.nom in ('INCLUDE','MACR_RECAL',) :
         list_concepts=list(mes_concepts(list_concepts=list_concepts,base=e))
      elif (e.sd != None) and (e.parent.nom=='INCLUDE') :
         nom_concept=e.sd.get_name()
         if not(nom_concept in list_concepts):
            list_concepts.append( nom_concept )
   return tuple(list_concepts)


# -------------------------------------------------------------------------------
def detr_concepts(self):
     liste_concepts=mes_concepts(base=self.parent)
     for e in liste_concepts:
        nom = string.strip(e)
        DETRUIRE( OBJET =self.g_context['_F'](CHAINE = nom), INFO=2)
        if self.jdc.g_context.has_key(nom) : del self.jdc.g_context[nom]
     del(liste_concepts)


# -------------------------------------------------------------------------------
def get_tables(tables_calc, tmp_repe_table, prof):
   """ Recupere les resultats Aster (Table Aster -> numpy)
   """
   assert (tables_calc is not None)
   assert (tmp_repe_table is not None)

   # Import du module lire_table
   if os.environ.has_key('ASTER_ROOT'):
      version = prof['version'][0]
      bibpyt = os.path.join(os.environ['ASTER_ROOT'], version, 'bibpyt')
      sys.path.append(bibpyt)
      for mdl in glob.glob(os.path.join(bibpyt, '*')):
         sys.path.append(os.path.join(os.environ['ASTER_ROOT'], version, 'bibpyt', mdl))
   try:
      from Utilitai.TableReader import TableReaderFactory
   except:
      UTMESS('F','RECAL0_23')

   reponses = tables_calc
   Lrep=[]
   for i in range(len(reponses)):
      _fic_table = tmp_repe_table + os.sep + "fort."+str(int(100+i))

      try:
         f=open(_fic_table,'r')
         texte=f.read()
         f.close()
      except Exception, err:
         ier=1
         UTMESS('F','RECAL0_24',valk=str(err))

      try:
         reader = TableReaderFactory(texte, 'ASTER', ' ')
         table_lue = reader.read(1)
         list_para = table_lue.para
         tab_lue   = table_lue.values()
      except Exception, err:
         ier=1
      else:
         ier=0

      if ier!=0 : UTMESS('F','RECAL0_24',valk=str(err))

      F = table2numpy(tab_lue, list_para, reponses, i)
      Lrep.append(F)


   return Lrep


# --------------------------------------------------------------------------------------------------
def table2numpy(tab_lue, list_para, reponses, i):
   """  Extraction des resultats depuis la table Aster
   """
   try:
       nb_val = len(tab_lue[ list_para[0] ])
       F = NP.zeros((nb_val,2))
       for k in range(nb_val):
         F[k][0] = tab_lue[ str(reponses[i][1]) ][k]
         F[k][1] = tab_lue[ str(reponses[i][2]) ][k]
   except Exception, err:
       UTMESS('F','RECAL0_24',valk=str(err))
   return F


# --------------------------------------------------------------------------------------------------
def Ecriture_Fonctionnelle(output_file, type_fonctionnelle, fonctionnelle):

   try:    os.remove(output_file)
   except: pass

   f=open(output_file, 'w')
   if type_fonctionnelle == 'vector':
      try:    fonctionnelle = fonctionnelle.tolist()
      except: pass
      fonctionnelle = str(fonctionnelle).replace('[','').replace(']','').replace('\n', ' ')
   f.write( str(fonctionnelle) )
   f.close()


# --------------------------------------------------------------------------------------------------
def Ecriture_Derivees(output_file, derivees):

   try:    os.remove(output_file)
   except: pass

   # on cherche a imprimer la gradient calcule a partir de Fcalc
   if isinstance(derivees, (list, tuple)):
       t = []
       for l in derivees:
          l = str(l).replace('[', '').replace(']', '')
          t.append( l )
       txt = '\n'.join(t)

   # On cherche a imprimer la matrice des sensibilite (A ou A_nodim)
   elif isinstance(derivees, NP.ndarray):
       t = []
       a = derivees
       for c in range(len(a[0,:])):
           l = a[:,c].tolist()
           l = str(l).replace('[', '').replace(']', '')
           t.append( l )
       txt = '\n'.join(t)

   else: raise Exception("Wrong type for gradient !")

   # Ecriture
   f=open(output_file, 'w')
   f.write(txt)
   f.close()



# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
class CALCULS_ASTER:
   """
      Classe gérant les calculs Aster (distribues ou include)
   """

   # ---------------------------------------------------------------------------
   def __init__(self,

       # MACR_RECAL inputs are optional here (if passed to self.run methods)
       parametres          = None,
       calcul              = None,
       experience          = None,
       LANCEMENT        = 'DISTRIBUTION',
       jdc                 = None,
               ):

       self.parametres         = parametres
       self.calcul             = calcul
       self.experience         = experience
       #self.eval_esclave       = mode_esclave
       self.LANCEMENT       = LANCEMENT
       self.UNITE_ESCL         = None
       self.UNITE_INCLUDE      = None
       self.ASTER_ROOT         = None

       self.jdc                = jdc

       self.follow_output      = False
       self.unity_follow       = None


       self.list_params        = [x[0] for x in parametres]
       self.list_params.sort()

       # Valable uniquement pour le mode INCLUDE
       self.pre    = None
       self.pretxt = None
       self.new    = None
       self.newtxt = None

       # Mode dynamique desactive par defaut
       self.SetDynamiqueMode(None, None)


   # ---------------------------------------------------------------------------------------------------------
   def SetDynamiqueMode(self, DYNAMIQUE, graph_mac):
       self.DYNAMIQUE = DYNAMIQUE
       self.graph_mac = graph_mac


   # ---------------------------------------------------------------------------------------------------------
   # ---------------------------------------------------------------------------------------------------------
   def run(self,

                # Current estimation
                X0,
                dX             = None,

                # Code_Aster installation
                ASTER_ROOT     = None,
                as_run         = None,

                # General
                resudir        = None,
                clean          = True,
                info           = None,
                NMAX_SIMULT   = None,

                # Study
                export         = None,

                # MACR_RECAL inputs
                parametres     = None,
                calcul         = None,
                experience     = None,
        ):

        # Current estimation
        self.X0             = X0
        self.dX             = dX

        # Code_Aster installation
        self.ASTER_ROOT     = ASTER_ROOT
        self.as_run         = as_run

        # General
        self.resudir        = resudir
        self.clean          = clean
        self.info           = info
        if not NMAX_SIMULT: NMAX_SIMULT = 0
        self.NMAX_SIMULT   = NMAX_SIMULT

        # Study
        self.export         = export

        # MACR_RECAL inputs
        if parametres:   self.parametres     = parametres
        if calcul:       self.calcul         = calcul
        if experience:   self.experience     = experience

        parametres  = self.parametres
        calcul      = self.calcul
        experience  = self.experience

        list_params = self.list_params

        if dX: CalcGradient = True
        else:  CalcGradient = False
        self.CalcGradient   = CalcGradient

        self.list_diag      = []

        # Pour le moment on conserve un seul fichier
        self.UNITE_INCLUDE  = self.UNITE_ESCL


        # ----------------------------------------------------------------------------
        # Liste de tous les jeux de parametres (initial + differences finies)
        # ----------------------------------------------------------------------------
        list_val = []

        # Dictionnaire des parametres du point courant
        dic = dict( zip( list_params, X0 ) )
        list_val.append( dic )

        # Calcul du gradient (perturbations des differences finies)
        if CalcGradient:
            UTMESS('I','RECAL0_16')
            # Dictionnaires des parametres des calculs esclaves
            for n in range(1,len(dX)+1):
               l = [0] * len(dX)
               l[n-1] = dX[n-1]
#               X = [ X0[i] * (1+l[i]) for i in range(len(dX)) ]
               X = []
               for i in range(len(dX)):
                  new_Xi = X0[i] * (1+l[i])
                  if new_Xi > self.parametres[i][3]:
                     UTMESS('I', 'RECAL0_75', valk=( str(self.parametres[i][0]), str(new_Xi), str(self.parametres[i][2]), str(self.parametres[i][3]), str(l[i]) ) )
#                     new_Xi = X0[i] * (1-l[i])  # diff finie a gauche marche pas fort
                  X.append( new_Xi )
               #print 'X=', X
               dic = dict( zip( list_params, X ) )
               list_val.append( dic )

        # ----------------------------------------------------------------------------
        # Aiguillage vers INCLUDE
        # ----------------------------------------------------------------------------
        if self.LANCEMENT == 'INCLUSION':
           UTMESS('I','RECAL0_29', valk=self.LANCEMENT)
           fonctionnelle, gradient = self.run_include(list_val)


        # ----------------------------------------------------------------------------
        # Aiguillage vers ASRUN distribue
        # ----------------------------------------------------------------------------
        elif self.LANCEMENT == 'DISTRIBUTION':
           UTMESS('I','RECAL0_29', valk=self.LANCEMENT)
           fonctionnelle, gradient = self.run_distrib(list_val)


        # ----------------------------------------------------------------------------
        # Erreur d'aiguillage
        # ----------------------------------------------------------------------------
        else:
           raise Exception("Erreur : mode %s inconnu!" % self.LANCEMENT)


        #sys.exit()
        # ----------------------------------------------------------------------------
        # Sortie
        # ----------------------------------------------------------------------------
        return fonctionnelle, gradient



   # ---------------------------------------------------------------------------------------------------------
   # ---------------------------------------------------------------------------------------------------------
   def run_include(self,list_val):
     """  Module permettant de lancer N+1 calculs via un mecanisme d'include
     """

     try:
         import aster
         import Macro
         from Cata import cata
         from Cata.cata import OPER, MACRO
         from Accas import _F

         # Declaration de toutes les commandes Aster
         import cata
         from Cata.cata import *
     except Exception, e:
         raise Exception("Le mode INCLUDE doit etre lance depuis Aster : \nErreur : %s" % e)


     list_params = self.list_params
     calcul      = self.calcul
     reponses    = self.calcul

# AA : marche pas car on peut oublier des courbes, tant pis on refait des extract en trop..
#      liste_reponses = []
#      for reponse in [ x[0] for x in calcul ]:
#          if not reponse in liste_reponses: liste_reponses.append(reponse)

     liste_reponses = [ x[0] for x in calcul ]


     # ----------------------------------------------------------------------------
     # Boucle sur les N+1 calculs
     # ----------------------------------------------------------------------------
     Lcalc = []
     for i in range(len(list_val)):
         params = list_val[i]


         # ----------------------------------------------------------------------------
         # Affectation des valeurs des parametres
         # ----------------------------------------------------------------------------
         for nompara in list_params:
             valpara = params[nompara]
             exec( "%s=%s" % (nompara, valpara) )    #  YOUN__ = X0[0], DSDE__ = X0[1], ...


         # ----------------------------------------------------------------------------
         # Affichage des parametres du calcul courant
         # ----------------------------------------------------------------------------
         tpara = Affiche_Param(list_params, [ params[x] for x in list_params] )
         if i==0:  UTMESS('I', 'RECAL0_67', valk=tpara)
         else:     UTMESS('I', 'RECAL0_68', valk=(tpara, list_params[i-1]) )


         # ----------------------------------------------------------------------------
         # Lancement du calcul (par un include)
         # ----------------------------------------------------------------------------
         new = "fort.%s.new" % self.UNITE_INCLUDE
         try:
            execfile(new)
         except Exception, e:
            UTMESS('F', 'RECAL0_85', valk=str(e))


         # ----------------------------------------------------------------------------
         # On considere que le job est OK s'il ne s'est pas plante dans le except precedent..
         # ----------------------------------------------------------------------------
         self.list_diag.append("OK")


         # ----------------------------------------------------------------------------
         # Extraction des tables
         # ----------------------------------------------------------------------------
         Lrep=[]
         for i in range(len(liste_reponses)):
             reponse = liste_reponses[i]
             DETRUIRE(OBJET=_F(CHAINE='VEXTR___'), INFO=1)  # Faudrait proteger ce nom ici (VEXTR___ peut etre deja utilise dans l'etude)
             exec( "VEXTR___ = %s.EXTR_TABLE()" % reponse)
             list_para = VEXTR___.para
             tab_lue   = VEXTR___.values()
             F = table2numpy(tab_lue, list_para, reponses, i)
             Lrep.append(F)



         Lcalc.append( Lrep )


         # Destruction des concepts Aster
         liste_concepts = self.jdc.g_context.keys()
         for c in liste_concepts:
             DETRUIRE(OBJET=_F(CHAINE=c), INFO=1);

         #detr_concepts(self.jdc)  # marche pas !
         #sys.exit()


     # ----------------------------------------------------------------------------
     # Calcul de la fonctionnelle et du gradient
     # ----------------------------------------------------------------------------
     if debug: print "AA4/Lcalc=", Lcalc
     fonctionnelle, gradient = self.calc2fonc_gradient(Lcalc)


     # ----------------------------------------------------------------------------
     # Save all calculated responses
     self.Lcalc = Lcalc
     # ----------------------------------------------------------------------------


     return fonctionnelle, gradient






   # ---------------------------------------------------------------------------------------------------------
   # ---------------------------------------------------------------------------------------------------------
   def run_distrib(self, list_val):
        """ Module permettant de lancer N+1 calculs avec le module de calculs distribues d'asrun
        """

        # ----------------------------------------------------------------------------
        # Parametres
        # ----------------------------------------------------------------------------

        # Code_Aster installation
        ASTER_ROOT     = self.ASTER_ROOT
        as_run         = self.as_run

        # General
        resudir        = self.resudir
        clean          = self.clean
        info           = self.info

        # Study
        export         = self.export

        # MACR_RECAL inputs
        parametres     = self.parametres
        calcul         = self.calcul
        experience     = self.experience

        parametres     = self.parametres
        calcul         = self.calcul
        experience     = self.experience

        CalcGradient   = self.CalcGradient
        NMAX_SIMULT   = self.NMAX_SIMULT


        # ----------------------------------------------------------------------------
        # Import des modules python d'ASTK
        # ----------------------------------------------------------------------------
        if not ASTER_ROOT:
            try:    ASTER_ROOT = os.path.join(aster_core.get_option('repout'), '..')
            except: pass
        try:
            sys.path.append(os.path.join(ASTER_ROOT, 'ASTK', 'ASTK_SERV', 'lib'))
            sys.path.append(os.path.join(ASTER_ROOT, 'lib', 'python%s.%s' % (sys.version_info[0], sys.version_info[1] ) , 'site-packages'))
        except: pass
        try:
            from asrun.run          import AsRunFactory
            from asrun.profil       import AsterProfil
            from asrun.repart       import get_hostrc
            from asrun.parametric   import is_list_of_dict
            from asrun.thread       import Dispatcher
            from asrun.distrib      import DistribParametricTask
        except Exception, e:
            print e
            UTMESS('F','RECAL0_99')


        assert is_list_of_dict(list_val)
        nbval = len(list_val)

        # ----------------------------------------------------------------------------
        # Generation des etudes esclaves
        # ----------------------------------------------------------------------------
        sys.argv = ['']
        run = AsRunFactory()
        #if info<=2: run.options['debug_stderr'] = False  # pas d'output d'executions des esclaves dans l'output maitre
        if self.unity_follow and info==2: run.options['debug_stderr'] = True
        else:                             run.options['debug_stderr'] = False  # pas d'output d'executions des esclaves dans l'output maitre

        # Master profile
        prof = AsterProfil(filename=export)
        tmp_param = tempfile.mkdtemp()
        try:    username = prof.param['username'][0]
        except: username = os.environ['LOGNAME']
        try:    noeud    = prof.param['noeud'][0]
        except: noeud    = platform.uname()[1]
        tmp_param = "%s@%s:%s" % ( username, noeud, tmp_param)
        prof.Set('R', {'type' : 'repe', 'isrep' : True, 'ul' : 0, 'compr' : False, 'path' : tmp_param })
        if info>=2: print prof

        # Si batch n'est pas possible, on bascule en interactif
        if prof.param['mode'][0]=='batch' and run.get('batch')=='non':
           UTMESS('I','RECAL0_28',valk=noeud)
           prof.param['mode'][0] = 'interactif'

        # result directories
        if resudir:
            if not os.path.isdir(resudir):
                try:    os.mkdir(resudir)
                except:
                    if info>=1: UTMESS('A','RECAL0_82',valk=resudir)
                    resudir = None
        if not resudir:
            # Par defaut, dans un sous-repertoire du repertoire d'execution
            shared_tmp=None
            pref = 'tmp_macr_recal_'
            # On cherche s'il y a un fichier hostfile pour placer les fichiers dans un repertoire partage
            l_fr = getattr(prof, 'data')
            l_tmp = l_fr[:]
            for dico in l_tmp:
               if dico['type']=='hostfile':
                  shared_tmp = run.get('shared_tmp')
                  if not shared_tmp: shared_tmp = os.path.join( os.environ['HOME'], 'tmp_macr_recal')
                  pref = shared_tmp + os.sep + 'tmp_macr_recal_'
                  break
            # Si batch alors on place les fichiers dans un repertoire partage
            if prof['mode'][0]=='batch':
                  shared_tmp = run.get('shared_tmp')
                  if not shared_tmp: shared_tmp = os.path.join( os.environ['HOME'], 'tmp_macr_recal')
                  pref = shared_tmp + os.sep + 'tmp_macr_recal1_'

            # Creation du repertoire temporaire racine de macr_recal
            if shared_tmp:
               if not os.path.isdir(shared_tmp):
                   try:    os.mkdir(shared_tmp)
                   except:
                      if info>=1: UTMESS('F','RECAL0_82',valk=shared_tmp)

            resudir = tempfile.mkdtemp(prefix=pref)
        flashdir = os.path.join(resudir,'flash')
        if info>=1: UTMESS('I','RECAL0_81',valk=resudir)

        prof.WriteExportTo( os.path.join(resudir, 'master.export') )

        # get hostrc object
        hostrc = get_hostrc(run, prof)

        # timeout before rejected a job
        timeout = prof.get_timeout()


        # Ajout des impressions de tables a la fin du .comm
        t = []
        reponses = calcul
        for i in range(len(reponses)):
            _ul = str(int(100+i))
            num_ul = '99'

            # Pour la dynamique la table avec la matrice MAC a un traitement different
            if self.DYNAMIQUE:

               if ('MAC' in reponses[i][2]):
                       t.append( self.ajout_post_mac( reponses[i] ) )

            try:    os.remove( 'tmp_macr_recal'+os.sep+"REPE_TABLE"+os.sep+"fort."+_ul )
            except: pass

            t.append("\n# Recuperation de la table : " + str(reponses[i][0]) + "\n")
            t.append("DEFI_FICHIER(UNITE=" + num_ul + ", FICHIER='" + os.path.join('.', 'REPE_OUT', 'fort.'+_ul) + "',);\n" )
            t.append("IMPR_TABLE(TABLE="+str(reponses[i][0])+", FORMAT='ASTER', UNITE="+num_ul+", INFO=1, FORMAT_R='E30.20',);\n")
            t.append("DEFI_FICHIER(ACTION='LIBERER', UNITE="+num_ul+",);\n")

        # Pour la dynamique uniquement
        if self.DYNAMIQUE:
            if ( self.DYNAMIQUE['APPARIEMENT_MANUEL'] == 'OUI' and self.graph_mac and (True in ['MAC' in reponses[ii][2] for ii in range (len(reponses))]) ) : ## on cherche a inverser la liste de frequences potentiellement changee par la fenetre MAC 
                for ind_rep in range(len(reponses)) :
                    if reponses[ind_rep][2]=='FREQ' :
                         t.append( "data1 = "+reponses[ind_rep][0]+".EXTR_TABLE().Array('"+reponses[ind_rep][1] +"','FREQ')\n" ) ## on recupere la table des frequences
                         t.append( "nume_freq=data1[:,0].tolist()\n" )
                         t.append( "val_freq=data1[:,1].tolist()\n" )
                         t.append( "val_freq_permute=[]\n" ) ## val_freq_permute contient la liste de frequences permutee
      
                         t.append( "for ii in range(len(list_num_pour_freq)):\n" )
                         t.append( "   if list_num_pour_freq[ii]==list_exp_pour_freq[ii]:\n" )
                         t.append( "     val_freq_permute.append(val_freq[ii])\n" )
                         t.append( "   else:\n" )
                         t.append( "     ii_p= list_exp_pour_freq.index(list_num_pour_freq[ii])\n" )
                         t.append( "     val_freq_permute.append(val_freq[ii_p])\n" )
      
                         t.append( "DETRUIRE(CONCEPT=_F(NOM="+str(reponses[ind_rep][0])+"),)\n" )
                         t.append( reponses[ind_rep][0]+"=CREA_TABLE(LISTE=(_F(PARA='"+reponses[ind_rep][1]+"',LISTE_I=nume_freq,),_F(PARA='FREQ',LISTE_R=val_freq_permute,),),)\n" )

        # number of threads to follow execution
        numthread = 1


        # ----------------------------------------------------------------------------
        # Executions des etudes esclaves
        # ----------------------------------------------------------------------------
        # ----- Execute calcutions in parallel using a Dispatcher object
        # elementary task...
        task = DistribParametricTask(run=run, prof=prof, # IN
                                     hostrc=hostrc,
                                     nbmaxitem=self.NMAX_SIMULT, timeout=timeout,
                                     resudir=resudir, flashdir=flashdir,
                                     keywords={'POST_CALCUL': '\n'.join(t)},
                                     info=info,
                                     nbnook=[0,]*numthread, exec_result=[])            # OUT
        # ... and dispatch task on 'list_tests'
        etiq = 'calc_%%0%dd' % (int(log10(nbval)) + 1)
        labels = [etiq % (i+1) for i in range(nbval)]
        couples = zip(labels, list_val)

        if info>=2: print couples
        execution = Dispatcher(couples, task, numthread=numthread)

        # ----------------------------------------------------------------------------
        # Liste des diagnostics
        # ----------------------------------------------------------------------------
        d_diag = {}


        for result in task.exec_result:
            label = result[0]
            diag  = result[2]
            if len(result) >= 8: output_filename = os.path.join('~', 'flasheur', str(result[7]))
            else:                output_filename = ''
            d_diag[label] = diag

            # Affichage de l'output de l'esclave dans l'output du maitre
            if self.unity_follow:
                affiche(unity=self.unity_follow, filename=output_filename, label=label, filetype='stdout')

            # Calcul esclave NOOK
            if not diag[0:2] in ['OK', '<A']:

              # Affichage de l'output et/ou de l'error de l'esclave dans l'output du maitre
              try:
                  affiche(unity=None, filename=output_filename, label=label, filetype='stdout')
                  error_filename = '.'.join(output_filename.split('.')[0:-1]) + '.e' + output_filename.split('.')[-1][1:]
                  affiche(unity=None, filename=error_filename, label=label, filetype='stderr')
              except Exception, e:
                  print e

              if diag in ['<F>_NOT_RUN', '<A>_NOT_SUBMITTED']:
                  UTMESS('F', 'RECAL0_86', valk=(label, diag))
              else:
                  UTMESS('A', 'RECAL0_83', valk=(label, output_filename))


        if not d_diag:
            UTMESS('F', 'RECAL0_84', valk=resudir)
        self.list_diag = [ d_diag[label] for label in labels ]

        # ----------------------------------------------------------------------------
        # Arret si tous les jobs ne se sont pas deroules correctement
        # ----------------------------------------------------------------------------
        if sum(task.nbnook) > 0:
           UTMESS('F', 'RECAL0_84', valk=resudir)



        # ----------------------------------------------------------------------------
        # Recuperation des tables calculees
        # ----------------------------------------------------------------------------
        Lcalc = []
        i=0
        for c in labels:
            tbl = get_tables(tables_calc=calcul, tmp_repe_table=os.path.join(resudir, c, 'REPE_OUT'), prof=prof)
            Lcalc.append( tbl )  # On stocke sous la forme d'une liste de numpy
            i+=1


        # ----------------------------------------------------------------------------
        # Calcul de la fonctionnelle et du gradient
        # ----------------------------------------------------------------------------
        if debug: print "AA4/Lcalc=", Lcalc
        fonctionnelle, gradient = self.calc2fonc_gradient(Lcalc)


        # ----------------------------------------------------------------------------
        # Clean result directories
        # ----------------------------------------------------------------------------
        if clean: shutil.rmtree(resudir, ignore_errors=True)


        # ----------------------------------------------------------------------------
        # Save all calculated responses
        # ----------------------------------------------------------------------------
        self.Lcalc = Lcalc

        return fonctionnelle, gradient


   # ---------------------------------------------------------------------------------------------------------
   # ---------------------------------------------------------------------------
   def calc2fonc_gradient(self, Lcalc):
        """  Calculs de la fonctionnelle et du gradient a partir des tables calculees
        """

        #print "AA1/Lcalc=", Lcalc

        info         = self.info
        CalcGradient = self.CalcGradient

        # ----------------------------------------------------------------------------
        # Recuperation des tables calculees
        # ----------------------------------------------------------------------------
        seq_FX   = []
        seq_FY   = []
        seq_DIMS = []
        lst_iter = []
        for i in range(len(Lcalc)):
            tbl = Lcalc[i]
            FX = []
            FY = []
            ldims = []
            for array in tbl:
                 FX.extend([ x[0] for x in array ])
                 FY.extend([ x[1] for x in array ])
                 ldims.append(len(array))
            # Agregation des resultats
            seq_FX.append(FX)
            seq_FY.append(FY)
            seq_DIMS.append(ldims)
            lst_iter.append(i)


        # ----------------------------------------------------------------------------
        # Fonctionnelle
        # ----------------------------------------------------------------------------
        # Calcul maitre (point X0)
        idx0 = lst_iter.index(0)   # index (les calculs arrivent-ils dans le desordre?)
        FY_X0 = seq_FY[idx0]
        fonctionnelle = FY_X0


        # ----------------------------------------------------------------------------
        # Procedure d'assemblage du gradient (une liste de liste)
        # ----------------------------------------------------------------------------
        gradient = []
        if CalcGradient:
            for n in range(len(lst_iter))[1:]:
                idx = lst_iter.index(n)
                FY   = seq_FY[idx]
                col = [ (y-x) for x, y in zip(FY, FY_X0) ]
                gradient.append(col)
                #print 'Calcul numero: %s - Diagnostic: %s' % (n, self.list_diag[idx])
                if info>=1: UTMESS('I', 'RECAL0_74', valk=(str(n), self.list_diag[idx]) )

        # ----------------------------------------------------------------------------
        # Affichages
        # ----------------------------------------------------------------------------
        if info>=2:
            UTMESS('I', 'RECAL0_72', valk=str(fonctionnelle))
            import pprint
            if CalcGradient:
                UTMESS('I', 'RECAL0_73')
                pprint.pprint(gradient)

        return fonctionnelle, gradient


   # ---------------------------------------------------------------------------------------------------------
   # ---------------------------------------------------------------------------
   def find_parameter0(self, content, param):
       """
       Return the lowest index in content where param is found and
       the index of the end of the command.
       """
       if not self.ASTER_ROOT:
           try:    ASTER_ROOT = os.path.join(aster_core.get_option('repout'), '..')
           except: pass
       try:
           sys.path.append(os.path.join(ASTER_ROOT, 'ASTK', 'ASTK_SERV', 'lib'))
           sys.path.append(os.path.join(ASTER_ROOT, 'lib', 'python%s.%s' % (sys.version_info[0], sys.version_info[1] ) , 'site-packages'))
       except: pass
       try:
           from asrun.utils        import search_enclosed
       except Exception, e:
           print e
           UTMESS('F','RECAL0_99')

       pos, endpos = -1, -1
       re_start = re.compile('^ *%s *\=' % re.escape(param), re.M)
       mat_start = re_start.search(content)
       if mat_start is not None:
          pos = mat_start.start()
          endpos = search_enclosed(content, pos)
       return pos, endpos


   # ---------------------------------------------------------------------------------------------------------
   # ---------------------------------------------------------------------------
   def find_parameter(self, content, param):
       """
       Supprime les parametres du fichier de commande
       """
       re_start = re.compile('^ *%s *\=' % re.escape(param), re.M)
       l=[]
       for line in content.split('\n'):
          mat_start = re_start.search(line)
          if mat_start is None: l.append(line)
       return '\n'.join(l)


   # ---------------------------------------------------------------------------------------------------------
   # ---------------------------------------------------------------------------
   def ajout_post_mac(self, reponse):
      """
         Ajoute un bloc a la fin de l'esclave pour l'affichage des MAC pour l'appariement manuel
      """
      txt = []
      txt.append( "from Macro.reca_mac import extract_mac_array, get_modes, fenetre_mac\n" )
      txt.append( "_mac = extract_mac_array("+str(reponse[0])+")\n" )
      txt.append( "l_mac=[]\n" )
      txt.append( "nb_freq=_mac.shape[1]\n" )
      if (self.DYNAMIQUE['APPARIEMENT_MANUEL']=='OUI' and self.graph_mac):
          txt.append( "frame =fenetre_mac(" + self.DYNAMIQUE['MODE_EXP']+"," + self.DYNAMIQUE['MODE_CALC']+",_mac)\n" )
          txt.append( "list_exp,list_num =frame.get_list()\n" )
          txt.append( "list_exp_pour_freq=list_exp" ) ### on duplique les valeurs pour etre reutilisees dans la table des FREQ
          txt.append( "list_num_pour_freq=list_num" ) ### on duplique les valeurs pour etre reutilisees dans la table des FREQ
          txt.append( "for i in range(nb_freq): l_mac.append(_mac[int(list_num[i])-1,int(list_exp[i])-1])\n" )
      else:
          txt.append( "for i in range(nb_freq): l_mac.append(_mac[i,i])\n" )
      txt.append( "DETRUIRE(CONCEPT=_F(NOM="+str(reponse[0])+"),)\n" )
      txt.append( str(reponse[0]) + "=CREA_TABLE(LISTE=(_F(PARA='NUME_ORDRE',LISTE_I=range(1,nb_freq+1),),_F(PARA='MAC',LISTE_R=l_mac,),),)\n" )
      return '\n'.join(txt)


# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
class CALC_ERROR:
   """
      Classe gérant l'erreur par rapport aux donnees experimentales, la matrice des sensibilites
   """
   # ---------------------------------------------------------------------------
   def __init__(self, experience, X0, calcul, poids=None, objective_type='vector', info=0, unite_resu=None):

       if poids is None:
            poids = NP.ones(len(experience))
       self.experience     = experience
       self.X0             = X0
       self.calcul         = calcul
       self.poids          = poids
       self.objective_type = objective_type
       self.INFO           = info
       self.unite_resu     = unite_resu

       from Macro import reca_interp, reca_algo
       self.test_convergence   = reca_algo.test_convergence
       self.calcul_gradient    = reca_algo.calcul_gradient
       self.Simul              = reca_interp.Sim_exp(self.experience, self.poids)
       try:    self.Dim   = reca_algo.Dimension(copy.copy(self.X0))
       except: self.Dim   = reca_algo.Dimension(copy.copy(self.X0), None)  # gere l'ancienne version de MACR_RECAL
       #self.Dim   = reca_algo.Dimension(copy.copy(self.X0))

       self.F               = None
       self.L_J_init        = None
       self.L_J             = None
       self.J_init          = None
       self.J               = None
       self.L_init          = None
       self.erreur          = None
       self.norme           = None
       self.A               = None
       self.A_nodim         = None
       self.norme_A_        = None
       self.norme_A_nodim   = None

       if info>=3: self.debug = True
       else:       self.debug = False
       if debug: self.debug = True


   # ---------------------------------------------------------------------------
   def CalcError(self, Lcalc):

       self.F = Lcalc[0]
       if self.L_init is None:    self.L_init   = copy.copy(self.F)

       self.L_J, self.erreur = self.Simul.multi_interpole(self.F, self.calcul)
       if self.L_J_init is None:  self.L_J_init = copy.copy(self.L_J)

       self.J = self.Simul.norme_J( copy.copy(self.L_J_init), copy.copy(self.L_J) )
       if self.J_init is None:      self.J_init   = copy.copy(self.J)

       # norme de l'erreur
       self.norme = NP.sum( [x**2 for x in self.erreur] )

       if self.debug:
           print "AA1/F=", self.F
           print "AA1/calcul=", self.calcul
           print "AA1/L_J=", self.L_J
           print "AA1/erreur=", self.erreur
           print "AA1/L_J_init=", self.L_J_init
           print "AA1/J=", self.J
           print "AA1/norme de l'erreur=", self.norme
           print "AA1/norme de J (fonctionnelle)=", str(self.J)

       if self.INFO>=1:
           UTMESS('I', 'RECAL0_30')

       if self.objective_type=='vector':
           if self.INFO>=1: UTMESS('I', 'RECAL0_35', valr=self.norme)
           return self.erreur
       else:
           if self.INFO>=1: UTMESS('I', 'RECAL0_36', valr=self.norme)
           return self.norme


   # ---------------------------------------------------------------------------
   def CalcSensibilityMatrix(self, Lcalc, val, dX=None, pas=None):

      """
         Calcul de F(X0) et de tous les F(X0+h)
         Formation de la matrice des sensibilites A
         N+1 calculs distribues
      """

      if not dX and not pas:
         raise Exception("Need 'dX' or 'pas' parameter.")
      if dX and pas:
         raise Exception("Need 'dX' or 'pas' parameter, not both.")
      if pas: dX = len(val)*[pas]
      if len(dX) != len(val):
         raise Exception("Error : 'dX' and 'val' parameters aren't compatible (lenght are not equal).\ndX = %s\nval = %s" % (dx, val))

      reponses  = self.calcul
      resu_exp  = self.experience
      len_para  = len(val)  # initialement len(self.para)


      # Erreur de l'interpolation de F_interp : valeur de F interpolée sur les valeurs experimentales
      F = Lcalc[0]
      F_interp = self.Simul.multi_interpole_sensib(F, reponses)  #F_interp est une liste contenant des tab num des reponses interpolés


      # Creation de la liste des matrices de sensibilités
      L_A=[]
      for i in range(len(reponses)):
          L_A.append(NP.zeros((len(resu_exp[i]),len(val))) )

      for k in range(len(val)):   # pour une colone de A (dim = nb parametres)

          F_perturbe = Lcalc[k+1]

          # Erreur de l'interpolation de F_perturb : valeur de F (perturbée) interpolée sur les valeurs experimentales
          F_perturbe_interp = self.Simul.multi_interpole_sensib(F_perturbe, reponses)

          # Calcul de L_A (matrice sensibilité des erreurs sur F interpolée)
          h = val[k]*dX[k]
          for j in range(len(reponses)):
             for i in range(len(resu_exp[j])):
                if NP.all(h != 0.):
                   L_A[j][i,k] = -1*(F_interp[j][i] - F_perturbe_interp[j][i])/h
                else:
                   if self.unite_resu:
                       fic=open(os.getcwd()+'/fort.'+str(unite_resu),'a')
                       fic.write('\n Probleme de division par zéro dans le calcul de la matrice de sensiblité')
                       fic.write('\n Le parametre '+para[k]+'est nul ou plus petit que la précision machine')
                       fic.close()
                   UTMESS('F','RECAL0_45',valk=para[k])
                   return

      # On construit la matrice de sensiblité sous forme d'un tab num
      dim =[]
      for i in range(len(L_A)):
         dim.append(len(L_A[i]))
      dim_totale = NP.sum(dim)
      a=0
      self.A_nodim = NP.zeros((dim_totale,len(val)))
      for n in range(len(L_A)):
         for k in range(len(val)):
            for i in range(dim[n]):
               self.A_nodim[i+a][k] = L_A[n][i,k]
         a=dim[n]

      del(L_A)


      self.A = self.Dim.adim_sensi( copy.copy(self.A_nodim) )

      # Si on n'est pas encore passe par CalcError...
      if self.erreur is None:
          self.erreur = self.CalcError(Lcalc)
      self.gradient_init = self.calcul_gradient(self.A, self.erreur)  #utile pour le test de convergence, on prend les valeurs dimensionnées
      self.residu = self.test_convergence(self.gradient_init, self.erreur, self.A, NP.zeros(len(self.gradient_init)))

      if self.debug:
          print "AA1/erreur=", self.erreur
          print "AA1/residu=", self.residu
          print "AA1/A_nodim=", self.A_nodim
          print "AA1/A=", self.A


      if self.objective_type=='vector':
          return self.erreur, self.residu, self.A_nodim, self.A
      else:
          # norme de l'erreur
          self.norme = NP.dot(self.erreur, self.erreur)**0.5
          self.norme_A_nodim = NP.zeros( (1,len_para))
          self.norme_A       = NP.zeros( (1,len_para))
          for c in range(len(self.A[0,:])):
              norme_A_nodim = 0
              norme_A       = 0
              for l in range(len(self.A[:,0])):
                   norme_A_nodim += self.A_nodim[l,c] * self.A_nodim[l,c]
                   norme_A       += self.A[l,c] * self.A[l,c]
              self.norme_A_nodim[0,c] = sqrt( norme_A_nodim )
              self.norme_A[0,c] = sqrt( norme_A )
          if self.debug:
              print "AA1/norme_A_nodim=", self.norme_A_nodim
              print "AA1/norme_A=", self.norme_A
          return self.erreur, self.residu, self.norme_A_nodim, self.norme_A






# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
if __name__ == '__main__':

    # Execution via YACS ou en externe
    isFromYacs = globals().get('ASTER_ROOT', None)


    # ------------------------------------------------------------------------------------------------------------------
    #                               Execution depuis YACS
    # ------------------------------------------------------------------------------------------------------------------
    if isFromYacs:
        # Execution depuis YACS : les parametres sont deja charges en memoire

        # ----------------------------------------------------------------------------
        # Parametres courant
        X0 = globals().get('X0', [ 80000.,  1000., 30. ])
        dX = globals().get('dX', [ 0.001, 0.001, 0.0001])
        # ----------------------------------------------------------------------------

        # ----------------------------------------------------------------------------
        # Parametres
        os.environ['ASTER_ROOT'] = ASTER_ROOT
        if debug:
            clean = False
            info  = 1
        else:
            clean = True
            info  = 0
        # ----------------------------------------------------------------------------


    # ------------------------------------------------------------------------------------------------------------------
    #                               Execution en mode EXTERNE
    # ------------------------------------------------------------------------------------------------------------------
    else:
        # Execution en mode EXTERNE : on doit depouiller les parametres de la ligne de commande


        from optparse import OptionParser, OptionGroup

        p = OptionParser(usage='usage: %s fichier_export [options]' % sys.argv[0])

        # Current estimation
        p.add_option('--input',             action='store',       dest='input',             type='string',                                       help='Chaine de texte contenant les parametres')
        p.add_option('--input_step',        action='store',       dest='input_step',        type='string',                                       help='Chaine de texte contenant les pas de discretisation des differences finies')
        p.add_option('--input_file',        action='store',       dest='input_file',        type='string',   default='input.txt',                help='Fichier contenant les parametres')
        p.add_option('--input_step_file',   action='store',       dest='input_step_file',   type='string',                                       help='Fichier contenant les pas de discretisation des differences finies')

        # Outputs
        p.add_option('--output',            action='store',       dest='output',            type='string',   default='output.txt',               help='fichier contenant la fonctionnelle')
        p.add_option('--output_grad',       action='store',       dest='output_grad',       type='string',   default='grad.txt',                 help='fichier contenant le gradient')

        # Code_Aster installation
        p.add_option('--aster_root',        action='store',       dest='aster_root',        type='string',                                       help="Chemin d'installation d'Aster")
        p.add_option('--as_run',            action='store',       dest='as_run',            type='string',                                       help="Chemin vers as_run")

        # General
        p.add_option('--resudir',           action='store',       dest='resudir',           type='string',                                       help="Chemin par defaut des executions temporaires d'Aster")
        p.add_option("--noclean",           action="store_false", dest="clean",                              default=True,                       help="Erase temporary Code_Aster execution directory")
        p.add_option('--info',              action='store',       dest='info',              type='int',      default=1,                          help="niveau de message (0, [1], 2)")
        p.add_option('--sources_root',      action='store',       dest='SOURCES_ROOT',      type='string',                                       help="Chemin par defaut des surcharges Python")
        #p.add_option('--slave_computation', action='store',       dest='slave_computation', type='string',   default='distrib',                  help="Evaluation de l'esclave ([distrib], include)")

        # MACR_RECAL parameters
        p.add_option('--objective',         action='store',       dest='objective',         type='string',   default='fcalc',                    help="Fonctionnelle ([fcalc]/[error])")
        p.add_option('--objective_type',    action='store',       dest='objective_type',    type='string',   default='vector',                   help="type de la fonctionnelle (float/[vector])")
        p.add_option('--gradient_type',     action='store',       dest='gradient_type' ,    type='string',   default='no',                       help="calcul du gradient par Aster ([no]/normal/adim)")

        # MACR_RECAL inputs
        p.add_option('--mr_parameters',     action='store',       dest='mr_parameters',     type='string',   default='N_MR_Parameters.py',       help="Fichier de parametres de MACR_RECAL : parametres, calcul, experience")
        p.add_option('--study_parameters',  action='store',       dest='study_parameters',  type='string',                                       help="Fichier de parametre de l'etude : export")
        p.add_option('--parameters',        action='store',       dest='parameters',        type='string',                                       help="Fichier de parametres")

        options, args = p.parse_args()


        # Study : .export file
        if args: export =  args[0]
        else:
           liste = glob.glob('*.export')
           export = liste[0]
        if not os.path.isfile(export):
           raise Exception("Export file : is missing!")


        # Code_Aster installation
        ASTER_ROOT = None
        if options.aster_root:                  ASTER_ROOT = options.aster_root
        elif os.environ.has_key('ASTER_ROOT'):  ASTER_ROOT = os.environ['ASTER_ROOT']
        if not ASTER_ROOT: raise Exception("ASTER_ROOT is missing! Set it by --aster_root flag or environment variable ASTER_ROOT")
        if not os.path.isdir(ASTER_ROOT): raise Exception("Wrong directory for ASTER_ROOT : %s" % ASTER_ROOT)
        os.environ['ASTER_ROOT'] = ASTER_ROOT
#         sys.path.append(get_absolute_path(os.path.join(ASTER_ROOT, 'STA10.1', 'bibpyt' )))
#         from Utilitai.Utmess import UTMESS

        if options.as_run:          as_run = options.as_run
        else:                       as_run = os.path.join(ASTER_ROOT, 'bin', 'as_run')


        # General
        if options.resudir: resudir = options.resudir
        clean = options.clean

#         if   options.info == 0: info = False
#         elif options.info == 1: info = False
#         elif options.info == 2: info = True
        info = options.info

        # Import des modules supplementaires
        if options.SOURCES_ROOT:
             if not os.path.isdir(options.SOURCES_ROOT): raise Exception("Wrong directory for sources_root : %s" % options.SOURCES_ROOT)
             else:
                 sys.path.insert(0, options.SOURCES_ROOT)
                 sys.path.insert(0, os.path.join(options.SOURCES_ROOT, 'sources'))


        # MACR_RECAL inputs
        if options.mr_parameters:
            try:
                if info>=1: print "Read MR parameters file : %s" % options.mr_parameters
                execfile(options.mr_parameters)
            except: raise Exception("Wrong file for MR Parameters: %s" % options.mr_parameters)
        else: raise Exception("MR Parameters file needed ! Use --mr_parameters flag")
        parametres = globals().get('parametres',  None)
        calcul     = globals().get('calcul',      None)
        experience = globals().get('experience',  None)
        poids      = globals().get('poids',       None)

        if not parametres:  raise Exception("MR Parameters file need to define 'parametres' variable")
        if not calcul:      raise Exception("MR Parameters file need to define 'calcul' variable")
        if not isinstance(parametres, list):
           raise Exception("Wrong type for 'parametres' variable in MR parameters file : %s"  % options.mr_parameters)
        if not isinstance(calcul, list):
           raise Exception("Wrong type for 'calcul' variable in MR parameters file : %s"      % options.mr_parameters)

        if options.objective == 'error':
             if not isinstance(experience, list):
                raise Exception("For error objective output, the 'experience' variable must be a list of arrays")
             if not isinstance(poids, (list, tuple, NP.ndarray)):
                raise Exception("The 'poids' variable must be a list or an array")
             if len(poids) != len(experience):
                raise Exception("'experience' and 'poids' lists must have the same lenght")


        # MACR_RECAL parameters
        objective      = options.objective
        objective_type = options.objective_type
        gradient_type  = options.gradient_type


        # X0 : read from commandline flag or from file
        if not os.path.isfile(options.input_file): options.input_file = None
        if not (options.input or options.input_file):
           raise Exception("Missing input parameters")
        if (options.input and options.input_file):
           raise Exception("Error : please use only one choice for input parameters definition")

        if options.input_file:
            try:
                f = open(options.input_file, 'r')
                options.input = f.read()
                f.close()
            except:
                raise Exception("Can't read input parameters file : %s" % options.input_file)

        # Extract X0 from text
        try:
            txt = options.input.strip()
            txt = txt.replace(',', ' ')
            txt = txt.replace(';', ' ')
            X0 = [ float(x) for x in txt.split() ]
            if not isinstance(X0,list):
               raise Exception("Wrong string for input parameters : %s" % options.input)
        except:
            raise Exception("Can't decode input parameters string : %s.\n It should be a comma separated list." % options.input)


        # dX : read from commandline flag or from file
        dX = None
        if options.gradient_type == 'no':
           if (options.input_step or  options.input_step_file):
              raise Exception("You must set 'gradient_type' to another choice than 'no' or remove input step parameters from commandline")
        else:
            if not (options.input_step or  options.input_step_file):
               raise Exception("Missing input step parameters")
            if (options.input_step and options.input_step_file):
               raise Exception("Error : please use only one choice for input step parameters definition")

            if options.input_step_file:
                try:
                    f = open(options.input_step_file, 'r')
                    options.input_step = f.read()
                    f.close()
                except:
                    raise Exception("Can't read file for discretisation step : %s" % options.input_step_file)

            # Extract dX from text
            try:
                txt = options.input_step.strip()
                txt = txt.replace(',', ' ')
                txt = txt.replace(';', ' ')
                dX = [ float(x) for x in txt.split() ]
                if not isinstance(dX, list):
                   raise Exception("Wrong string for discretisation step : %s" % options.input_step)
            except:
                raise Exception("Can't decode input parameters string : %s.\n It should be a comma separated list." % options.input_step)




    # ------------------------------------------------------------------------------------------------------------------
    #                               Execution des calculs (N+1 calculs distribues si dX est fourni)
    # ------------------------------------------------------------------------------------------------------------------

    # Repertoire contenant les resultats des calculs Aster (None = un rep temp est cree)
    resudir = globals().get('resudir', None)

    # Affichage des parametres
    lpara = [x[0] for x in parametres]
    lpara.sort()
    if info >=1:
       lpara = [x[0] for x in parametres]
       lpara.sort()
       print "Calcul avec les parametres : \n%s" % Affiche_Param(lpara, X0)

    C = CALCULS_ASTER(
                # MACR_RECAL inputs
                parametres          = parametres,
                calcul              = calcul,
                experience          = experience,
                     )

    fonctionnelle, gradient = C.run(
                # Current estimation
                X0                  = X0,
                dX                  = dX,

                # Code_Aster installation
                ASTER_ROOT          = ASTER_ROOT,
                as_run              = as_run,

                # General
                resudir             = resudir,
                clean               = clean,
                info                = info,

                # Study
                export              = export,

#                 # MACR_RECAL inputs
#                 parametres          = parametres,
#                 calcul              = calcul,
#                 experience          = experience,
    )

    # ------------------------------------------------------------------------------------------------------------------
    #                               Calcul de l'erreur par rapport aux donnees experimentale
    # ------------------------------------------------------------------------------------------------------------------
    if not isFromYacs:        # Execution en mode EXTERNE uniquement

        # Calcul de l'erreur par rapport aux donnees experimentale
        if objective == 'error':
            E = CALC_ERROR(
                experience          = experience,
                X0                  = X0,
                calcul              = calcul,
                poids               = poids,
                objective_type      = objective_type,
                info=info,
            )

            erreur                      = E.CalcError(C.Lcalc)
            erreur, residu, A_nodim, A  = E.CalcSensibilityMatrix(C.Lcalc, X0, dX=dX, pas=None)

            fonctionnelle = erreur
            if   gradient_type == 'normal': gradient = A
            elif gradient_type == 'adim':   gradient = A_nodim
            else: raise Exception("??")



    # ------------------------------------------------------------------------------------------------------------------
    #                               Ecriture des resultats
    # ------------------------------------------------------------------------------------------------------------------
    if not isFromYacs:        # Execution en mode EXTERNE uniquement

        # Fonctionnelle
        if options.objective_type == 'float':
           fonctionnelle = sqrt( NP.sum( [x**2 for x in fonctionnelle] ) )
        Ecriture_Fonctionnelle(output_file=options.output, type_fonctionnelle=options.objective_type, fonctionnelle=fonctionnelle)

        # Gradient
        if gradient: Ecriture_Derivees(output_file=options.output_grad, derivees=gradient)



    # ------------------------------------------------------------------------------------------------------------------
    #                               Affichages
    # ------------------------------------------------------------------------------------------------------------------
    if info>=2:
        print "\nFonctionnelle au point X0: \n%s" % str(fonctionnelle)
        import pprint
        if dX:
           print "\nGradient au point X0:"
           pprint.pprint(gradient)
