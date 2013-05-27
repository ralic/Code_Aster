# -*- coding: iso-8859-1 -*-
# person_in_charge: aimery.assire at edf.fr
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
# ======================================================================


debug = False

import copy
import types
import os
import sys
import pprint
import math
import glob
import socket
import shutil
import tempfile

import numpy as NP

import aster_core
from Utilitai.System import ExecCommand
from Utilitai.Utmess import UTMESS

from recal import Affiche_Param, CALCULS_ASTER, CALC_ERROR
from reca_utilitaires import Random_Tmp_Name


# ------------------------------------------------------------------------------
class CALCUL_ASTER:

  def __init__(self, jdc, METHODE,
                          UNITE_ESCL,
                          UNITE_RESU,
                          para,
                          reponses,
                          PARA_DIFF_FINI=1.E-3,
                          vector_output=True,
                          GRADIENT=None,
                          DYNAMIQUE=None,
                          #LANCEMENT='DISTRIBUE',
                          CALCUL_ESCLAVE=None,
                          INFO=0,
               ):


      self.METHODE               = METHODE
      self.UNITE_ESCL            = UNITE_ESCL
      self.UNITE_RESU            = UNITE_RESU
      self.para                  = para
      self.reponses              = reponses
      self.PARA_DIFF_FINI        = PARA_DIFF_FINI
      self.vector_output         = vector_output

      self.MODE                  = CALCUL_ESCLAVE['MODE']
#       self.MEMOIRE               = CALCUL_ESCLAVE['MEMOIRE']
#       self.TEMPS                 = CALCUL_ESCLAVE['TEMPS']

      # Parametres batch
      self.CLASSE                = CALCUL_ESCLAVE['CLASSE']
      self.ACTUALISATION         = CALCUL_ESCLAVE['ACTUALISATION']
      self.NMAX_SIMULT           = CALCUL_ESCLAVE['NMAX_SIMULT']
      self.LANCEMENT             = CALCUL_ESCLAVE['LANCEMENT']

      # Parametres des job esclaves
      self.tpsjob                = CALCUL_ESCLAVE['tpsjob']
      self.tpmax                 = CALCUL_ESCLAVE['tpmax']
      self.mem_aster             = CALCUL_ESCLAVE['mem_aster']
      self.memjob                = CALCUL_ESCLAVE['memjob']
      self.memjeveux             = CALCUL_ESCLAVE['memjeveux']
      self.mpi_nbcpu             = CALCUL_ESCLAVE['mpi_nbcpu']
      self.mpi_nbnoeud           = CALCUL_ESCLAVE['mpi_nbnoeud']

      self.INFO                  = INFO

      # Optionnels
      self.UNITE_GRAPHIQUE       = None
      self.export                = None
      self.follow_output         = None
      self.unity_follow          = None

      self.GRADIENT              = GRADIENT
      self.DYNAMIQUE             = DYNAMIQUE
      #self.LANCEMENT             = LANCEMENT

      # Variables locales
      tmpfile = tempfile.NamedTemporaryFile(prefix= os.path.join(os.getcwd(), 'tmp_export_') )
      self.new_export            = str(tmpfile.name)
      tmpfile.close()

      # Variables calculees
      self.evaluation_fonction   = 0

      # Initialisation
      self.reset()

      # Dynamique : pour l'appariement manuel des modes en dynamique
      self.graph_mac             = False

      # JDC
      self.jdc                   = jdc


  # ------------------------------------------------------------------------------
  def Set_Parameters(self, **args):
    for cle in args.keys():
       exec( "%s=%s" % (cle, args[cle]) )


  # ------------------------------------------------------------------------------
  def reset(self):

      self.Lcalc                 = None
      self.erreur                = None
      self.residu                = None
      self.norme                 = None
      self.A_nodim               = None
      self.A                     = None
      self.norme_A_nodim         = None
      self.norme_A               = None
      self.L                     = None
#      self.L_J_init              = None


  # ------------------------------------------------------------------------------
  def calcul_Aster(self, val, dX=None):

        # ----------------------------------------------------------------------------
        # Commun
        # ----------------------------------------------------------------------------
        self.val         = val
        info             = self.INFO

        # MACR_RECAL inputs
        parametres       = self.LIST_PARA
        calcul           = self.RESU_CALC
        experience       = self.RESU_EXP

        # Current estimation
        X0               = val
        dX               = dX

        # Objet Calcul
        C = CALCULS_ASTER(
                # MACR_RECAL inputs
                parametres    = parametres,
                calcul        = calcul,
                experience    = experience,
                LANCEMENT     = self.LANCEMENT,
                jdc           = self.jdc,
                         )

        # Traitement special pour la dynamique (affichage des MAC dans l'esclave)
        if self.DYNAMIQUE: C.SetDynamiqueMode(self.DYNAMIQUE, self.graph_mac)


        # ----------------------------------------------------------------------------
        # ASRUN distribue
        # ----------------------------------------------------------------------------
        if self.LANCEMENT == 'DISTRIBUTION':

            # Creation du repertoire temporaire pour l'execution de l'esclave
            tmp_macr_recal = self.Creation_Temporaire_Esclave()

            # Creation du fichier .export de l'esclave
            self.Creation_Fichier_Export_Esclave(tmp_macr_recal)

            # Code_Aster installation
            if  os.environ.has_key('ASTER_ROOT'):
                ASTER_ROOT = os.environ['ASTER_ROOT']
            else:
                import aster
                ASTER_ROOT       = os.path.join(aster_core.get_option('repout'), '..')
            as_run           = os.path.join(ASTER_ROOT, 'bin', 'as_run')

            # General
            resudir          = None
            clean            = True
            NMAX_SIMULT      = self.NMAX_SIMULT

            # Study
            export           = self.new_export

            C.follow_output = self.follow_output
            C.unity_follow  = self.unity_follow


            # Lancement des calculs
            fonctionnelle, gradient = C.run(
                # Current estimation
                X0,
                dX,

                # Code_Aster installation
                ASTER_ROOT     = ASTER_ROOT,
                as_run         = as_run,

                # General
                resudir        = resudir,
                clean          = clean,
                info           = info,
                NMAX_SIMULT    = NMAX_SIMULT,

                # Study
                export         = export,

                                           )


        # ----------------------------------------------------------------------------
        # Aiguillage vers INCLUDE
        # ----------------------------------------------------------------------------
        if self.LANCEMENT == 'INCLUSION':
            C.UNITE_ESCL  = self.UNITE_ESCL

            # Lancement des calculs
            fonctionnelle, gradient = C.run(
                # Current estimation
                X0,
                dX,
                # General
                info,
                                  )


        # ----------------------------------------------------------------------------
        # Sortie
        # ----------------------------------------------------------------------------
        if dX: self.evaluation_fonction += 1+ len(dX)
        else:  self.evaluation_fonction += 1

        self.Lcalc = C.Lcalc

        if not dX: return self.Lcalc[0], {}
        else:      return fonctionnelle, gradient


  # ------------------------------------------------------------------------------
  def Affiche_Param(self, val):
      """ Affiche les parametres """
      return Affiche_Param(self.para, val)


  # ------------------------------------------------------------------------------
  def calcul_F(self, val):
       """
          Calcul de F
       """
       UTMESS('I', 'RECAL0_25', valk=self.Affiche_Param(val) )

       # Reset les variables deja calculees par les calculs precedents
       self.reset()

       # Calcul pour le jeu de parametre val
       fonctionnelle, gradient = self.calcul_Aster(val, dX=None)

       # Calcul de l'erreur par rapport aux donnees experimentale
       E = CALC_ERROR(
           experience          = self.RESU_EXP,
           X0                  = val,
           calcul              = self.RESU_CALC,
           poids               = self.Simul.poids,
           objective_type      = 'vector',
           info                = self.INFO,
       )
       self.erreur  = E.CalcError(self.Lcalc)

       # norme de l'erreur
       self.norme = NP.sum( [x**2 for x in self.erreur] )

       if debug:
           print "self.reponses=", self.reponses
           print "F=", E.F
           print "L_J=", E.L_J
           print "L_J_init=", E.L_J_init
           print "J=", E.J
           print 'erreur=', self.erreur
           print "norme de l'erreur=", self.norme
           print "norme de J (fonctionnelle)=", str(E.J)

       if self.INFO>=2:
          UTMESS('I', 'RECAL0_30')
          if self.evaluation_fonction >1: UTMESS('I', 'RECAL0_39', valk=str(self.evaluation_fonction))

       if self.vector_output:
          if self.INFO>=2: UTMESS('I', 'RECAL0_35', valr=self.norme)
          return self.erreur
       else:
          if self.INFO>=2: UTMESS('I', 'RECAL0_36', valr=self.norme)
          return self.norme


  # ------------------------------------------------------------------------------
  def calcul_F2(self, val):
      """
         Calcul de F (et de G) mais renvoit juste la fonctionnelle
         Sert pour les algorithmes qui veulent une fonction ou F, une fonction pour G mais qu'on veut pouvoir tout calculer en distibue
      """
      a, b, c, d = self.calcul_FG(val)
      if self.vector_output: return self.erreur
      else:                  return self.norme


  # ------------------------------------------------------------------------------
  def verif_borne_gradient(self, val, dX):
      """
         Verification que les parametres perturbes sont bien dans l'intervalle defini par l'utilisateur
         Sinon, on colle le parametre a la borne
      """
      print self.para
      print self.LIST_PARA
      for i in range(len(val)):
         print i, val[i], dX[i]
         # min
      sys.exit()

  # ------------------------------------------------------------------------------
  def calcul_FG(self, val):
      """
         Calcul de F et de G
      """
      UTMESS('I', 'RECAL0_26', valk=self.Affiche_Param(val) )

      # Reset les variables deja calculees par les calculs precedents
      self.reset()

      # Calcul pour le jeu de parametres val
      dX = len(val)*[self.PARA_DIFF_FINI]
      fonctionnelle, gradient = self.calcul_Aster(val, dX)

      # Calcul de l'erreur par rapport aux donnees experimentale
      E = CALC_ERROR(
          experience          = self.RESU_EXP,
          X0                  = val,
          calcul              = self.RESU_CALC,
          poids               = self.Simul.poids,
          objective_type      = 'vector',
          info                = self.INFO,
      )

      self.erreur, self.residu, self.A_nodim, self.A  = E.CalcSensibilityMatrix(Lcalc=self.Lcalc, val=val, dX=None, pas=self.PARA_DIFF_FINI)

      if debug:
          print "A_nodim=", self.A_nodim
          print "self.A=", self.A
          print "self.erreur=", self.erreur
          print "self.residu=", self.residu
          print "self.vector_output=", self.vector_output


      if self.vector_output:
          return self.erreur, self.residu, self.A_nodim, self.A
      else:
          # norme de l'erreur
          self.norme = NP.dot(self.erreur, self.erreur)**0.5
          self.norme_A_nodim = NP.zeros( (1,len(self.para)))
          self.norme_A       = NP.zeros( (1,len(self.para)))
          for c in range(len(self.A[0,:])):
              norme_A_nodim = 0
              norme_A       = 0
              for l in range(len(self.A[:,0])):
                   norme_A_nodim += self.A_nodim[l,c] * self.A_nodim[l,c]
                   norme_A       += self.A[l,c] * self.A[l,c]
              self.norme_A_nodim[0,c] = math.sqrt( norme_A_nodim )
              self.norme_A[0,c] = math.sqrt( norme_A )
          return self.norme, self.residu, self.norme_A_nodim, self.norme_A



  # ------------------------------------------------------------------------------
  def calcul_G(self, val):
      """
        Calcul de G
      """
      UTMESS('I', 'RECAL0_27', valk=self.Affiche_Param(val) )

      # Si le calcul Aster (et ses derivees) est deja effectue pour val on ne le refait pas
      if not ( (self.val == val) and self.A):
          self.erreur, self.residu, self.A_nodim, self.A = self.calcul_FG(val)
      return NP.dot(NP.transpose(self.A), self.erreur)


  # ------------------------------------------------------------------------------
  def Creation_Temporaire_Esclave(self):
     """
        Creation du repertoire temporaire d'execution du calcul esclace
     """
     # Creation du repertoire temporaire
     tmp_macr_recal = os.getcwd() + os.sep + 'tmp_macr_recal'
#     tmp_macr_recal = Random_Tmp_Name( prefix = os.getenv('HOME') + os.sep + 'tmp_macr_recal_' )
     try:    os.mkdir(tmp_macr_recal)
     except: pass
     if not os.path.exists(tmp_macr_recal): UTMESS('F','RECAL0_82',valk=tmp_macr_recal)
     try:    os.mkdir(tmp_macr_recal + os.sep + 'REPE_TABLE')
     except: pass
     if not os.path.exists(tmp_macr_recal + os.sep + 'REPE_TABLE'): UTMESS('F','RECAL0_82',valk=tmp_macr_recal + os.sep + 'REPE_TABLE')

     return tmp_macr_recal


  # ------------------------------------------------------------------------------
  def Creation_Fichier_Export_Esclave(self, tmp_macr_recal):
     """
        Creation du fichier .export pour le calcul esclave
     """

     from asrun.profil import ASTER_PROFIL
     from asrun.common.sysutils import on_64bits

     # Recuperation du fichier .export
     if self.export:  export = self.export
     else:
         list_export = glob.glob('*.export')
         if len(list_export) == 0: UTMESS('F','RECAL0_4')
         elif len(list_export) >1: UTMESS('F','RECAL0_5')
         export = list_export[0]

     # On modifie le profil
     prof = ASTER_PROFIL(export)

     # En local
     user_mach = ''

     # Chaine user@hostname (pour les calculs distribues et en batch)
     try:         username = prof.param['username'][0]
     except:
         try:     username = os.getlogin()
         except:
                  import getpass
                  username = getpass.getuser()
     user_mach_dist = "%s@%s:" % ( username, socket.gethostname() )

     # On cherche s'il y a un fichier hostfile pour rajouter user@hostname
     l_fr = getattr(prof, 'data')
     l_tmp = l_fr[:]
     for dico in l_tmp:
        if dico['type']=='hostfile':
           user_mach = user_mach_dist
           break

     # Parametres ajoutes par le mot-cle CALCUL_ESCLAVE
     if self.tpsjob:      prof.param['tpsjob']      = str(self.tpsjob)
     if self.tpmax:       prof.args['tpmax']        = str(self.tpmax)
     if self.mem_aster:   prof.param['mem_aster']   = str(self.mem_aster)
     if self.memjob:      prof.param['memjob']      = str(self.memjob)
     if self.memjeveux:   prof.args['memjeveux']    = str(self.memjeveux)
     if self.mpi_nbcpu:   prof.param['mpi_nbcpu']   = str(self.mpi_nbcpu)
     if self.mpi_nbnoeud: prof.param['mpi_nbnoeud'] = str(self.mpi_nbnoeud)


     # En batch et distribue
     if self.MODE == 'BATCH':
        user_mach = user_mach_dist
        prof.param['mode']      = 'batch'
        #if self.mem_aster: prof.param['mem_aster'] = str(self.mem_aster)

        # Choix d'une classe reservee
        if self.CLASSE:
           prof.param['classe'] = self.CLASSE

     # xterm
     if prof.param.has_key('xterm'):
        del prof.param['xterm']

     # fichier/répertoire
     for lab in ('data', 'resu'):
       l_fr = getattr(prof, lab)
       l_tmp = l_fr[:]

       for dico in l_tmp:

         # répertoires
         if dico['isrep']:

           # base non prise en compte
           if dico['type'] in ('base', 'bhdf'):
             l_fr.remove(dico)

           if lab == 'resu':
             dico['path'] = user_mach + os.path.join(tmp_macr_recal, os.path.basename(dico['path']))

         # fichiers
         else:

           # Nom du fichier .mess (pour recuperation dans REPE_OUT)
           if dico['ul'] == 6:
             self.nom_fichier_mess_fils = os.path.basename(dico['path'])

           # Nom du fichier .resu (pour recuperation dans REPE_OUT)
           if dico['ul'] == 8:
             self.nom_fichier_resu_fils = os.path.basename(dico['path'])

           # Ancien .comm non pris en compte
           # Fichier d'unite logique UNITE_RESU (rapport de MACR_RECAL) non pris en compte
           if dico['type'] == 'comm' or (dico['ul'] == self.UNITE_RESU and lab == 'resu'):
             l_fr.remove(dico)

           # Fichier d'unite logique UL devient le nouveau .comm
           elif dico['ul'] == self.UNITE_ESCL:
             self.fichier_esclave = dico['path']
             dico['type'] = 'comm'
             dico['ul']   = 1
             dico['path'] = user_mach + os.path.join(os.getcwd(), 'fort.%d' % self.UNITE_ESCL)

           # Tous les autres fichiers en Resultat
           elif lab == 'resu':
              l_fr.remove(dico)

           # Tous les autres fichiers en Donnees
           elif lab == 'data':
              if dico['type'] not in ('exec', 'ele'):
                 if dico['ul'] != 0:   # Traite le cas des sources python sourchargees
                     # Si distant/distribue on doit prendre les fichiers de donnes dans un endroit partage entre les machines/noeuds
                     if user_mach:
                        src = dico['path']
                        dst = os.path.join(tmp_macr_recal, os.path.basename(dico['path']))
                        try:
                            shutil.copyfile(src, dst)
                            dico['path'] = user_mach + os.path.join(tmp_macr_recal, os.path.basename(dico['path']))
                        except Exception, e:
                            if debug: print e
                     else:
                        dico['path'] = user_mach + os.path.join(os.getcwd(), 'fort.%s' % dico['ul'])

           # sinon on garde la ligne telle quelle
       setattr(prof, lab, l_fr)

     # Ecriture du nouveau fichier export
     prof.WriteExportTo(self.new_export)

     if debug: os.system('cp ' + self.new_export + ' /tmp')

     # --FIN CLASSE  ----------------------------------------------------------------------------
