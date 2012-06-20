#@ MODIF chainage_thm_ops Macro  DATE 18/06/2012   AUTEUR DELMAS J.DELMAS 
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

############################################
#
# On pourrait éventuellement utiliser MACR_ADAP_MAIL dans
# cette macro pour gagner du temps sur
# la phase de projection ...
# Serait utile surtout pour le 3D et pour les gros
# maillages
#
############################################

import aster
import numpy
from Accas import _F
from Noyau.N_utils import AsType
from Utilitai.Utmess import UTMESS
from chainage_init import *
from chainage_hydr_meca import *
from chainage_meca_hydr import *

def chainage_thm_ops(self,TYPE_CHAINAGE,**args):
  """
     Ecriture de la macro CHAINAGE_THM
  """
  ##################################################################
  # RESU_MECA / MODELE_HYDR : résultat mécanique à projeter
  #                           modèle hydraulique d'arrivée
  # RESU_HYDR / MODELE_MECA : résultat hydraulique à projeter
  #                           modèle mécanique d'arrivée
  # INST                    : instant auquel on veut les variables de commande
  # MATR_MH / MATR_HM1 / MATR_HM2 : matrices de projection pour gagner
  #                                 du temps sur les phases de projection
  # TYPE_CHAINAGE   : MECA_HYDR / HYDR_MECA / INIT
  # TYPE_RESU       : obligatoire si TYPE_CHAINAGE = HYDR_MECA
  #                   -> evol_varc
  #                   -> cham_no
  ##################################################################

  #########################################################
  # Introduction : déclarations préalables
  #########################################################

  ier=0

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # On importe les definitions des commandes a utiliser dans la macro
  CREA_MAILLAGE   = self.get_cmd('CREA_MAILLAGE')
  PROJ_CHAMP      = self.get_cmd('PROJ_CHAMP')
  CREA_CHAMP      = self.get_cmd('CREA_CHAMP')
  CREA_RESU       = self.get_cmd('CREA_RESU')
  DEFI_LIST_REEL  = self.get_cmd('DEFI_LIST_REEL')

  #########################################################
  # Début de la macro-commande
  #########################################################

  # précision machine
  prec = numpy.finfo(float).eps
  
  motscles=dict()

  INST = None
  if args.has_key('INST'):
    if   args['INST'] != None : INST = args['INST']

  b_type_resu_cham_no = False
  if args.has_key('TYPE_RESU'):
    if   args['TYPE_RESU'] != None : TYPE_RESU = args['TYPE_RESU']
    if (TYPE_RESU == "CHAM_NO")    : b_type_resu_cham_no = True

  #########################################################
  # 3 possibilités pour TYPE_CHAINAGE :
  #  1. HYDR_MECA
  #  2. MECA_HYDR
  #  3. INIT
  #########################################################
  #########################################################
  # 1. Chaînage HYDRAULIQUE ===> MECANIQUE
  #########################################################
  
  if (TYPE_CHAINAGE == "HYDR_MECA") :

     self.DeclareOut('nomres',self.sd)

     nomres = CHAINAGE_HYDR_MECA(self,args,motscles)

  #########################################################
  # 2. Chaînage MECANIQUE ===> HYDRAULIQUE
  #########################################################
  
  elif (TYPE_CHAINAGE == "MECA_HYDR") :

     self.DeclareOut('nomres',self.sd)

     nomres = CHAINAGE_MECA_HYDR(self,args,motscles)

  #########################################################
  # 3. Initialisation des matrices de projection
  #########################################################
  
  elif (TYPE_CHAINAGE == "INIT") :

     self.DeclareOut('MATR_MH', args['MATR_MH'])
     self.DeclareOut('MATR_HM1',args['MATR_HM1'])
     self.DeclareOut('MATR_HM2',args['MATR_HM2'])

     MATR_MH, MATR_HM1, MATR_HM2 = CHAINAGE_INIT(self,args,motscles)
    
  else :

    UTMESS('F', 'DVP_1')

  return ier




