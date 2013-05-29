# coding=utf-8
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

import aster
from Accas import _F
from Utilitai.Utmess import UTMESS
import numpy
prec = numpy.finfo(float).eps

def CHAINAGE_HYDR_MECA(self,args,motscles):

    CREA_CHAMP      = self.get_cmd('CREA_CHAMP')
    CREA_RESU       = self.get_cmd('CREA_RESU')
    PROJ_CHAMP      = self.get_cmd('PROJ_CHAMP')

    INST = None
    if args.has_key('INST'):
      if   args['INST'] != None : INST = args['INST']

    b_type_resu_cham_no = False
    TYPE_RESU = args['TYPE_RESU']
      
    if (TYPE_RESU == "CHAM_NO") : b_type_resu_cham_no = True

    RESU_HYDR   = args['RESU_HYDR']
    MODELE_MECA = args['MODELE_MECA']
    MATR_HM1    = args['MATR_HM1']
    MATR_HM2    = args['MATR_HM2']
   
    para = RESU_HYDR.LIST_PARA()
    smo = set(para['MODELE'])

# normalement, il ne doit y avoir qu'un modèle ...
    if len(smo) <> 1 : UTMESS('F', 'CHAINAGE_10')
    nom_mo_re = list(smo)[0]

    __modele = self.get_concept(nom_mo_re)

  ############################################################
  # Nom du modèle obtenu à partir du résultat : nom_modele_1
  ############################################################

    iret,ibid,nom_modele_1 = aster.dismoi('F','MODELISATION',__modele.nom,'MODELE')
    nom_modele_1=nom_modele_1.strip()

    iret,ibid,yathm1 = aster.dismoi('F','EXI_THM',__modele.nom,'MODELE')
  
  #########################################################
  # A l'heure actuelle, les modélisations autorisées pour
  # faire du chaînage sont :
  #
  # Pour la mécanique :
  #
  #   => D_PLAN, D_PLAN_SI, D_PLAN_GRAD_SIGM
  #   => 3D, 3D_SI
  #
  #  Pour l'hydraulique :
  #
  #   => D_PLAN_HS
  #   => 3D_HS
  #   => les modélisations HM saturées à intégration sélective :
  #      D_PLAN_HMS, 3D_HMS
  #########################################################
  
    mod_mec_autorise = ['D_PLAN','D_PLAN_SI','D_PLAN_GRAD_SIGM']
    mod_hyd_autorise = ['D_PLAN_HS','D_PLAN_HMS']
    
  #############################################
  # Nom du modèle 2 fourni en entrée : nom_modele_2
  #############################################
  
    iret,ibid,nom_modele_2 = aster.dismoi('F','MODELISATION',MODELE_MECA.nom,'MODELE')
    nom_modele_2=nom_modele_2.strip()

    linst_resultat = RESU_HYDR.LIST_VARI_ACCES()['INST']

  ###########################################################
  # INST est rentré en argument par l'utilisateur
  # instp et instm sont les 2 derniers instants présents dans
  # le résultat donné en entrée
  ###########################################################
  
    instp = linst_resultat[-1]

    instm = None
    inst0 = linst_resultat[0]

    if (inst0 < instp) :
  # Dans ce cas, on a au moins 2 "vrais" instants dans RESULTAT : instp et instm
      instm = linst_resultat[-2]
      b_inst_initial = False
    else :

  # Dans ce cas, on a instp = 0 et instm n'existe pas
  # On particularise ce cas pour le chaînage HYDR_MECA, mais pas pour le chaînage
  # MECA_HYDR où le traitement est plus simple. En effet, cette situation correspond
  # à un chaînage HM au premier pas de temps. Il suffit de ne pas donner de variable de
  # commande !

      b_inst_initial = True

    inst_coincident = False
    if (INST != None) :
      if (INST < instp ) :
        UTMESS('F', 'CHAINAGE_6', valr=[INST],valk=[RESU_HYDR.nom])
      if abs(instp - INST)<prec :
        inst_coincident = True

  #########################################################
  # On vérifie que le résultat donné en entrée
  # (hydraulique) est défini
  # sur un modèle pour lequel on sait faire du chaînage
  #
  # A l'heure actuelle, les modélisations autorisées sont :
  # => D_PLAN_HS
  #
  # On répondra plus tard aux demandes d'évolution pour l'insaturé
  # et pour le 3D
  #########################################################

    if not(nom_modele_1 in mod_hyd_autorise) :
      UTMESS('F', 'CHAINAGE_3', valk=[nom_modele_1,'de départ'])
  
  ###############################################################
  # On récupère le nom du maillage hydraulique à partir du modèle
  # hydraulique
  ###############################################################
  
    iret,ibid,nom_mail = aster.dismoi('F','NOM_MAILLA',__modele.nom,'MODELE')
    nom_mail=nom_mail.strip()
    __maillage_h = self.get_concept(nom_mail)

  ###############################################################
  # On vérifie que le résultat donné en sortie
  # (mécanique) est défini
  # sur un modèle pour lequel on sait faire du chaînage
  #
  # A l'heure actuelle, les modélisations autorisées sont :
  # => D_PLAN, D_PLAN_SI
  # => 3D, 3D_SI
  ###############################################################

    if not(nom_modele_2 in mod_mec_autorise) :
      UTMESS('F', 'CHAINAGE_4', valk=[nom_modele_2,'d arrivée'])
    
    __prep=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',
                      OPERATION='EXTR',
                      RESULTAT=RESU_HYDR,
                      NOM_CHAM='DEPL',
                      INST=instp,**motscles);

    __prepmec=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',
                         OPERATION='ASSE',
                         MAILLAGE=__maillage_h,
                         ASSE=(_F(TOUT='OUI',
                                  CHAM_GD=__prep,
                                  NOM_CMP='PRE1',
                                  NOM_CMP_RESU='PTOT',),),**motscles);
      
    if b_type_resu_cham_no :

      __proch=PROJ_CHAMP(CHAM_GD=__prepmec,
                         MATR_PROJECTION = MATR_HM1,**motscles);

      nomres=PROJ_CHAMP(CHAM_GD=__proch,MATR_PROJECTION=MATR_HM2,**motscles);

    else :

      if not(b_inst_initial) :

        __prem=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',
                          OPERATION='EXTR',
                          RESULTAT=RESU_HYDR,
                          NOM_CHAM='DEPL',
                          INST=instm,**motscles);

        __premmec=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',
                             OPERATION='ASSE',
                             MAILLAGE=__maillage_h,
                             ASSE=(_F(TOUT='OUI',
                                      CHAM_GD=__prem,
                                      NOM_CMP='PRE1',
                                      NOM_CMP_RESU='PTOT',),),**motscles);

      else :

        __premmec=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',
                             OPERATION='AFFE',
                             MAILLAGE=__maillage_h,
                             AFFE=(_F(TOUT='OUI',
                                      VALE=0.,
                                      NOM_CMP='PTOT',),),**motscles);        

      if inst_coincident :

        __ptotre=CREA_RESU(OPERATION='AFFE',
                           TYPE_RESU='EVOL_VARC',
                           NOM_CHAM='PTOT',
                           AFFE=(
                                _F(CHAM_GD=__premmec,INST=instm,),
                                _F(CHAM_GD=__prepmec,INST=instp,),),);

      else :

  #############################################################
  # l'incrément de pression à l'instant t_i=INST est donné par
  # les valeurs à t_(i-2)=instm et t_(i-1)=instp
  #############################################################
  
        __ptotre=CREA_RESU(OPERATION='AFFE',
                           TYPE_RESU='EVOL_VARC',
                           NOM_CHAM='PTOT',
                           AFFE=(
                                _F(CHAM_GD=__premmec,INST=instp,),
                                _F(CHAM_GD=__prepmec,INST=INST,),),);
        
      __projres=PROJ_CHAMP(RESULTAT=__ptotre,MATR_PROJECTION=MATR_HM1,**motscles);

      nomres=PROJ_CHAMP(RESULTAT=__projres,MATR_PROJECTION=MATR_HM2,**motscles);
