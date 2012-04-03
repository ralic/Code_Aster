#@ MODIF chainage_meca_hydr Macro  DATE 03/04/2012   AUTEUR SELLENET N.SELLENET 
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

import aster
from Accas import _F
from Utilitai.Utmess import UTMESS
import numpy
prec = numpy.finfo(float).eps

def CHAINAGE_MECA_HYDR(self,args,motscles):

    DEFI_LIST_REEL  = self.get_cmd('DEFI_LIST_REEL')
    CALC_ELEM       = self.get_cmd('CALC_ELEM')
    CREA_CHAMP      = self.get_cmd('CREA_CHAMP')
    CREA_RESU       = self.get_cmd('CREA_RESU')
    CALC_CHAMP      = self.get_cmd('CALC_CHAMP')
    PROJ_CHAMP      = self.get_cmd('PROJ_CHAMP')
    
    b_info = False
    if args.has_key('INFO'):
      if   args['INFO'] != None :
        motscles['INFO'] = args['INFO']
        if args['INFO']==2 :
          b_info = True
        
    RESU_MECA   = args['RESU_MECA']
    MODELE_HYDR = args['MODELE_HYDR']
    INST        = args['INST']
    MATR_MH     = args['MATR_MH']
    
    para = RESU_MECA.LIST_PARA()
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
  
    iret,ibid,nom_modele_2 = aster.dismoi('F','MODELISATION',MODELE_HYDR.nom,'MODELE')
    nom_modele_2=nom_modele_2.strip()

    linst_resultat = RESU_MECA.LIST_VARI_ACCES()['INST']

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
        UTMESS('F', 'CHAINAGE_6', valr=[INST],valk=[RESU_MECA.nom])
      if abs(instp - INST)<prec :
        inst_coincident = True
        
  #########################################################
  # On vérifie que le résultat donné en entrée
  # (mécanique) est défini
  # sur un modèle pour lequel on sait faire du chaînage
  #
  # A l'heure actuelle, les modélisations autorisées sont :
  # => D_PLAN, D_PLAN_SI
  # => 3D, 3D_SI
  #########################################################

    if not(nom_modele_1 in mod_mec_autorise) and (yathm1=='NON') :
      UTMESS('F', 'CHAINAGE_4', valk=[nom_modele_1,'de départ'])

  #########################################################
  # On vérifie que le résultat donné en sortie
  # (hydraulique) est défini
  # sur un modèle pour lequel on sait faire du chaînage
  #
  # A l'heure actuelle, les modélisations autorisées sont :
  # => D_PLAN_HS
  # => 3D_HS
  #########################################################

    if not(nom_modele_2 in mod_hyd_autorise) :
      UTMESS('F', 'CHAINAGE_3', valk=[nom_modele_2,'d arrivée'])

    iret,ibid,nom_mail = aster.dismoi('F','NOM_MAILLA',MODELE_HYDR.nom,'MODELE')
    nom_mail=nom_mail.strip()
    __maillage_h = self.get_concept(nom_mail)

    linst=[instm,instp];

    __listinst=DEFI_LIST_REEL(VALE=linst,**motscles);

    __epsir=CALC_ELEM(RESULTAT=RESU_MECA,
                      OPTION='EPSI_ELNO',
                      TOUT='OUI',
                      LIST_INST=__listinst,**motscles);

    if b_info :  UTMESS('I', 'CHAINAGE_7',valk=['epsi_elno',nom_mo_re],valr=[instp])
      
    __epsip=CREA_CHAMP(TYPE_CHAM='ELNO_EPSI_R',
                       OPERATION='EXTR',
                       RESULTAT=__epsir,
                       NOM_CHAM='EPSI_ELNO',
                       INST=instp,**motscles);

    if b_info :  UTMESS('I', 'CHAINAGE_7',valk=['epsi_elno',nom_mo_re],valr=[instm])

    __epsim=CREA_CHAMP(TYPE_CHAM='ELNO_EPSI_R',
                       OPERATION='EXTR',
                       RESULTAT=__epsir,
                       NOM_CHAM='EPSI_ELNO',
                       INST=instm,**motscles);

    if b_info :  UTMESS('I', 'CHAINAGE_7',valk=['divu',nom_mo_re],valr=[instp])
    
    __defvp=CREA_CHAMP(TYPE_CHAM='ELNO_EPSI_R',
                       OPERATION='ASSE',
                       MODELE=__modele,
                       PROL_ZERO='OUI',
                       ASSE=(
                       _F(CHAM_GD=__epsip,
                          NOM_CMP='EPXX',
                          NOM_CMP_RESU='EPXX',
                          CUMUL='OUI',
                          TOUT='OUI',),
                       _F(CHAM_GD=__epsip,
                          NOM_CMP='EPYY',
                          NOM_CMP_RESU='EPXX',
                          CUMUL='OUI',
                          TOUT='OUI',),
                       _F(CHAM_GD=__epsip,
                          NOM_CMP='EPZZ',
                          NOM_CMP_RESU='EPXX',
                          CUMUL='OUI',
                          TOUT='OUI',),),**motscles);

    if b_info :  UTMESS('I', 'CHAINAGE_7',valk=['divu',nom_mo_re],valr=[instm])
    
    __defvm=CREA_CHAMP(TYPE_CHAM='ELNO_EPSI_R',
                       OPERATION='ASSE',
                       MODELE=__modele,
                       PROL_ZERO='OUI',
                       ASSE=(
                       _F(CHAM_GD=__epsim,
                          NOM_CMP='EPXX',
                          NOM_CMP_RESU='EPXX',
                          CUMUL='OUI',
                          TOUT='OUI',),
                       _F(CHAM_GD=__epsim,
                          NOM_CMP='EPYY',
                          NOM_CMP_RESU='EPXX',
                          CUMUL='OUI',
                          TOUT='OUI',),
                       _F(CHAM_GD=__epsim,
                          NOM_CMP='EPZZ',
                          NOM_CMP_RESU='EPXX',
                          CUMUL='OUI',
                          TOUT='OUI',),),**motscles);

    __defvr1=CREA_RESU(OPERATION='AFFE',
                       TYPE_RESU='EVOL_NOLI',
                       NOM_CHAM='EPSI_ELNO',
                       AFFE=(_F(CHAM_GD=__defvm,
                                MODELE=__modele,
                                INST=instm,),
                             _F(CHAM_GD=__defvp,
                                MODELE=__modele,
                                INST=instp,),),);
    
    __defvr1=CALC_CHAMP(reuse=__defvr1,
                        RESULTAT=__defvr1,
                        DEFORMATION='EPSI_NOEU',
                        LIST_INST=__listinst,
                        TOUT='OUI',);

    __epsrpro=PROJ_CHAMP(RESULTAT=__defvr1,
                           NOM_CHAM='EPSI_NOEU',
                           MATR_PROJECTION=MATR_MH,**motscles);
    
    __defvrppro=CREA_CHAMP(TYPE_CHAM='NOEU_EPSI_R',
                           OPERATION='EXTR',
                           RESULTAT=__epsrpro,
                           NOM_CHAM='EPSI_NOEU',
                           INST=instp,**motscles);

    __defvrmpro=CREA_CHAMP(TYPE_CHAM='NOEU_EPSI_R',
                           OPERATION='EXTR',
                           RESULTAT=__epsrpro,
                           NOM_CHAM='EPSI_NOEU',
                           INST=instm,**motscles);
    
    if b_info :  UTMESS('I', 'CHAINAGE_7',valk=['divu',MODELE_HYDR.nom],valr=[instm])
    
    __divum=CREA_CHAMP(TYPE_CHAM='NOEU_EPSI_R',
                       OPERATION='ASSE',
                       MODELE=MODELE_HYDR,
                       ASSE=(
                       _F(CHAM_GD=__defvrmpro,
                          NOM_CMP='EPXX',
                          NOM_CMP_RESU='DIVU',
                          TOUT='OUI',),),**motscles);

    if b_info :  UTMESS('I', 'CHAINAGE_7',valk=['divu',MODELE_HYDR.nom],valr=[instp])
    
    __divup=CREA_CHAMP(TYPE_CHAM='NOEU_EPSI_R',
                       OPERATION='ASSE',
                       MODELE=MODELE_HYDR,
                       ASSE=(
                       _F(CHAM_GD=__defvrppro,
                          NOM_CMP='EPXX',
                          NOM_CMP_RESU='DIVU',
                          TOUT='OUI',),),**motscles);

    if inst_coincident :
      nomres=CREA_RESU(OPERATION='AFFE',
                       TYPE_RESU='EVOL_VARC',
                       NOM_CHAM='DIVU',
                       AFFE=(_F(CHAM_GD=__divum,
                                INST=instm,),
                             _F(CHAM_GD=__divup,
                                INST=instp,),),);
    else :
      nomres=CREA_RESU(OPERATION='AFFE',
                       TYPE_RESU='EVOL_VARC',
                       NOM_CHAM='DIVU',
                       AFFE=(_F(CHAM_GD=__divum,
                                INST=instp,),
                             _F(CHAM_GD=__divup,
                                INST=INST,),),);
