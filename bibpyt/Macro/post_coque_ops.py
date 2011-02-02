#@ MODIF post_coque_ops Macro  DATE 02/02/2011   AUTEUR PELLET J.PELLET 

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

def post_coque_ops(self,RESULTAT,COOR_POINT,CHAM,NUME_ORDRE,INST,
                     **args):
    """
    macro post_coque
    """
    import aster
    import os, string
    import Accas 
    from Accas import _F
    from Utilitai.Utmess import  UTMESS, MasquerAlarme, RetablirAlarme
    from Utilitai.Table      import Table
    from Noyau.N_utils import AsType
    ier=0

    # On importe les definitions des commandes a utiliser dans la macro
    MACR_LIGN_COUPE  =self.get_cmd('MACR_LIGN_COUPE')
    CREA_CHAMP      =self.get_cmd('CREA_CHAMP')
    CREA_TABLE      =self.get_cmd('CREA_TABLE')
    CALC_ELEM       =self.get_cmd('CALC_ELEM')


    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)
    MasquerAlarme('MODELISA4_9')
    
  # Le concept sortant (de type table) est nomme
  # 'tabout' dans le contexte de la macro
  
    self.DeclareOut('tabout',self.sd)
    assert AsType(RESULTAT).__name__ in ('evol_elas','evol_noli',)
    dico = RESULTAT.LIST_CHAMPS()
    dico2 = RESULTAT.LIST_VARI_ACCES()
  # si ni INST ni NUME_ORDRE ne sont presents, on prend le premier 
  # instant calcule 
    if not INST and not NUME_ORDRE :
       INST = dico2['INST'][0]
    iret,ibid,n_modele = aster.dismoi('F','MODELE',RESULTAT.nom,'RESULTAT')
    MODEL = self.get_concept(n_modele)
    if INST != 0.0 :
      if not INST in dico2['INST'] :
          UTMESS('F','POST0_20',valr=INST)
    else :
      if not NUME_ORDRE in dico['SIEF_ELNO'] :
        if NUME_ORDRE in dico['DEPL'] :        
          CALC_ELEM(RESULTAT=RESULTAT,reuse=RESULTAT,OPTION='SIEF_ELNO',
                    NUME_ORDRE=NUME_ORDRE)
        else :
          UTMESS('F','POST0_19',vali=NUME_ORDRE)
    dico = RESULTAT.LIST_CHAMPS()

  # Calcul des deformations sur les peaux inf et sup :
    if CHAM=='DEFORMATION':
      if NUME_ORDRE:
        __ressup=CALC_ELEM(RESULTAT=RESULTAT,
                  OPTION='EPSI_ELNO',REPE_COQUE=_F(NIVE_COUCHE='SUP'),
                  NUME_ORDRE=NUME_ORDRE)
        __resinf=CALC_ELEM(RESULTAT=RESULTAT,
                  OPTION='EPSI_ELNO',REPE_COQUE=_F(NIVE_COUCHE='INF'),
                  NUME_ORDRE=NUME_ORDRE)
        __epssup=CREA_CHAMP(RESULTAT=__ressup,TYPE_CHAM='ELNO_EPSI_R',
                  OPERATION='EXTR',NOM_CHAM='EPSI_ELNO',
                  NUME_ORDRE=NUME_ORDRE)
        __epsinf=CREA_CHAMP(RESULTAT=__resinf,TYPE_CHAM='ELNO_EPSI_R',
                  OPERATION='EXTR',NOM_CHAM='EPSI_ELNO',
                  NUME_ORDRE=NUME_ORDRE)
      else :
        __ressup=CALC_ELEM(RESULTAT=RESULTAT,
                  OPTION='EPSI_ELNO',REPE_COQUE=_F(NIVE_COUCHE='SUP'),
                  INST=INST)
        __resinf=CALC_ELEM(RESULTAT=RESULTAT,
                  OPTION='EPSI_ELNO',REPE_COQUE=_F(NIVE_COUCHE='INF'),
                  INST=INST)
        __epssup=CREA_CHAMP(RESULTAT=__ressup,TYPE_CHAM='ELNO_EPSI_R',
                  OPERATION='EXTR',NOM_CHAM='EPSI_ELNO',
                  INST=INST)
        __epsinf=CREA_CHAMP(RESULTAT=__resinf,TYPE_CHAM='ELNO_EPSI_R',
                  OPERATION='EXTR',NOM_CHAM='EPSI_ELNO',
                  INST=INST)

                                                                     
    # Appel MACR_LIGN_COUPE :
    motscles={}
    if   CHAM=='EFFORT'      : motscles['NOM_CHAM']   ='SIEF_ELNO'
    if   CHAM=='DEFORMATION' : motscles['NOM_CHAM']   ='EPSI_ELNO'

    if CHAM=='EFFORT' :
      motscles['LIGN_COUPE']=[]
      iocc=0
      for m in COOR_POINT:
        iocc=iocc+1
        lst=m['COOR']
        if len(lst)==4 :
          if lst[3]!=0. :
            UTMESS('A','POST0_21',vali=iocc,valr=lst[3])
          lst=lst[0:3]
        motscles['LIGN_COUPE'].append(_F(TYPE='SEGMENT',
                                 NB_POINTS=2,
                                 COOR_ORIG=lst,
                                 COOR_EXTR=lst,
                                 DISTANCE_MAX=10.0,),)
      __tabl=MACR_LIGN_COUPE(RESULTAT=RESULTAT,**motscles)

    if CHAM=='DEFORMATION' :
      tab2=Table()
      iocc=0
      for m in COOR_POINT:
        motscles['LIGN_COUPE']=[]
        iocc=iocc+1
        lst=m['COOR']
        if len(lst)!=4 :
          UTMESS('F','POST0_22',vali=iocc,)
        elif (lst[3]>1.) or (lst[3]<-1.) :
          UTMESS('F','POST0_23',vali=iocc,valr=lst[3],)
        coesup=0.5+lst[3]*0.5
        coeinf=0.5-lst[3]*0.5
        __epsz=CREA_CHAMP(TYPE_CHAM='ELNO_EPSI_R',OPERATION='ASSE',
                MODELE=MODEL,
                ASSE=(
                _F(TOUT='OUI',
                   CHAM_GD=__epsinf,
                   CUMUL='OUI',
                   COEF_R=coeinf,),
                _F(TOUT='OUI',
                   CHAM_GD=__epssup,
                   CUMUL='OUI',
                   COEF_R=coesup,),),);
        lst=lst[0:3]
        motscles['LIGN_COUPE'].append(_F(TYPE='SEGMENT',
                                 NB_POINTS=2,
                                 COOR_ORIG=lst,
                                 COOR_EXTR=lst,
                                 DISTANCE_MAX=10.0,),)
        __tab=MACR_LIGN_COUPE(CHAM_GD=__epsz,MODELE=MODEL,**motscles)
        __tab2=__tab.EXTR_TABLE()
        for ligne in __tab2:
          tab2.append(ligne)
        tab2=tab2[__tab2.para]

    if CHAM=='EFFORT' :
      tab2=__tabl.EXTR_TABLE()
      tab3=(tab2.INST==INST)
      tab2=tab3

    tab4=Table()
    ilig=0
    for ligne in tab2:
       ilig=ilig+1
       if(ilig%2)==0:
         tab4.append(ligne)       
    tab4=tab4[tab2.para]
    dprod = tab4.dict_CREA_TABLE()
    tabout = CREA_TABLE(TYPE_TABLE='TABLE',
                       **dprod)
    RetablirAlarme('MODELISA4_9')
    return ier                                                           

