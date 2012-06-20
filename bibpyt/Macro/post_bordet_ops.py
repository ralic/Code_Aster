#@ MODIF post_bordet_ops Macro  DATE 18/06/2012   AUTEUR DELMAS J.DELMAS 

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

def post_bordet_ops(self, TOUT, GROUP_MA, INST, PRECISION, CRITERE, NUME_ORDRE,
                    PROBA_NUCL, RESULTAT, PARAM, TEMP, COEF_MULT,**args):
    """Corps de POST_BORDET"""
    import numpy as NP
    import aster
    from Accas import _F
    from Cata.cata import fonction_sdaster, nappe_sdaster
    from Utilitai.Utmess import  UTMESS

    ier=0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)
    # On importe les definitions des commandes a utiliser dans la macro
    CREA_CHAMP      = self.get_cmd('CREA_CHAMP')
    CALC_CHAM_ELEM  = self.get_cmd('CALC_CHAM_ELEM')
    CALC_CHAMP      = self.get_cmd('CALC_CHAMP')
    CREA_TABLE      = self.get_cmd('CREA_TABLE')
    FORMULE         = self.get_cmd('FORMULE')
    CALC_TABLE      = self.get_cmd('CALC_TABLE')
    #
    # Definition du concept sortant dans le contexte de la macro
    self.DeclareOut('tabout', self.sd)
    #
    # Recuperation du modele a partir du resultat
    iret,ibid,n_modele = aster.dismoi('F','MODELE',RESULTAT.nom,'RESULTAT')
    n_modele = n_modele.rstrip()
    if len(n_modele) == 0 or n_modele == "#PLUSIEURS":
        UTMESS('F','RUPTURE1_58')
    model = self.get_concept(n_modele)

    # Dimension du modele
    iret, ndim, rbid = aster.dismoi('F','DIM_GEOM',model.nom,'MODELE')

    if iret == 1 or ndim == 23:
        UTMESS('F','RUPTURE1_57')

  #
  #Definition des formules pour le calcul de sigy plus tard
  #

    __MAXI = FORMULE(NOM_PARA=('T1'),VALE="""max(T1,0.)""")

  #
  #Calcul des grandeurs dont on a besoin : contrainte principale, def plastique et volume du pt de gauss
  #

  #Volume point de gauss
    __VOL_PG = CALC_CHAM_ELEM(MODELE=model,
                              TOUT='OUI',
                              OPTION='COOR_ELGA',)
    if GROUP_MA:
        GROUP_MA = list(GROUP_MA)
        vol = __VOL_PG.EXTR_COMP('W', GROUP_MA)
    elif TOUT:
        vol = __VOL_PG.EXTR_COMP('W',[])

#contrainte principale max et deformation plastique
    __RESU=CALC_CHAMP(
             RESULTAT=self['RESULTAT'],
             CRITERES='SIEQ_ELGA',
             DEFORMATION='EPSP_ELGA',)

#Recuperation de la liste des instants et des ordres de calcul
    list_ordre=__RESU.LIST_VARI_ACCES()['NUME_ORDRE']
    list_inst=__RESU.LIST_VARI_ACCES()['INST']

#
#On va travailler en ordre ; si l'utilisateur entre un instant, on va le transformer en ordre
    entree_instant=None
    if INST :
        if CRITERE=='ABSOLU':
            prec=PRECISION
        elif CRITERE=='RELATIF':
            prec=PRECISION*INST
        entree_instant=True
        n=0
        trouv=None
        while (n<len(list_inst) and not trouv):
            if (list_inst[n]+prec>=INST) and (list_inst[n]-prec<=INST):
                instant=list_inst[n]
                trouv=True
            n=n+1
        if not trouv:
            UTMESS('F','RUPTURE1_53',valr=INST,valk='utilise pour le calcul de Bordet')
    if entree_instant==True:
        index_ordre=list_inst.index(instant)
        nume_ordre=list_ordre[index_ordre]
    elif NUME_ORDRE:
        nume_ordre=NUME_ORDRE
        if nume_ordre not in list_ordre :
            UTMESS('F','RUPTURE0_51',vali=int(nume_ordre),valk='utilise pour le calcul de Bordet')
    #
    # Pour Bordet, il nous faut les champs a tous les instants jusqu'a l'instant considere
    EP=[[None  for j in range(6)] for i in range(nume_ordre+1)]     #tenseur des deformations plastiques
    EPEQ=[[None for j in range(0)] for i in range(nume_ordre+1)]   #deformation plastique equivalente
    EPEQM=[0.] * (nume_ordre + 1)  #deformation plastique equivalente a l'instant precedent
    PRIN=[None] * (nume_ordre + 1)
    EQ_BAR=[None] * (nume_ordre + 1)
    EQ_PT=[None] * (nume_ordre + 1)
    EQ_PT2=[None] * (nume_ordre + 1)
    PR_BAR=[None] * (nume_ordre + 1)
    DEP=[None] * (nume_ordre + 1)
    BORDTI=0. # valeur sans l'exposant final, sommee sur les instants
    BORDTT=[0.] * (nume_ordre + 1)  # valeur avec l'exposant, que l'on stocke dans la table a chaque instant
    PROBA=[0.] * (nume_ordre + 1)   # Probabilite de rupture par clivage

#LISTE DES PARAMETRES
    sig0=PARAM['SEUIL_REFE']
    sigth=PARAM['SIG_CRIT']
    sigref=PARAM['SIGM_REFE']
    m=PARAM['M']
    V0=PARAM['VOLU_REFE']
    if PROBA_NUCL=='OUI':
        ep0=PARAM['DEF_PLAS_REFE']
    elif PROBA_NUCL=='NON':
        ep0=0
    c_mult=COEF_MULT
#
#On va constuire des champs a chaque instant
#
    if list_ordre[0]==0:
        fin_ordre=nume_ordre+1
    elif list_ordre[0]!=0:
        fin_ordre=nume_ordre
    for ordre in range(list_ordre[0],fin_ordre):
#
#Temperature a extraire : soit une fonction du temps, soit un reel
#
        if type(TEMP)==fonction_sdaster:
            tempe=TEMP(list_inst[ordre])
        elif type(TEMP)!=fonction_sdaster:
            tempe=TEMP

        def fseuil(epsi):
            return PARAM['SEUIL_CALC'](epsi, tempe)

        self.update_const_context({'fseuil' : fseuil})
        __NPY=FORMULE(NOM_PARA=('EPSI'), VALE="""fseuil(EPSI)""")

#
#On met ces grandeurs dans des champs specifiques
#
        __S_TOT=CREA_CHAMP(TYPE_CHAM='ELGA_SIEF_R',
                  RESULTAT=__RESU,
                  OPERATION='EXTR',
                  NUME_ORDRE=ordre,
                  NOM_CHAM='SIEQ_ELGA',)

        __EPSP=CREA_CHAMP(TYPE_CHAM='ELGA_EPSI_R',
                  RESULTAT=__RESU,
                  OPERATION='EXTR',
                  NUME_ORDRE=ordre,
                  NOM_CHAM='EPSP_ELGA',)

        # On recupere la valeur des champs au niveau des groupes qui nous interessent
        if GROUP_MA:
            PRIN[ordre]=__S_TOT.EXTR_COMP('PRIN_3',GROUP_MA,0).valeurs

            # Pour la deformation plastique, on construit de quoi calculer sa norme de VMises
            EP[ordre][0]=__EPSP.EXTR_COMP('EPXX',GROUP_MA,0).valeurs
            EP[ordre][1]=__EPSP.EXTR_COMP('EPYY',GROUP_MA,0).valeurs
            EP[ordre][2]=__EPSP.EXTR_COMP('EPZZ',GROUP_MA,0).valeurs
            EP[ordre][3]=__EPSP.EXTR_COMP('EPXY',GROUP_MA,0).valeurs
            if ndim==3:
                EP[ordre][4]=EPSP[ordre].EXTR_COMP('EPXZ',GROUP_MA,0).valeurs
                EP[ordre][5]=EPSP[ordre].EXTR_COMP('EPYZ',GROUP_MA,0).valeurs

        elif TOUT:
            PRIN[ordre]=__S_TOT.EXTR_COMP('PRIN_3',[],0).valeurs
            EP[ordre][0]=__EPSP.EXTR_COMP('EPXX',[],0).valeurs
            EP[ordre][1]=__EPSP.EXTR_COMP('EPYY',[],0).valeurs
            EP[ordre][2]=__EPSP.EXTR_COMP('EPZZ',[],0).valeurs
            EP[ordre][3]=__EPSP.EXTR_COMP('EPXY',[],0).valeurs
            if ndim==3:
                EP[ordre][4]=__EPSP.EXTR_COMP('EPXZ',[],0).valeurs
                EP[ordre][5]=__EPSP.EXTR_COMP('EPYZ',[],0).valeurs

        nval=len(PRIN[ordre])
        nval2=len(EP[ordre][0])
        if nval2 != nval:
            UTMESS('F','RUPTURE1_54')

        if ndim==3:
            EPEQ[ordre]=NP.sqrt(2./3.*(EP[ordre][0]**2 + EP[ordre][1]**2 + EP[ordre][2]**2 \
                                + 2.*EP[ordre][3]**2 + 2.*EP[ordre][4]**2 + 2.*EP[ordre][5]**2))
        elif ndim==2:
            EPEQ[ordre]=NP.sqrt(2./3.*(EP[ordre][0]**2 + EP[ordre][1]**2 + EP[ordre][2]**2 + 2.*EP[ordre][3]**2))

        # Construction des champs barre et des champs de vitesse
        EQ_PT2[list_ordre[0]]=NP.zeros([nval])
        EPEQ[ordre]=NP.array(EPEQ[ordre])

        if ordre != list_ordre[0]:
            dt=list_inst[ordre]-list_inst[ordre-1]
            if dt==0 : UTMESS('F','RUPTURE1_55')
            EPEQM[ordre]=EPEQ[ordre-1]
            EQ_BAR[ordre]=(EPEQ[ordre] +EPEQ[ordre-1])/2.
            EQ_PT2[ordre]=(EPEQ[ordre]-EPEQ[ordre-1])/(2*dt)
            EQ_PT[ordre]=EQ_PT2[ordre-1]+EQ_PT2[ordre]
            DEP[ordre]=EPEQ[ordre]-EPEQ[ordre-1]
            PR_BAR[ordre]=(PRIN[ordre]+PRIN[ordre-1])/2.

            if type(PARAM['SEUIL_CALC'])==fonction_sdaster:
                sigy=PARAM['SEUIL_CALC'](tempe)
            elif type(PARAM['SEUIL_CALC'])==nappe_sdaster:
                EQ_PT[ordre]=list(EQ_PT[ordre])
                __TAB=CREA_TABLE(LISTE=(
                                       _F(PARA='EPSI',LISTE_R=EQ_PT[ordre],),
                                       ),
                                )
                __TAB=CALC_TABLE(TABLE = __TAB,
                                 reuse =__TAB,
                                 ACTION=_F(OPERATION='OPER',
                                           FORMULE=__NPY,
                                           NOM_PARA='TSIGY'),)
                sigy=__TAB.EXTR_TABLE().values()['TSIGY']
                sigy=NP.array(sigy)

            T1=sigy/sig0*(PR_BAR[ordre]**m-sigth**m)
            T1=list(T1)
            __TABT1=CREA_TABLE(LISTE=(
                                      _F(PARA='T1',LISTE_R=T1,),
                                      )
                              )
            __TABT1=CALC_TABLE(TABLE = __TABT1,
                                reuse =__TABT1,
                                ACTION=_F(OPERATION='OPER',
                                          FORMULE=__MAXI,
                                          NOM_PARA='T1BIS'),)

            T1 = __TABT1.EXTR_TABLE().values()['T1BIS']
            T1 = NP.array(T1)
            if PROBA_NUCL == 'OUI':
                T2=NP.exp(-sigy/sig0*EQ_BAR[ordre]/ep0)
            elif PROBA_NUCL=='NON':
                T2=1.
            T3=DEP[ordre]
            T4=vol.valeurs/V0
            BORDTI = BORDTI + NP.cumsum(T1*T2*T3*T4)[-1]

        BORDTT[ordre]=(c_mult*BORDTI)**(1/m)

        if sigref(tempe)!=0.:
            PROBA[ordre]=1-NP.exp(-(BORDTT[ordre]/sigref(tempe))**m)
        elif sigref(tempe)==0.:
            UTMESS('F','RUPTURE1_56',valr=list_inst[ordre])

    tabout=CREA_TABLE(LISTE=(
                   _F(PARA='INST', LISTE_R=list_inst[0:nume_ordre+1]),
                   _F(PARA='SIG_BORDET', LISTE_R=BORDTT,),
                   _F(PARA='PROBA_BORDET', LISTE_R=PROBA,),
                   ),)
    return ier
