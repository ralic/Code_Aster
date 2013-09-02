# coding=utf-8
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


# person_in_charge: aimery.assire at edf.fr

def calc_precont_ops(self,reuse,MODELE,CHAM_MATER,CARA_ELEM,EXCIT,
                                CABLE_BP,CABLE_BP_INACTIF,
                                COMP_INCR,ETAT_INIT,METHODE,ENERGIE,
                                RECH_LINEAIRE,CONVERGENCE,INCREMENT,SOLVEUR,
                                INFO,TITRE,**args):
  """
     Ecriture de la macro CALC_PRECONT
  """
  import copy
  import aster
  import string
  import types
  from Accas import _F
  from Cata.cata import listr8_sdaster, list_inst
  from Noyau.N_utils import AsType
  from Noyau.N_types import is_sequence
  from Utilitai.Utmess     import  UTMESS, MasquerAlarme, RetablirAlarme
  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  AFFE_MODELE      = self.get_cmd('AFFE_MODELE')
  CREA_CHAMP       = self.get_cmd('CREA_CHAMP')
  AFFE_CHAR_MECA   = self.get_cmd('AFFE_CHAR_MECA')
  DEFI_LIST_REEL   = self.get_cmd('DEFI_LIST_REEL')
  STAT_NON_LINE    = self.get_cmd('STAT_NON_LINE')
  CALC_CHAMP       = self.get_cmd('CALC_CHAMP')
  CREA_CHAMP       = self.get_cmd('CREA_CHAMP')
  DEFI_FONCTION    = self.get_cmd('DEFI_FONCTION')
  RECU_TABLE       = self.get_cmd('RECU_TABLE')
  DEFI_MATERIAU    = self.get_cmd('DEFI_MATERIAU')
  AFFE_MATERIAU    = self.get_cmd('AFFE_MATERIAU')
  IMPR_TABLE        = self.get_cmd('IMPR_TABLE')
  DETRUIRE         = self.get_cmd('DETRUIRE')
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # Le concept sortant (de type evol_noli) est nomme RES dans
  # le contexte de la macro

  self.DeclareOut('RES',self.sd)

  # alarme de STAT_NON_LINE si les mot-cles de COMP_INCR sont renseignes a tort
  MasquerAlarme('COMPOR1_70')

  # -------------------------------------------------------------
  # 1. CREATION DES MOTS-CLES ET CONCEPTS POUR LES STAT_NON_LINE
  # ------------------------------------------------------------


  # 1.1 Recuperation de la liste d'instants, de l'instant initial et final
  #     Creation de la nouvelle liste d'instants
  # ----------------------------------------------------------

  dIncrement=INCREMENT[0].cree_dict_valeurs(INCREMENT[0].mc_liste)
  __prec = dIncrement['PRECISION']

  __L0   = dIncrement['LIST_INST']

  if   type(__L0) == listr8_sdaster:
  #cas où liste definie par DEFI_LIST_REEL
    __L1   = __L0.Valeurs()
  elif type(__L0) == list_inst:
  # cas où liste definie par DEFI_LIST_INST
    tmp = __L0.get_name().ljust(8) + '.LIST.' + 'DITR'.ljust(18)
    __L1 = aster.getvectjev(tmp)

  # Traitement de l'etat initial
  if ETAT_INIT:
      dEtatInit=ETAT_INIT[0].cree_dict_valeurs(ETAT_INIT[0].mc_liste)
      for i in dEtatInit.keys():
          if dEtatInit[i]==None : del dEtatInit[i]

      __EVINIT = dEtatInit['EVOL_NOLI']
  else :
      dEtatInit=None

  # Teste si INST_INIT est donné ou bien recalcule __TMIN
  if dIncrement['INST_INIT'] == None:
    if self.reuse == None:
      __TMIN = __L1[0]
    else:
      __dico = __EVINIT.LIST_VARI_ACCES()
      __TMIN = __dico['INST'][-1]
  else:
    __TMIN = dIncrement['INST_INIT']

  # Teste si INST_FIN est donné ou bien recalcule __TMAX
  if dIncrement['INST_FIN'] == None:
    __TMAX = __L1[-1]
  else:
    __TMAX = dIncrement['INST_FIN']

  # Teste si INST_INIT est bien plus petit que INST_FIN
  if __TMAX <= __TMIN:
    UTMESS('F','CABLE0_1')

  # Preparation de  la liste d'instant __L2 allant de __TMIN a __TMAX
  # et preparation des instants supplementaire __TINT et __TINT2
  __L2=[]
  for m in __L1:
    if m>=__TMIN and m<=__TMAX:
      __L2.append(m)

  __TINT = (9.*__L2[-1] + __L2[-2])/10.
  __TINT2 = (9.5*__L2[-1] + .5*__L2[-2])/10.

  # cas ADHERENT ou non
  ii = 0
  for mcabl in CABLE_BP :
      __TCAB1 = RECU_TABLE(CO=mcabl, NOM_TABLE='CABLE_GL')
      table_cable=__TCAB1.EXTR_TABLE()
      __adher    = table_cable.ADHERENT.values()[0]
      if ii==0 :
          adher = __adher
      elif ii!=0 and __adher!= adher:
          UTMESS('F','CABLE0_3')
      ii+=1

      DETRUIRE(CONCEPT=_F(NOM=__TCAB1))

  adher = adher.strip()

  if (adher == 'OUI') :
    __MOD = string.ljust(MODELE.nom,8)
    __MOD1 =__MOD+'.MODELE    .LGRF        '
    __LMAIL = aster.getvectjev(__MOD1)
    __MAIL  = string.strip(__LMAIL[0])
    __MOD1 =__MOD+'.MAILLE      '
    __typeMaille = aster.getvectjev(__MOD1)
    __teNomte    = aster.getvectjev('&CATA.TE.NOMTE')

    __frott = False
    for iel in __typeMaille :
      if string.strip(__teNomte[iel-1]) == 'MECGSEG3' :
        __frott = True
        break

    if __frott :
      __modCable = "CABLE_GAINE"
    else :
      __modCable = "BARRE"

    # finalisation liste instants
    __L2[-1:-1] = [__TINT]

    # __LST0 est la liste d'instants utilisée pour l'etape 1
    __LST0=DEFI_LIST_REEL( DEBUT = __TMIN,
                      INTERVALLE = _F(JUSQU_A = __TMAX, NOMBRE = 1),)

    # __LST et __FCT sont utilisés pour les etapes 2 et 3
    __LST=DEFI_LIST_REEL(VALE=__L2,);
    __FCT=DEFI_FONCTION(INTERPOL=('LIN','LIN'),
                          NOM_PARA='INST',
                          VALE=(__TMIN,0.0,__TINT,1.0,__TMAX,1.0),);
    for i in dIncrement.keys():
        if dIncrement[i]==None : del dIncrement[i]
    dIncrement['LIST_INST']= __LST
    dIncrement['INST_FIN'] = __TINT


    # 1.2 Recuperation des parametres pour STAT_NON_LINE
    # -------------------------------------------------------
    motscle4={}
    motscle5={}

    if METHODE == 'NEWTON':
      motscle4['NEWTON']=args['NEWTON'].List_F()
      motscle5['NEWTON']=args['NEWTON'].List_F()
  #     for j in dNewton.keys():
  #       if dNewton[j]==None : del dNewton[j]


    dConvergence=CONVERGENCE[0].cree_dict_valeurs(CONVERGENCE[0].mc_liste)
    for i in dConvergence.keys():
        if dConvergence[i]==None : del dConvergence[i]

    dSolveur=SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
    for i in dSolveur.keys():
        if dSolveur[i]==None : del dSolveur[i]

    if RECH_LINEAIRE:
      dRech_lin=RECH_LINEAIRE[0].cree_dict_valeurs(RECH_LINEAIRE[0].mc_liste)
      for i in dRech_lin.keys():
          if dRech_lin[i]==None : del dRech_lin[i]
    else :
      dRech_lin=None

    if ENERGIE:
      dEnergie=ENERGIE[0].cree_dict_valeurs(ENERGIE[0].mc_liste)
      motscle4['ENERGIE']=dEnergie
      motscle5['ENERGIE']=dEnergie


    # 1.3 Creation des mots-cles pour les 3 AFFE_CHAR_MECA
    #     Recuperation des cables dans les concepts CABLE_BP
    #     et CABLE_BP_INACTIF
    # ------------------------------------------------------
    if type(CABLE_BP) is not types.NoneType:
      if not is_sequence(CABLE_BP):
        CABLE_BP0 = CABLE_BP
        CABLE_BP = []
        CABLE_BP.append ( CABLE_BP0 )

    if type(CABLE_BP_INACTIF) is not types.NoneType:
      if not is_sequence(CABLE_BP_INACTIF):
        CABLE_BP_INACTIF0 = CABLE_BP_INACTIF
        CABLE_BP_INACTIF = []
        CABLE_BP_INACTIF.append ( CABLE_BP_INACTIF0 )

    motscles={}
    motscles['RELA_CINE_BP']=[]
    motscle2={}
    motscle2['RELA_CINE_BP']=[]
    motscle3={}
    motscle3['RELA_CINE_BP']=[]
    set_GROUP_MA_A = set()
    for mcabl in CABLE_BP:
      print "etienne ",mcabl
      # Creation de mots-cles pour les AFFE_CHAR_MECA
      motscles['RELA_CINE_BP'].append(_F(CABLE_BP=mcabl,
                                        SIGM_BPEL = 'OUI',
                                        RELA_CINE = 'NON',) )
      motscle2['RELA_CINE_BP'].append(_F(CABLE_BP=mcabl,
                                        SIGM_BPEL = 'NON',
                                        RELA_CINE = 'OUI',) )
      motscle3['RELA_CINE_BP'].append(_F(CABLE_BP=mcabl,
                                        SIGM_BPEL = 'OUI',
                                        RELA_CINE = 'OUI',) )

      # Creation de __GROUP_MA_A : liste des noms des cables contenus
      # dans chaque concept CABLE_BP = cables  a activer
      __TCAB = RECU_TABLE(CO=mcabl, NOM_TABLE='CABLE_BP')
      col_nom_cable = __TCAB.EXTR_TABLE().NOM_CABLE
      set_GROUP_MA_A.update(col_nom_cable.values())
    __GROUP_MA_A = list(set_GROUP_MA_A)

    # Creation de __GROUP_MA_I : liste des noms des cables contenus
    # dans chaque CABLE_BP_INACTIF
    # __GROUP_MA_CABLE = liste des cables actifs et inactifs
    set_GROUP_MA_I = set()

    if CABLE_BP_INACTIF:
      motscle6={}
      motscle6['RELA_CINE_BP']=[]
      for mcabl in CABLE_BP_INACTIF:
        __TCA0 = RECU_TABLE(CO=mcabl, NOM_TABLE='CABLE_BP')
        col_nom_cable = __TCA0.EXTR_TABLE().NOM_CABLE
        set_GROUP_MA_I.update(col_nom_cable.values())

        # Creation de mots-cles pour les AFFE_CHAR_MECA
        motscle6['RELA_CINE_BP'].append(_F(CABLE_BP=mcabl,
                                          SIGM_BPEL = 'NON',
                                          RELA_CINE = 'OUI',) )
    __GROUP_MA_I = list(set_GROUP_MA_I)
    __GROUP_MA_CABLES = __GROUP_MA_A + __GROUP_MA_I


    # 1.4 Creation des mots-clés facteurs COMP_INCR
    # pour étape 2 (dComp_incr0) et étape 3 (dComp_incr1)
    # ------------------------------------------------------

    dComp_incr=[]
    for j in COMP_INCR :
        dComp_incr.append(j.cree_dict_valeurs(j.mc_liste))
        for i in dComp_incr[-1].keys():
            if dComp_incr[-1][i]==None : del dComp_incr[-1][i]

    PARM_THETA=0.
    for j in range(len(COMP_INCR)) :
      if dComp_incr[j]['RELATION'] == 'ELAS':
          PARM_THETA=dComp_incr[j]['PARM_THETA']

    if PARM_THETA == 0:
      PARM_THETA=dComp_incr[0]['PARM_THETA']

    if __frott:
      dComp_incrElas={'RELATION' : 'KIT_CG', 'TOUT' : 'OUI', 'PARM_THETA' : PARM_THETA}
      dComp_incrElas['RELATION_KIT']=('ELAS','CABLE_GAINE_FROT',)
    else :
      dComp_incrElas={'RELATION' : 'ELAS', 'TOUT' : 'OUI', 'PARM_THETA' : PARM_THETA}

    dComp_incr0=copy.copy(dComp_incr)
    dComp_incr1=copy.copy(dComp_incr)

    if __frott :
      dComp_incr0.append(_F(RELATION='KIT_CG',
                            RELATION_KIT=('SANS','CABLE_GAINE_FROT',),
                            GROUP_MA=__GROUP_MA_CABLES,) )
      if __GROUP_MA_I:
        dComp_incr1.append(_F(RELATION='KIT_CG',
                            RELATION_KIT=('SANS','CABLE_GAINE_FROT',),
                              GROUP_MA=__GROUP_MA_I,) )

    else :
      dComp_incr0.append(_F(RELATION='SANS',GROUP_MA=__GROUP_MA_CABLES,) )
      if __GROUP_MA_I:
        dComp_incr1.append(_F(RELATION='SANS',GROUP_MA=__GROUP_MA_I,) )


    # 1.5 Modele contenant uniquement les cables de precontrainte
    # ---------------------------------------------------------


    objma=self.get_sd_avant_etape(__MAIL,self)

    __M_CA=AFFE_MODELE( MAILLAGE=objma,
                        AFFE    =_F( GROUP_MA     = __GROUP_MA_A,
                                      PHENOMENE    = 'MECANIQUE',
                                      MODELISATION = __modCable) )


    # 1.6 Blocage de tous les noeuds des cables actifs
    # --------------------------------------------------

    _B_CA=AFFE_CHAR_MECA(MODELE=__M_CA,
                          DDL_IMPO= _F( GROUP_MA = __GROUP_MA_A,
                                        DX = 0.,
                                        DY = 0.,
                                        DZ = 0.,),)

    # 1.7 Chargements concernant les cables
    # -------------------------------------
    _C_CN=AFFE_CHAR_MECA(MODELE=__M_CA,**motscles)
    _C_CA=AFFE_CHAR_MECA(MODELE=MODELE,**motscle2)
    _C_CT=AFFE_CHAR_MECA(MODELE=MODELE,**motscle3)
    if CABLE_BP_INACTIF:
      _C_CI=AFFE_CHAR_MECA(MODELE=MODELE,**motscle6)



    # -------------------------------------------------------------
    # 2. CALCULS
    # ------------------------------------------------------------


    #-------------------------------------------------------------------
    # 2.1 Premiere etape : calcul sur le(s) cable(s) et
    #     recuperation des _F_CAs aux noeuds
    #     on travaile entre tmin et tmax
    #-------------------------------------------------------------------

    __EV1=STAT_NON_LINE(
                          MODELE     = __M_CA,
                          CHAM_MATER = CHAM_MATER,
                          CARA_ELEM  = CARA_ELEM,
                          EXCIT      =(_F(CHARGE = _B_CA),
                                        _F(CHARGE = _C_CN),),
                          COMP_INCR  =dComp_incrElas,
                          INCREMENT  =_F(LIST_INST = __LST0,
                                          PRECISION = __prec),
                          SOLVEUR = dSolveur,
                          INFO     =INFO,
                          TITRE = TITRE,  )
    __EV1 = CALC_CHAMP ( reuse    = __EV1,
                        RESULTAT = __EV1,
                      # GROUP_MA = __GROUP_MA_A,
                          FORCE  = 'FORC_NODA' )

    __REA = CREA_CHAMP (
                      TYPE_CHAM = 'NOEU_DEPL_R',
                      OPERATION = 'EXTR',
                      RESULTAT  =  __EV1,
                      NOM_CHAM  = 'FORC_NODA',
                      INST      = __TMAX);

    __REAC = CREA_CHAMP (TYPE_CHAM='NOEU_DEPL_R',
                      OPERATION = 'ASSE',
                      MODELE    = MODELE,
                      ASSE= _F(GROUP_MA=__GROUP_MA_A,
                                CHAM_GD=__REA,
                                COEF_R = -1.), )

    _F_CA=AFFE_CHAR_MECA(MODELE=__M_CA,
                            VECT_ASSE = __REAC )



    #-----------------------------------------------------------------------
    # 2.2 Deuxieme etape : application de la precontrainte sur le beton
    #     en desactivant les cables
    #-----------------------------------------------------------------------

    # Regeneration des mots-cles EXCIT passés en argument de la macro
    dExcit=[]
    for j in EXCIT :
        dExcit.append(j.cree_dict_valeurs(j.mc_liste))
        for i in dExcit[-1].keys():
            if dExcit[-1][i]==None : del dExcit[-1][i]

    if CABLE_BP_INACTIF:
      dExcit.append(_F(CHARGE=_C_CI),)

    # Creation du mots-cle EXCIT pour le STAT_NON_LINE
    dExcit1=copy.copy(dExcit)
    dExcit1.append(_F(CHARGE=_C_CA),)
    dExcit1.append(_F(CHARGE = _F_CA,
                      FONC_MULT=__FCT ),)


    if self.reuse:
      motscle4['reuse'] = self.reuse

    RES=STAT_NON_LINE(
                      MODELE      =MODELE,
                      CARA_ELEM   =CARA_ELEM,
                      CHAM_MATER  = CHAM_MATER,
                      COMP_INCR=dComp_incr0,
                      INCREMENT=dIncrement,
                      ETAT_INIT = dEtatInit,
                      METHODE=METHODE,
                      CONVERGENCE=dConvergence,
                      RECH_LINEAIRE = dRech_lin,
                      SOLVEUR = dSolveur,
                      ARCHIVAGE = _F(INST = __TINT),
                      INFO     =INFO,
                      TITRE = TITRE,
                      EXCIT = dExcit1,
                      **motscle4)

    # Recuperation du dernier numero d'ordre pour pouvoir  l'écraser dans RES
    __dico2 = RES.LIST_VARI_ACCES()
    __no = __dico2['NUME_ORDRE'][-1]


    #-----------------------------------------------------------------------
    # 2.2 Troisieme etape : on remet la tension dans les cables
    #-----------------------------------------------------------------------

    # Creation du mots-cles EXCIT pour le STAT_NON_LINE
    dExcit2=copy.copy(dExcit)
    dExcit2.append(_F(CHARGE=_C_CT,) )

    # Calcul sur un seul pas (de __TINT a __TMAX)
    RES=STAT_NON_LINE( reuse      = RES,
                      ETAT_INIT  = _F(EVOL_NOLI =RES),
                      MODELE     = MODELE,
                      CHAM_MATER = CHAM_MATER,
                      CARA_ELEM  = CARA_ELEM,

                      COMP_INCR=dComp_incr1,
                      INCREMENT=_F(LIST_INST = __LST,
                                    PRECISION = __prec),
                      METHODE=METHODE,
  #                     NEWTON =dNewton,
  #                     IMPLEX=dImplex,

                      RECH_LINEAIRE = dRech_lin,
                      CONVERGENCE=dConvergence,
                      SOLVEUR = dSolveur,
                      INFO  =INFO,
                      TITRE = TITRE,
                      EXCIT =dExcit2,
                      **motscle5
                      )
  elif adher =='NON' :
    motscle4={}
    motscle2={}
    motscle2={}
    motscle2['DDL_IMPO'] = []
    motscle2['RELA_CINE_BP'] = []
    motscle2a={}
    motscle2a['DDL_IMPO'] = []
    motscle2a['RELA_CINE_BP'] = []
    motscle2b={}
    motscle2b['DDL_IMPO'] = []
    motscle2b['RELA_CINE_BP'] = []
    motscle5={}
    motscle5['DDL_IMPO'] = []
    motscle5['RELA_CINE_BP'] = []
    motscle3={}
    motscle3['AFFE'] = []
    motscle3a={}
    motscle3a['AFFE'] = []
    motscle3b={}
    motscle3b['AFFE'] = []
    motscle6={}
    motscle6['DDL_IMPO'] = []
    __ActifActif = False
    if self.reuse:
      motscle4['reuse'] = self.reuse
    #assert (len(CABLE_BP) == 1)
    for mcabl in CABLE_BP :
      __TCAB1 = RECU_TABLE(CO=mcabl, NOM_TABLE='CABLE_GL')

      motscle2['RELA_CINE_BP'].append(_F(CABLE_BP=mcabl,
                                        SIGM_BPEL = 'NON',
                                        RELA_CINE = 'OUI',) )
      motscle5['RELA_CINE_BP'].append(_F(CABLE_BP=mcabl,
                                        SIGM_BPEL = 'NON',
                                        RELA_CINE = 'OUI',) )

      set_nume_cable=set()
      set_nume_cable.update(__TCAB1.EXTR_TABLE().NUME_CABLE.values())
      nb_cable = len(set_nume_cable)
      table_cable=__TCAB1.EXTR_TABLE()

      for icable in xrange(nb_cable) :

        __typ_ancr = (table_cable.TYPE_ANCRAGE1.values()[icable],table_cable.TYPE_ANCRAGE2.values()[icable])
        __typ_noeu = (table_cable.TYPE_NOEUD1.values()[icable],table_cable.TYPE_NOEUD2.values()[icable])
        __nom_noeu = (table_cable.NOM_ANCRAGE1.values()[icable],table_cable.NOM_ANCRAGE2.values()[icable])
        __tension  = table_cable.TENSION.values()[icable]
        __recul    = table_cable.RECUL_ANCRAGE.values()[icable]
        __recul_exists = (__recul != 0)

        actif=0
        for j in range(2) :
          if string.strip(__typ_ancr[j]) == 'PASSIF' :
            if string.strip(__typ_noeu[j]) == 'NOEUD' :
              motscle2['DDL_IMPO'].append(_F(NOEUD=string.strip(__nom_noeu[j]),
                                        GLIS = 0.) )
            else :
              motscle2['DDL_IMPO'].append(_F(GROUP_NO=string.strip(__nom_noeu[j]),
                                     GLIS = 0.) )
          else :
            actif += 1
            if string.strip(__typ_noeu[j]) == 'NOEUD' :
              motscle3['AFFE'].append(_F(NOEUD=string.strip(__nom_noeu[j]),NOM_CMP='GLIS',
                                        VALE = __tension*(-1)**(j+1)) )
              if j==0 :
                motscle3b['AFFE'].append(_F(NOEUD=string.strip(__nom_noeu[j]),NOM_CMP='GLIS',
                                        VALE = __tension*(-1)**(j+1)) )
                motscle2a['DDL_IMPO'].append(_F(NOEUD=string.strip(__nom_noeu[j]),
                                        GLIS = 0.) )
              else :
                motscle3a['AFFE'].append(_F(NOEUD=string.strip(__nom_noeu[j]),NOM_CMP='GLIS',
                                        VALE = __tension*(-1)**(j+1)) )
                motscle2b['DDL_IMPO'].append(_F(NOEUD=string.strip(__nom_noeu[j]),
                                        GLIS = 0.) )
              if __recul_exists :
                motscle5['DDL_IMPO'].append(_F(NOEUD=string.strip(__nom_noeu[j]),
                                            GLIS = __recul*(-1)**(j+1)) )
            else :
              motscle3['AFFE'].append(_F(GROUP_NO=string.strip(__nom_noeu[j]),NOM_CMP='GLIS',
                                     VALE = __tension*(-1)**(j+1)) )
              if j==0 :
                motscle3b['AFFE'].append(_F(GROUP_NO=string.strip(__nom_noeu[j]),NOM_CMP='GLIS',
                                        VALE = __tension*(-1)**(j+1)) )
                motscle2a['DDL_IMPO'].append(_F(GROUP_NO=string.strip(__nom_noeu[j]),
                                        GLIS = 0.) )
              else :
                motscle3a['AFFE'].append(_F(GROUP_NO=string.strip(__nom_noeu[j]),NOM_CMP='GLIS',
                                        VALE = __tension*(-1)**(j+1)) )
                motscle2b['DDL_IMPO'].append(_F(GROUP_NO=string.strip(__nom_noeu[j]),
                                        GLIS = 0.) )
              if __recul_exists :
                motscle5['DDL_IMPO'].append(_F(GROUP_NO=string.strip(__nom_noeu[j]),
                                          GLIS = -__recul*(-1)**(j+1)) )
        if (actif == 2) :
          __ActifActif = True
      DETRUIRE(CONCEPT=_F(NOM=__TCAB1))

    dExcit=[]
    for j in EXCIT :
        dExcit.append(j.cree_dict_valeurs(j.mc_liste))
        for i in dExcit[-1].keys():
            if dExcit[-1][i]==None : del dExcit[-1][i]

    assert(len(motscle3)>0)
    if __ActifActif :
      dExcit1a=copy.copy(dExcit)
      dExcit1b=copy.copy(dExcit)
      __CH1a=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',OPERATION='AFFE',MODELE=MODELE,
                    **motscle3a)
      __CH1b=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',OPERATION='AFFE',MODELE=MODELE,
                    **motscle3b)
      _C_CAc=AFFE_CHAR_MECA(MODELE=MODELE,VECT_ASSE=__CH1a,**motscle2)
      _C_CAd=AFFE_CHAR_MECA(MODELE=MODELE,VECT_ASSE=__CH1b,**motscle2)
      _C_CAa=AFFE_CHAR_MECA(MODELE=MODELE,**motscle2a)
      _C_CAb=AFFE_CHAR_MECA(MODELE=MODELE,**motscle2b)
      dExcit1a.append(_F(CHARGE=_C_CAa,
                         TYPE_CHARGE='DIDI'
                         ))
      dExcit1b.append(_F(CHARGE=_C_CAb,
                          TYPE_CHARGE='DIDI'))
      dExcit1a.append(_F(CHARGE=_C_CAc))
      dExcit1b.append(_F(CHARGE=_C_CAd))
      __L2[-1:-1] = [__TINT]
    else :
      dExcit1=copy.copy(dExcit)
      __CH1=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',OPERATION='AFFE',MODELE=MODELE,
                    **motscle3)
      _C_CA=AFFE_CHAR_MECA(MODELE=MODELE,VECT_ASSE=__CH1,**motscle2)
      dExcit1.append(_F(CHARGE=_C_CA,TYPE_CHARGE='DIDI'),)

    _C_RA=AFFE_CHAR_MECA(MODELE=MODELE,**motscle5)
    dExcit2=copy.copy(dExcit)
    dExcit2.append(_F(CHARGE=_C_RA,TYPE_CHARGE='DIDI'),)

    if __recul_exists :
      __L2[-1:-1] = [__TINT2]
      __LST=DEFI_LIST_REEL(VALE=__L2,);
      for i in dIncrement.keys():
          if dIncrement[i]==None : del dIncrement[i]
      dIncrement['LIST_INST']= __LST
    else :
      if __ActifActif :
        __LST=DEFI_LIST_REEL(VALE=__L2,);
        for i in dIncrement.keys():
            if dIncrement[i]==None : del dIncrement[i]
        dIncrement['LIST_INST']= __LST
      else :
        dIncrement=INCREMENT

    if __ActifActif :
      dIncrement['INST_FIN'] = __TINT
      RES=STAT_NON_LINE(
                      MODELE      =MODELE,
                      CARA_ELEM   =CARA_ELEM,
                      CHAM_MATER  = CHAM_MATER,
                      COMP_INCR=COMP_INCR,
                      #INCREMENT=INCREMENT,
                      INCREMENT=dIncrement,
                      NEWTON   = _F(REAC_ITER=1),
                      ETAT_INIT = ETAT_INIT,
                      METHODE=METHODE,
                      CONVERGENCE=CONVERGENCE,
                      RECH_LINEAIRE = RECH_LINEAIRE,
                      SOLVEUR = SOLVEUR,
                      INFO     =INFO,
                      TITRE = TITRE,
                      EXCIT = dExcit1a,
                      **motscle4)
      if __recul_exists :
        dIncrement['INST_FIN'] = __TINT2
      else :
        dIncrement['INST_FIN'] = __TMAX
      RES=STAT_NON_LINE(reuse=RES,
                        ETAT_INIT=_F(EVOL_NOLI=RES),
                      MODELE      =MODELE,
                      CARA_ELEM   =CARA_ELEM,
                      CHAM_MATER  = CHAM_MATER,
                      COMP_INCR=COMP_INCR,
                      #INCREMENT=INCREMENT,
                      INCREMENT=dIncrement,
                      NEWTON   = _F(REAC_ITER=1),
                      METHODE=METHODE,
                      CONVERGENCE=CONVERGENCE,
                      RECH_LINEAIRE = RECH_LINEAIRE,
                      SOLVEUR = SOLVEUR,
                      INFO     =INFO,
                      TITRE = TITRE,
                      EXCIT = dExcit1b,)

    else :
      if __recul_exists :
        dIncrement['INST_FIN'] = __TINT2

      RES=STAT_NON_LINE(
                      MODELE      =MODELE,
                      CARA_ELEM   =CARA_ELEM,
                      CHAM_MATER  = CHAM_MATER,
                      COMP_INCR=COMP_INCR,
                      #INCREMENT=INCREMENT,
                      INCREMENT=dIncrement,
                      NEWTON   = _F(REAC_ITER=1),
                      METHODE=METHODE,
                      ETAT_INIT = ETAT_INIT,
                      CONVERGENCE=CONVERGENCE,
                      RECH_LINEAIRE = RECH_LINEAIRE,
                      SOLVEUR = SOLVEUR,
                      INFO     =INFO,
                      TITRE = TITRE,
                      EXCIT = dExcit1,
                      **motscle4)
    if __recul_exists :
      RES=STAT_NON_LINE(reuse=RES,
                        ETAT_INIT=_F(EVOL_NOLI=RES),
                        MODELE      =MODELE,
                        CARA_ELEM   =CARA_ELEM,
                        CHAM_MATER  = CHAM_MATER,
                        COMP_INCR=COMP_INCR,
                        ##INCREMENT=INCREMENT,
                        INCREMENT=_F(LIST_INST = __LST,
                                      PRECISION = __prec),
                        NEWTON   = _F(REAC_ITER=1),
                        #ETAT_INIT = ETAT_INIT,
                        METHODE=METHODE,
                        CONVERGENCE=CONVERGENCE,
                        RECH_LINEAIRE = RECH_LINEAIRE,
                        SOLVEUR = SOLVEUR,
                        INFO     =INFO,
                        TITRE = TITRE,
                        EXCIT = dExcit2,
                        )

  else :
    raise Exception("erreur de programmation, adher different de OUI et NON")

  RetablirAlarme('COMPOR1_70')
  return ier
