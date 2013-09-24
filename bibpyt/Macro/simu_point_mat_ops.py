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

def simu_point_mat_ops(self, MATER, INCREMENT,SIGM_IMPOSE,EPSI_IMPOSE,SIGM_INIT,EPSI_INIT,VARI_INIT,NEWTON,CONVERGENCE,
           MASSIF,ANGLE,COMPORTEMENT,INFO,ARCHIVAGE,SUPPORT, **args) :

  """Simulation de la reponse d'un point materiel"""

  ier = 0
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)
  import math

  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  AFFE_CARA_ELEM  = self.get_cmd('AFFE_CARA_ELEM')
  AFFE_CHAR_MECA  = self.get_cmd('AFFE_CHAR_MECA')
  AFFE_MATERIAU   = self.get_cmd('AFFE_MATERIAU')
  AFFE_MODELE     = self.get_cmd('AFFE_MODELE')
  CALC_CHAMP      = self.get_cmd('CALC_CHAMP')
  CALC_POINT_MAT  = self.get_cmd('CALC_POINT_MAT')
  CALC_TABLE      = self.get_cmd('CALC_TABLE')
  CREA_CHAMP      = self.get_cmd('CREA_CHAMP')
  CREA_RESU       = self.get_cmd('CREA_RESU')
  DEFI_FONCTION   = self.get_cmd('DEFI_FONCTION')
  IMPR_TABLE      = self.get_cmd('IMPR_TABLE')
  LIRE_MAILLAGE   = self.get_cmd('LIRE_MAILLAGE')
  MODI_MAILLAGE   = self.get_cmd('MODI_MAILLAGE')
  MODI_REPERE     = self.get_cmd('MODI_REPERE')
  POST_RELEVE_T   = self.get_cmd('POST_RELEVE_T')
  STAT_NON_LINE   = self.get_cmd('STAT_NON_LINE')
  IMPR_RESU       = self.get_cmd('IMPR_RESU')

  from Accas import _F
  from Utilitai.UniteAster import UniteAster
  from Utilitai.Utmess import  UTMESS,MasquerAlarme, RetablirAlarme
  from Noyau.N_types import is_sequence

  # alarme de STAT_NON_LINE si les mot-cles de COMPORTEMENT sont renseignes a tort
  MasquerAlarme('COMPOR1_70')

# -- Tests de cohérence
  __fonczero = DEFI_FONCTION(NOM_PARA = 'INST',
  VALE     = ( 0,0, 10,0 ),PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT')
  EPS={}
  SIG={}
  itetra=0
  CMP_EPS=['EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ']
  CMP_SIG=['SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ']

  if COMPORTEMENT  :
     lcomp = COMPORTEMENT.List_F()[0]

  if SUPPORT != None :
     if SUPPORT=='ELEMENT':
        itetra=1
  if itetra==0 :
        if lcomp['DEFORMATION'] != 'PETIT' :
           if args.has_key('GRAD_IMPOSE'):
              if args['GRAD_IMPOSE'] != None:
                 if lcomp['DEFORMATION'] != 'SIMO_MIEHE' :
                    UTMESS('F','COMPOR2_22',valk=lcomp['DEFORMATION'] )
                 itetra=0
              else :
                 itetra=1
                 UTMESS('A','COMPOR2_1',valk=lcomp['DEFORMATION'] )

#===============================================================
# cas ou il n'y a pas d'élement fini : appel à CALC_POINT_MAT
#===============================================================
  if itetra == 0 :

       isig=0
       ieps=0
       igrd=0
       ic1c2=0
       if SIGM_IMPOSE:
          SIG=SIGM_IMPOSE[0].cree_dict_valeurs(SIGM_IMPOSE[0].mc_liste)
          isig=1
       if EPSI_IMPOSE:
          EPS=EPSI_IMPOSE[0].cree_dict_valeurs(EPSI_IMPOSE[0].mc_liste)
          ieps=1
       if args.has_key('GRAD_IMPOSE'):
          if args['GRAD_IMPOSE'] != None:
             FIJ=args['GRAD_IMPOSE'][0].cree_dict_valeurs(args['GRAD_IMPOSE'][0].mc_liste)
             igrd=1
       if args.has_key('MATR_C1') :
          if args['MATR_C1'] != None:
             ic1c2=1
       if args.has_key('MATR_C2') :
          if args['MATR_C2'] != None:
             ic1c2=1

       motscles={}
       if igrd :
          for i in FIJ.keys():
            motscles[i]=FIJ[i]
       elif ic1c2 :
          if args.has_key('MATR_C1') :
             if args['MATR_C1'] != None:
                motscles['MATR_C1']  = args['MATR_C1'].List_F()
          if args.has_key('MATR_C2'):
             if args['MATR_C2'] != None:
                motscles['MATR_C2']  = args['MATR_C2'].List_F()
          if args.has_key('VECT_IMPO'):
             if args['VECT_IMPO'] != None:
                motscles['VECT_IMPO']  = args['VECT_IMPO'].List_F()
       else :
          nbsig=6
          for index in range(nbsig):
              iks=CMP_SIG[index]
              ike=CMP_EPS[index]
              inds=0
              inde=0
              if ieps :
                if EPS[ike]!=None :
                   inde=1
              if isig :
                if SIG[iks]!=None :
                   inds=1
              if inde*inds!=0 :
                 UTMESS('F','COMPOR2_2',valk=iks)
              if inde==1 :
                 motscles[ike] = EPS[ike]
              elif inds==1:
                 motscles[iks]= SIG[iks]
              else:
                 motscles[iks]=__fonczero
#      Etat initial
       etatinit=0
       if SIGM_INIT != None :
          motscles['SIGM_INIT']   = SIGM_INIT.List_F()
       if EPSI_INIT != None :
          motscles['EPSI_INIT']   = EPSI_INIT.List_F()
       if VARI_INIT != None :
          motscles['VARI_INIT']   = VARI_INIT.List_F()
       if NEWTON != None :
          motscles['NEWTON']      = NEWTON.List_F()
       if CONVERGENCE != None :
          motscles['CONVERGENCE'] = CONVERGENCE.List_F()
       if MASSIF != None :
          motscles['MASSIF']      = MASSIF.List_F()
       if COMPORTEMENT  :
          motscles['COMPORTEMENT']   = COMPORTEMENT.List_F()
#      -- Deroulement du calcul
       motscles['INCREMENT']      = INCREMENT.List_F()

       if args.has_key('FORMAT_TABLE'):
          if args['FORMAT_TABLE'] != None:
             motscles['FORMAT_TABLE']  = args['FORMAT_TABLE']

       if args.has_key('OPER_TANGENT'):
          if args['OPER_TANGENT'] != None:
             motscles['OPER_TANGENT']  = args['OPER_TANGENT']

       if args.has_key('NB_VARI_TABLE'):
          if args['NB_VARI_TABLE'] != None:
             motscles['NB_VARI_TABLE']  = args['NB_VARI_TABLE']

       if   ARCHIVAGE   :
         motscles['ARCHIVAGE']   = ARCHIVAGE.List_F()

         #     variables de commande
       mcvarc=[]
       if args.has_key('AFFE_VARC'):
          if args['AFFE_VARC'] != None:
             motscles['AFFE_VARC']  = args['AFFE_VARC'].List_F()

       self.DeclareOut('REPONSE',self.sd)

       Titre='CALC_POINT_MAT'
       if ARCHIVAGE != None :
#         on ne prend en compte que ARCHIVAGE / LIST_INST
          if ARCHIVAGE['LIST_INST'] != None :
             __REP1 = CALC_POINT_MAT(INFO=INFO,MATER=MATER,ANGLE=ANGLE,**motscles)
             lr8=ARCHIVAGE['LIST_INST']
             lr=lr8.Valeurs()
             REPONSE=CALC_TABLE( TABLE=__REP1,TITRE=Titre,
                           ACTION=_F(OPERATION='FILTRE',NOM_PARA='INST',
                                     VALE=lr,PRECISION=ARCHIVAGE['PRECISION']),
                          )
          else :
             REPONSE = CALC_POINT_MAT(INFO=INFO,MATER=MATER,ANGLE=ANGLE,**motscles)
       else :
          REPONSE = CALC_POINT_MAT(INFO=INFO,MATER=MATER,ANGLE=ANGLE,**motscles)


#===============================================================
# cas ou on fait le calcul sur un TETRA4 A UN SEUL POINT DE GAUSS
#===============================================================
  elif itetra==1 :

      EPS={}
      SIG={}
      MODELISATION="3D"
      if args.has_key('MODELISATION'):
         if args['MODELISATION'] != None:
            MODELISATION=args['MODELISATION']

      if MODELISATION=="3D":
          nbsig=6
          CMP_EPS=['EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ']
          CMP_SIG=['SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ']
      else:
          nbsig=3
          CMP_EPS=['EPXX','EPYY','EPXY']
          CMP_SIG=['SIXX','SIYY','SIXY']


      if SIGM_IMPOSE:
         SIG=SIGM_IMPOSE[0].cree_dict_valeurs(SIGM_IMPOSE[0].mc_liste)
         for i in SIG.keys():
             if SIG[i]==None : SIG[i]=__fonczero
      else:
         for i in range(nbsig):
             SIG[CMP_SIG[i]]=__fonczero

      if EPSI_IMPOSE:
         EPS=EPSI_IMPOSE[0].cree_dict_valeurs(EPSI_IMPOSE[0].mc_liste)
      else:
         for i in range(nbsig):
             EPS[CMP_EPS[i]]=None

      for index in range(nbsig):
          iks=CMP_SIG[index]
          ike=CMP_EPS[index]
          if EPS[ike]!=None and SIG[iks] != __fonczero :
             UTMESS('F','COMPOR2_3',valk= str(iks) +' '+ str(ike))

#     -- Definition du maillage
      if MODELISATION=="3D":

         texte_ma = """
           COOR_3D
             P0  0.0   0.0   0.0
             P1  1.0   0.0   0.0
             P2  0.0   1.0   0.0
             P3  0.0   0.0   1.0
           FINSF
           TRIA3
             F1   P0 P3 P2
             F2   P0 P1 P3
             F3   P0 P2 P1
             F4   P1 P2 P3
           FINSF
           TETRA4
             VOLUME = P0 P1 P2 P3
           FINSF
           GROUP_MA
           TOUT  VOLUME
           FINSF
           GROUP_NO
           TOUT  P1 P2 P0 P3
           FINSF
           FIN
         """

      else :

         texte_ma = """
           COOR_2D
             P0  0.0   0.0
             P1  1.0   0.0
             P2  0.0   1.0
           FINSF
           SEG2
             S1   P2 P0
             S2   P0 P1
             S3   P1 P2
           FINSF
           TRIA3
             VOLUME = P0 P1 P2
           FINSF
           GROUP_MA
           TOUT  VOLUME
           FINSF
           GROUP_NO
           TOUT  P1 P2 P0
           FINSF
           FIN
         """

      fi_mail = open('simu.mail','w')
      fi_mail.write(texte_ma)
      fi_mail.close()

      UL = UniteAster()
      umail = UL.Libre(action='ASSOCIER', nom='simu.mail' )

      __MA  =  LIRE_MAILLAGE(UNITE=umail)
      UL.EtatInit()



      if MODELISATION=="3D":
         __MO = AFFE_MODELE(  MAILLAGE = __MA,
           AFFE=_F(MAILLE=('VOLUME','F1','F2','F3','F4'),PHENOMENE='MECANIQUE',MODELISATION='3D',))
##     ANGLE : rotation de ANGLE autour de Z uniquement, et seulement pour les déformations
##             imposées.
         if ANGLE != 0. :
            __MA=MODI_MAILLAGE(reuse=__MA ,MAILLAGE=__MA,ROTATION=_F(POIN_1=(0.,0.),ANGLE=ANGLE),)
            c=math.cos(ANGLE*math.pi/180.)
            s=math.sin(ANGLE*math.pi/180.)
            __C_RIGIDE=AFFE_CHAR_MECA(MODELE=__MO,
                  DDL_IMPO=_F(NOEUD='P0',DX=0,DY=0.,DZ=0.),
                  LIAISON_DDL = (
            _F(NOEUD=('P1','P1','P2','P2'),DDL=('DX','DY','DX','DY'),COEF_MULT=(s,-c,c,s),COEF_IMPO=0),
            _F(NOEUD=('P1','P3','P3'),DDL=('DZ','DX','DY'),COEF_MULT=(-1.0,c,s),COEF_IMPO=0),
            _F(NOEUD=('P2','P3','P3'),DDL=('DZ','DX','DY'),COEF_MULT=(-1.0,-s,c),COEF_IMPO=0),),)
         else :
#     -- Mouvement de corps rigide
            __C_RIGIDE = AFFE_CHAR_MECA(MODELE=__MO,
                  DDL_IMPO = _F(NOEUD = 'P0',DX = 0,DY = 0,DZ = 0),
                  LIAISON_DDL = (
            _F(NOEUD=('P2','P1'),DDL=('DX','DY'),COEF_MULT=(1,-1),COEF_IMPO=0),
            _F(NOEUD=('P3','P1'),DDL=('DX','DZ'),COEF_MULT=(1,-1),COEF_IMPO=0),
            _F(NOEUD=('P3','P2'),DDL=('DY','DZ'),COEF_MULT=(1,-1),COEF_IMPO=0),))
      else:
      # MODELISATION 2D
         __MO=AFFE_MODELE(MAILLAGE=__MA,
              AFFE=_F(MAILLE=('VOLUME','S1','S2','S3'),PHENOMENE='MECANIQUE',MODELISATION=MODELISATION))
##     ANGLE : rotation de ANGLE autour de Z uniquement, et seulement pour les déformations
##             imposées.
         if ANGLE != 0. :
            __MA=MODI_MAILLAGE(reuse=__MA ,MAILLAGE=__MA,ROTATION=_F(POIN_1=(0.,0.),ANGLE=ANGLE),)
            c=math.cos(ANGLE*math.pi/180.)
            s=math.sin(ANGLE*math.pi/180.)
            __C_RIGIDE = AFFE_CHAR_MECA(MODELE = __MO,
                  DDL_IMPO=_F(NOEUD='P0',DX=0,DY=0.),
                  LIAISON_DDL=(_F(NOEUD=('P1','P1','P2','P2'),DDL=('DX','DY','DX','DY'),
                       COEF_MULT=(s,-c,c,s),COEF_IMPO=0),),)
         else :
            __C_RIGIDE = AFFE_CHAR_MECA(MODELE = __MO,
                DDL_IMPO = _F(NOEUD = 'P0',DX = 0,DY = 0),
                LIAISON_DDL = (_F(NOEUD=('P2','P1'),DDL=('DX','DY'),COEF_MULT=(1,-1),COEF_IMPO=0),))

#     --MASSIF : orientation du materiau (monocristal, orthotropie)
      if MASSIF:
          ANGMAS=MASSIF[0].cree_dict_valeurs(MASSIF[0].mc_liste)
          if ANGMAS["ANGL_REP"]==None :
             __CARA=AFFE_CARA_ELEM(MODELE=__MO,MASSIF=_F(MAILLE='VOLUME',ANGL_EULER=ANGMAS["ANGL_EULER"]),);
          else :
             __CARA=AFFE_CARA_ELEM(MODELE=__MO,MASSIF=_F(MAILLE='VOLUME',ANGL_REP=ANGMAS["ANGL_REP"]),);

#     -- Chargement en deformation

      __E = [None]*nbsig

      __E[0] = AFFE_CHAR_MECA(MODELE = __MO,
        LIAISON_OBLIQUE = _F(NOEUD='P1', DX=1, ANGL_NAUT=ANGLE))

      __E[1] = AFFE_CHAR_MECA(MODELE = __MO,
        LIAISON_OBLIQUE = _F(NOEUD='P2', DY=1, ANGL_NAUT=ANGLE))

      if MODELISATION=="3D":

          __E[2] = AFFE_CHAR_MECA(MODELE = __MO,
             LIAISON_OBLIQUE = _F(NOEUD='P3', DZ=1, ANGL_NAUT=ANGLE))

          __E[3] = AFFE_CHAR_MECA(MODELE = __MO,
            LIAISON_OBLIQUE = _F(NOEUD='P1', DY=1, ANGL_NAUT=ANGLE))

          __E[4] = AFFE_CHAR_MECA(MODELE = __MO,
            LIAISON_OBLIQUE = _F(NOEUD='P1', DZ=1, ANGL_NAUT=ANGLE))

          __E[5] = AFFE_CHAR_MECA(MODELE = __MO,
            LIAISON_OBLIQUE = _F(NOEUD='P2', DZ=1, ANGL_NAUT=ANGLE))

      else:
          c=math.cos(ANGLE*math.pi/180.)
          s=math.sin(ANGLE*math.pi/180.)
          __E[2] = AFFE_CHAR_MECA(MODELE = __MO,
            LIAISON_OBLIQUE = _F(NOEUD='P1', DY=1, ANGL_NAUT=ANGLE),)

#     -- Chargement en contrainte

      __S = [None]*nbsig

      if MODELISATION=="3D":

          r33 = 3**-0.5
          __S[0] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_FACE = (
              _F(MAILLE='F1', FX=-1),
              _F(MAILLE='F4', FX= r33),))

          __S[1] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_FACE = (
              _F(MAILLE='F2', FY=-1),
              _F(MAILLE='F4', FY= r33),))

          __S[2] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_FACE = (
              _F(MAILLE='F3', FZ=-1),
              _F(MAILLE='F4', FZ= r33),))

          __S[3] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_FACE = (
              _F(MAILLE='F1', FY=-1),
              _F(MAILLE='F2', FX=-1),
              _F(MAILLE='F4', FX= r33, FY=r33),))

          __S[4] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_FACE = (
              _F(MAILLE='F1', FZ=-1),
              _F(MAILLE='F3', FX=-1),
              _F(MAILLE='F4', FX= r33, FZ=r33),))

          __S[5] = AFFE_CHAR_MECA( MODELE = __MO, FORCE_FACE = (
              _F(MAILLE='F2', FZ=-1),
              _F(MAILLE='F3', FY=-1),
              _F(MAILLE='F4', FY= r33, FZ=r33), ) )

      else:
          r22 = 2**-0.5
          __S[0] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_CONTOUR = (
              _F(MAILLE='S1', FX=-1),
              _F(MAILLE='S3', FX= r22), ))

          __S[1] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_CONTOUR = (
              _F(MAILLE='S2', FY=-1),
              _F(MAILLE='S3', FY= r22), ) )

          __S[2] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_CONTOUR = (
              _F(MAILLE='S1', FY=-1),
              _F(MAILLE='S2', FX=-1),
              _F(MAILLE='S3', FX= r22, FY=r22), ) )

#     -- Construction de la charge

      l_char = [  _F(CHARGE=__C_RIGIDE)  ]

      for i in xrange(nbsig) :
        ike=CMP_EPS[i]
        if EPS[ike]:
           l_char.append(  _F(CHARGE=__E[i],FONC_MULT=EPS[ike])  )

      for i in xrange(nbsig) :
        iks=CMP_SIG[i]
        l_char.append(  _F(CHARGE=__S[i],FONC_MULT=SIG[iks])  )

#     variables de commande
      mcvarc=[]
      if args.has_key('AFFE_VARC'):
         if args['AFFE_VARC'] != None:
             lvarc  = args['AFFE_VARC'].List_F()
             nbvarc=len(lvarc)
             for ivarc in range(nbvarc) :
                  dico={}
                  if(str(lvarc[ivarc]['NOM_VARC'])=='SECH'):
                      typech = 'NOEU_TEMP_R'
                      labsc=lvarc[ivarc]['VALE_FONC'].Absc()
                      lordo=lvarc[ivarc]['VALE_FONC'].Ordo()
                      l_affe_cham=[]
                      __CHV=[None]*len(labsc)
                      for it,time in enumerate(labsc):
                          __CHV[it]=CREA_CHAMP(TYPE_CHAM=typech,
                                     OPERATION='AFFE',
                                     MAILLAGE=__MA,
                                     AFFE=_F(MAILLE='VOLUME',
                                             NOM_CMP='TEMP',
                                             VALE=lordo[it],
                                            ),
                                     ),
                          dicoch={}
                          dicoch["CHAM_GD"]=__CHV[it]
                          dicoch["INST"]=time
                          l_affe_cham.append(dicoch)
                      __EVOV=CREA_RESU(OPERATION='AFFE',TYPE_RESU='EVOL_VARC',NOM_CHAM='TEMP',
                               AFFE = l_affe_cham)
                  elif(str(lvarc[ivarc]['NOM_VARC'])=='M_ZIRC'):
                      typech = 'ELNO_NEUT_R'
                      labsc =lvarc[ivarc]['V1'].Absc()
                      lordo1=lvarc[ivarc]['V1'].Ordo()
                      lordo2=lvarc[ivarc]['V2'].Ordo()
                      lordo3=lvarc[ivarc]['V3'].Ordo()
                      lordo4=lvarc[ivarc]['V4'].Ordo()
                      l_affe_cham=[]
                      __CHV=[None]*len(labsc)
                      __CHN=[None]*len(labsc)
                      for it,time in enumerate(labsc):
                          __CHN[it]=CREA_CHAMP(TYPE_CHAM=typech,
                                     OPERATION='AFFE',PROL_ZERO='OUI',
                                     MODELE=__MO,
                                     AFFE=(
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X1',
                                             VALE=lordo1[it],
                                            ),
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X2',
                                             VALE=lordo2[it],
                                            ),
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X3',
                                             VALE=lordo3[it],
                                            ),
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X4',
                                             VALE=lordo4[it],
                                            ),
                                     ),),
                          dicoch={}
                          __CHV[it] =CREA_CHAMP( OPERATION='ASSE',TYPE_CHAM='ELNO_VARI_R',
                                   MODELE=__MO,PROL_ZERO='OUI',
                                   ASSE=(
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X1',NOM_CMP_RESU='V1',GROUP_MA='TOUT'),
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X2',NOM_CMP_RESU='V2',GROUP_MA='TOUT'),
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X3',NOM_CMP_RESU='V3',GROUP_MA='TOUT'),
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X4',NOM_CMP_RESU='V4',GROUP_MA='TOUT'),
                                   )) ;
                          dicoch["CHAM_GD"]=__CHV[it]
                          dicoch["INST"]=time
                          l_affe_cham.append(dicoch)
                      __EVOV=CREA_RESU(OPERATION='AFFE',TYPE_RESU='EVOL_THER',
                                       NOM_CHAM=str('META_ELNO'),AFFE = l_affe_cham)
                      IMPR_RESU(RESU=_F(RESULTAT=__EVOV))
                  elif(str(lvarc[ivarc]['NOM_VARC'])=='M_ACIER'):
                      typech = 'ELNO_NEUT_R'
                      labsc =lvarc[ivarc]['V1'].Absc()
                      lordo1=lvarc[ivarc]['V1'].Ordo()
                      lordo2=lvarc[ivarc]['V2'].Ordo()
                      lordo3=lvarc[ivarc]['V3'].Ordo()
                      lordo4=lvarc[ivarc]['V4'].Ordo()
                      lordo5=lvarc[ivarc]['V5'].Ordo()
                      lordo6=lvarc[ivarc]['V6'].Ordo()
                      lordo7=lvarc[ivarc]['V7'].Ordo()
                      l_affe_cham=[]
                      __CHV=[None]*len(labsc)
                      __CHN=[None]*len(labsc)
                      for it,time in enumerate(labsc):
                          __CHN[it]=CREA_CHAMP(TYPE_CHAM=typech,
                                     OPERATION='AFFE',PROL_ZERO='OUI',
                                     MODELE=__MO,
                                     AFFE=(
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X1',
                                             VALE=lordo1[it],
                                            ),
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X2',
                                             VALE=lordo2[it],
                                            ),
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X3',
                                             VALE=lordo3[it],
                                            ),
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X4',
                                             VALE=lordo4[it],
                                            ),
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X5',
                                             VALE=lordo5[it],
                                            ),
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X6',
                                             VALE=lordo6[it],
                                            ),
                                          _F(MAILLE='VOLUME',
                                             NOM_CMP='X7',
                                             VALE=lordo7[it],
                                            ),
                                     ),),
                          dicoch={}
                          __CHV[it] =CREA_CHAMP( OPERATION='ASSE',TYPE_CHAM='ELNO_VARI_R',
                                   MODELE=__MO,PROL_ZERO='OUI',
                                   ASSE=(
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X1',NOM_CMP_RESU='V1',GROUP_MA='TOUT'),
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X2',NOM_CMP_RESU='V2',GROUP_MA='TOUT'),
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X3',NOM_CMP_RESU='V3',GROUP_MA='TOUT'),
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X4',NOM_CMP_RESU='V4',GROUP_MA='TOUT'),
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X5',NOM_CMP_RESU='V5',GROUP_MA='TOUT'),
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X6',NOM_CMP_RESU='V6',GROUP_MA='TOUT'),
                                   _F(CHAM_GD=__CHN[it],NOM_CMP='X7',NOM_CMP_RESU='V7',GROUP_MA='TOUT'),
                                   )) ;
                          dicoch["CHAM_GD"]=__CHV[it]
                          dicoch["INST"]=time
                          l_affe_cham.append(dicoch)
                      __EVOV=CREA_RESU(OPERATION='AFFE',TYPE_RESU='EVOL_THER',
                                       NOM_CHAM=str('META_ELNO'),AFFE = l_affe_cham)
                      IMPR_RESU(RESU=_F(RESULTAT=__EVOV))
                  else:
                      typech = 'NOEU_' + str(lvarc[ivarc]['NOM_VARC']) + '_R'
                      labsc=lvarc[ivarc]['VALE_FONC'].Absc()
                      lordo=lvarc[ivarc]['VALE_FONC'].Ordo()
                      l_affe_cham=[]
                      __CHV=[None]*len(labsc)
                      for it,time in enumerate(labsc):
                          __CHV[it]=CREA_CHAMP(TYPE_CHAM=typech,
                                     OPERATION='AFFE',
                                     MAILLAGE=__MA,
                                     AFFE=_F(MAILLE='VOLUME',
                                             NOM_CMP=lvarc[ivarc]['NOM_VARC'],
                                             VALE=lordo[it],
                                            ),
                                     ),
                          dicoch={}
                          dicoch["CHAM_GD"]=__CHV[it]
                          dicoch["INST"]=time
                          l_affe_cham.append(dicoch)
                      __EVOV=CREA_RESU(OPERATION='AFFE',TYPE_RESU='EVOL_VARC',NOM_CHAM=str(lvarc[ivarc]['NOM_VARC']),
                               AFFE = l_affe_cham)
                  dico["MAILLE"]='VOLUME'
                  dico["EVOL"]=__EVOV
                  if (str(lvarc[ivarc]['NOM_VARC'])=='M_ZIRC'):
                     dico["NOM_VARC"]="M_ZIRC"
                  elif (str(lvarc[ivarc]['NOM_VARC'])=='M_ACIER'):
                     dico["NOM_VARC"]="M_ACIER"
                  else:
                     dico["NOM_VARC"]=lvarc[ivarc]['NOM_VARC']
                     if lvarc[ivarc]['VALE_REF'] != None:
                        dico["VALE_REF"]=lvarc[ivarc]['VALE_REF']
                  mcvarc.append(dico)
#      -- Materiau et modele
      if len(mcvarc) > 0 :
         __CHMAT=AFFE_MATERIAU(MAILLAGE=__MA,AFFE = _F(MAILLE='VOLUME',MATER=MATER),
                               AFFE_VARC=mcvarc,
                              )
      else :
         __CHMAT=AFFE_MATERIAU(MAILLAGE=__MA,AFFE = _F(MAILLE='VOLUME',MATER=MATER))

#     Etat initial
      SIGINI={}
      VARINI={}
      LCSIG=[]
      LVSIG=[]
      init_dico={}
      etatinit=0

#     --contraintes initiales
      if SIGM_INIT:
          etatinit=1
          SIGINI=SIGM_INIT[0].cree_dict_valeurs(SIGM_INIT[0].mc_liste)
          for i in SIGINI.keys():
              if SIGINI[i]!=None :
                  LCSIG.append(i)
                  LVSIG.append(SIGINI[i])

          __SIG_INIT=CREA_CHAMP(MAILLAGE=__MA,OPERATION='AFFE',TYPE_CHAM='CART_SIEF_R',
                    AFFE=_F(TOUT='OUI', NOM_CMP=LCSIG, VALE=LVSIG,))
          init_dico['SIGM']=__SIG_INIT

#     --variables internes initiales
      if VARI_INIT:
          etatinit=1
          lnomneu=[]
          lnomvar=[]
          VARINI=VARI_INIT[0].cree_dict_valeurs(VARI_INIT[0].mc_liste)
          if (not is_sequence(VARINI['VALE'])) :
              VARINI['VALE'] = [VARINI['VALE'],]
          nbvari=len(VARINI['VALE'])
          for i in range(nbvari):
              lnomneu.append('X'+str(i+1))
              lnomvar.append('V'+str(i+1))

          __NEUT=CREA_CHAMP(OPERATION='AFFE', TYPE_CHAM='CART_NEUT_R', MAILLAGE=__MA,
            AFFE=_F( MAILLE ='VOLUME', NOM_CMP = lnomneu, VALE = VARINI['VALE']))

          __VAR_INIT=CREA_CHAMP(MODELE=__MO,OPERATION='ASSE',TYPE_CHAM='ELGA_VARI_R',
                       ASSE=_F(TOUT='OUI',CHAM_GD=__NEUT,NOM_CMP=lnomneu,NOM_CMP_RESU=lnomvar))
          init_dico['VARI']=__VAR_INIT

      # --deformations initiales
      if EPSI_INIT:
          etatinit=1
          EPSINI={}
          LCDEPL=[]
          LNDEPL=[]
          LVDEPL=[]
          LIST_AFFE=[]
          mon_dico={}
          mon_dico["NOEUD"]='P0'
          mon_dico["NOM_CMP"]=("DX","DY","DZ")
          mon_dico["VALE"]=(0.,0.,0.)
          LIST_AFFE.append(mon_dico)

          EPSINI=EPSI_INIT[0].cree_dict_valeurs(EPSI_INIT[0].mc_liste)
          mon_dico={}
          mon_dico["NOEUD"]='P1'
          mon_dico["NOM_CMP"]='DX'
          mon_dico["VALE"]=EPSINI['EPXX']
          LIST_AFFE.append(mon_dico)
          mon_dico={}
          mon_dico["NOEUD"]='P2'
          mon_dico["NOM_CMP"]='DY'
          mon_dico["VALE"]=EPSINI['EPYY']
          LIST_AFFE.append(mon_dico)
          if MODELISATION=="3D":
              mon_dico={}
              mon_dico["NOEUD"]='P3'
              mon_dico["NOM_CMP"]='DZ'
              mon_dico["VALE"]=EPSINI['EPZZ']
              LIST_AFFE.append(mon_dico)
              mon_dico={}
              mon_dico["NOEUD"]='P1'
              mon_dico["NOM_CMP"]='DY'
              mon_dico["VALE"]=EPSINI['EPXY']
              LIST_AFFE.append(mon_dico)
              mon_dico={}
              mon_dico["NOEUD"]='P2'
              mon_dico["NOM_CMP"]='DX'
              mon_dico["VALE"]=EPSINI['EPXY']
              LIST_AFFE.append(mon_dico)
              mon_dico={}
              mon_dico["NOEUD"]='P1'
              mon_dico["NOM_CMP"]='DZ'
              mon_dico["VALE"]=EPSINI['EPXZ']
              LIST_AFFE.append(mon_dico)
              mon_dico={}
              mon_dico["NOEUD"]='P3'
              mon_dico["NOM_CMP"]='DX'
              mon_dico["VALE"]=EPSINI['EPXZ']
              LIST_AFFE.append(mon_dico)
              mon_dico={}
              mon_dico["NOEUD"]='P2'
              mon_dico["NOM_CMP"]='DZ'
              mon_dico["VALE"]=EPSINI['EPYZ']
              LIST_AFFE.append(mon_dico)
              mon_dico={}
              mon_dico["NOEUD"]='P3'
              mon_dico["NOM_CMP"]='DY'
              mon_dico["VALE"]=EPSINI['EPYZ']
              LIST_AFFE.append(mon_dico)
          else:
              mon_dico={}
              mon_dico["NOEUD"]='P1',
              mon_dico["NOM_CMP"]='DY'
              mon_dico["VALE"]=EPSINI['EPXY']
              LIST_AFFE.append(mon_dico)
              mon_dico={}
              mon_dico["NOEUD"]='P2'
              mon_dico["NOM_CMP"]='DX'
              mon_dico["VALE"]=EPSINI['EPXY']
              LIST_AFFE.append(mon_dico)
          __DEP_INI=CREA_CHAMP(MAILLAGE=__MA,OPERATION='AFFE',TYPE_CHAM='NOEU_DEPL_R',AFFE=LIST_AFFE)
          init_dico['DEPL']=__DEP_INI

#     -- Deroulement du calcul
      motscles={}
      if   COMPORTEMENT  :
          motscles['COMPORTEMENT']   = COMPORTEMENT.List_F()

      motscles['CONVERGENCE'] = CONVERGENCE.List_F()

      motscles['NEWTON']      = NEWTON.List_F()

      if args.has_key('RECH_LINEAIRE'):
         if args['RECH_LINEAIRE'] != None:
             motscles['RECH_LINEAIRE']  = args['RECH_LINEAIRE'].List_F()

      motscles['INCREMENT']   = INCREMENT.List_F()

      if   ARCHIVAGE   :
         motscles['ARCHIVAGE']   = ARCHIVAGE.List_F()

      if args.has_key('SUIVI_DDL'):
         if args['SUIVI_DDL'] != None:
            motscles['SUIVI_DDL']   = args['SUIVI_DDL'].List_F()

      if   etatinit == 1  :

         if MASSIF:
          __EVOL1 = STAT_NON_LINE(INFO = INFO,CARA_ELEM=__CARA,MODELE = __MO,CHAM_MATER = __CHMAT,
            ETAT_INIT=init_dico, EXCIT = l_char,**motscles)
         else:
          __EVOL1 = STAT_NON_LINE(INFO = INFO,MODELE = __MO,CHAM_MATER = __CHMAT,
            ETAT_INIT=init_dico, EXCIT = l_char,**motscles)

      else:

         if MASSIF:
             __EVOL1 = STAT_NON_LINE(INFO = INFO,MODELE = __MO, CARA_ELEM=__CARA,CHAM_MATER = __CHMAT,
                                 EXCIT = l_char,**motscles)
         else:
             __EVOL1 = STAT_NON_LINE(INFO = INFO,MODELE = __MO, CHAM_MATER = __CHMAT,
                                 EXCIT = l_char,**motscles)


      if lcomp['DEFORMATION'] != 'PETIT' :
         nomepsi='EPSG_ELNO'
      else :
         nomepsi='EPSI_ELNO'

      __EVOL1 = CALC_CHAMP(reuse = __EVOL1,RESULTAT = __EVOL1,
        CONTRAINTE   = 'SIGM_ELNO',
        DEFORMATION  = nomepsi,
        VARI_INTERNE = 'VARI_ELNO',);

      if MODELISATION=="3D":
          angles=(ANGLE,0,0)
          __EVOL=MODI_REPERE(RESULTAT=__EVOL1, MODI_CHAM=(
              _F(NOM_CHAM='DEPL',NOM_CMP=('DX','DY','DZ'),TYPE_CHAM='VECT_3D',),
              _F(NOM_CHAM='SIGM_ELNO',NOM_CMP=('SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ'),TYPE_CHAM='TENS_3D',),
              _F(NOM_CHAM=nomepsi,NOM_CMP=('EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ'),TYPE_CHAM='TENS_3D',),
                                  ),
                            REPERE='UTILISATEUR',
                            AFFE=_F(ANGL_NAUT=angles),);
      else :
          angles=ANGLE
          __EVOL=MODI_REPERE(RESULTAT=__EVOL1,MODI_CHAM=(
                     _F(NOM_CHAM='DEPL',NOM_CMP=('DX','DY'),TYPE_CHAM='VECT_2D',),
                     _F(NOM_CHAM='SIGM_ELNO',NOM_CMP=('SIXX','SIYY','SIZZ','SIXY'),TYPE_CHAM='TENS_2D',),
                     _F(NOM_CHAM=nomepsi,NOM_CMP=('EPXX','EPYY','EPZZ','EPXY'),TYPE_CHAM='TENS_2D',),
                                  ),
                            REPERE='UTILISATEUR',
                            AFFE=_F(ANGL_NAUT=angles),);

#     -- Recuperation des courbes

      __REP_VARI = POST_RELEVE_T(ACTION = (
          _F(INTITULE='VARI_INT',RESULTAT=__EVOL1,NOM_CHAM='VARI_ELNO',
            TOUT_CMP='OUI',OPERATION='EXTRACTION',NOEUD='P0'),))


      __REP_EPSI = POST_RELEVE_T(ACTION = (
          _F(INTITULE='EPSILON',RESULTAT=__EVOL,NOM_CHAM=nomepsi,
            TOUT_CMP='OUI',OPERATION='EXTRACTION',NOEUD     = 'P0'),))

      __REP_SIGM = POST_RELEVE_T(ACTION = (
          _F(INTITULE  = 'SIGMA',RESULTAT  =  __EVOL,NOM_CHAM  = 'SIGM_ELNO',
            TOUT_CMP  = 'OUI',OPERATION = 'EXTRACTION',NOEUD     = 'P0'),))

      __REP_INV = POST_RELEVE_T(ACTION = (
          _F(INTITULE  = 'INV',RESULTAT  =  __EVOL,NOM_CHAM  = 'SIGM_ELNO',
            INVARIANT  = 'OUI',OPERATION = 'EXTRACTION',NOEUD     = 'P0'),))

      __REP_INV=CALC_TABLE( TABLE=__REP_INV,reuse=__REP_INV,
               ACTION=_F(OPERATION='EXTR',NOM_PARA=('INST','TRACE','VMIS'), ) )

      self.DeclareOut('REPONSE',self.sd)

      REPONSE=CALC_TABLE( TABLE=__REP_EPSI,TITRE='TABLE ',ACTION=(
                       _F(OPERATION='COMB',TABLE=__REP_SIGM,NOM_PARA=('INST'), ),
                       _F(OPERATION='COMB',TABLE=__REP_INV ,NOM_PARA=('INST'), ),
                       _F(OPERATION='COMB',TABLE=__REP_VARI,NOM_PARA=('INST'), ),))

  RetablirAlarme('COMPOR1_70')
  return ier
