      SUBROUTINE ACEAPC ( NOMU, NOMA, LMAX, NBOCC )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      CHARACTER*8         NOMU, NOMA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/02/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE  CRP_6
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT DEFI_ARC
C ----------------------------------------------------------------------
C IN  : NOMU   : NOM UTILISATEUR DE LA COMMANDE
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : LMAX   : LONGUEUR
C IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE DEFI_ARC
C ----------------------------------------------------------------------
      INTEGER VALI
      REAL*8 XCEN(3),XTAN(3),X1(3),X2(3),XC1(3),XC2(3)
      REAL*8 ANGL(3),XM(3),V1(3),TM(3)
      CHARACTER*8  CRIT, ZK8BID
      CHARACTER*16  MCLEPT(3), MCLEPC(3)
      CHARACTER*19 CARTAR
      CHARACTER*24 TMPNAR, TMPVAR, MLGGMA, MLGNMA, MLGCNX, MLGCOO
      CHARACTER*24 NOMAIL, VALK(2)
      INTEGER      IARG
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IER ,IFM ,IGM ,IJM ,IMG ,IOC
      INTEGER ISPV ,IUNIFI ,JDCC ,JDCO ,JDGM ,JDLS ,JDNO
      INTEGER JDVC ,LMAX ,N1 ,N2 ,NA ,NBOCC ,NC
      INTEGER NDIM ,NF ,NFY ,NFZ ,NG ,NM ,NMG
      INTEGER NN1 ,NN2 ,NO1 ,NO2 ,NP ,NR ,NS
      INTEGER NSY ,NSZ ,NUMMAI
      REAL*8 DGRD ,DM ,EPSI ,PADIST ,PHI ,PHIS2 ,PI
      REAL*8 R8DGRD ,R8PI ,R8RDDG ,RDDG ,RR ,TOLE ,TX1
      REAL*8 TX2 ,XANG ,XFL ,XFLY ,XFLZ ,XRC ,XRC1
      REAL*8 XRC2 ,XSI ,XSIY ,XSIZ ,ZERO
C-----------------------------------------------------------------------
      CALL JEMARQ()
      IFM  = IUNIFI('MESSAGE')
      DGRD = R8DGRD()
      RDDG = R8RDDG()
      PI   = R8PI()
      ZERO = 0.D0
      IER  = 0
      NDIM = 3
      MLGGMA = NOMA//'.GROUPEMA'
      MLGNMA = NOMA//'.NOMMAI'
      MLGCNX = NOMA//'.CONNEX'
C
C --- CONSTRUCTION DES CARTES ET ALLOCATION
      CARTAR = NOMU//'.CARARCPO'
      TMPNAR = CARTAR//'.NCMP'
      TMPVAR = CARTAR//'.VALV'
      CALL ALCART('G',CARTAR,NOMA,'CAARPO')
      CALL JEVEUO(TMPNAR,'E',JDCC)
      CALL JEVEUO(TMPVAR,'E',JDVC)
      MLGCOO = NOMA//'.COORDO    .VALE'
      CALL JEVEUO(MLGCOO,'L',JDCO)
C
      CALL WKVECT('&&TMPPOUTRE_COURBE','V V K24',LMAX,JDLS)
C
      ZK8(JDCC)   = 'RCOURB'
      ZK8(JDCC+1) = 'ORIE_ARC'
      ZK8(JDCC+2) = 'C_FLEX'
      ZK8(JDCC+3) = 'I_SIGM'
      ZK8(JDCC+4) = 'C_FLEX_Y'
      ZK8(JDCC+5) = 'I_SIGM_Y'
      ZK8(JDCC+6) = 'C_FLEX_Z'
      ZK8(JDCC+7) = 'I_SIGM_Z'
C
      MCLEPT(1) = 'POIN_TANG       '
      MCLEPT(2) = 'NOEUD_POIN_TANG '
      MCLEPT(3) = 'GROUP_NO_POIN_TG'
      MCLEPC(1) = 'CENTRE          '
      MCLEPC(2) = 'NOEUD_CENTRE    '
      MCLEPC(3) = 'GROUP_NO_CENTRE '
C
C --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CAARPO
      DO 10 IOC = 1 , NBOCC
         XRC  = ZERO
         XANG = ZERO
         XFL  = 1.D0
         XSI  = 1.D0
         XFLY = 1.D0
         XSIY = 1.D0
         XFLZ = 1.D0
         XSIZ = 1.D0
C
         CALL GETVEM(NOMA,'GROUP_MA','DEFI_ARC','GROUP_MA',
     &              IOC,IARG,LMAX,ZK24(JDLS),NG)
         CALL GETVEM(NOMA,'MAILLE','DEFI_ARC','MAILLE',
     &            IOC,IARG,LMAX,ZK24(JDLS),NM)
         CALL GETVR8('DEFI_ARC','RAYON',IOC,IARG,1,
     &               XRC      ,NR  )
         CALL GETVR8('DEFI_ARC','ORIE_ARC',IOC,IARG,1,
     &               XANG     ,NA  )
         CALL GETVR8('DEFI_ARC','COEF_FLEX',IOC,IARG,1,
     &               XFL      ,NF  )
         CALL GETVR8('DEFI_ARC','COEF_FLEX_XY',IOC,IARG,1,
     &               XFLY     ,NFY )
         CALL GETVR8('DEFI_ARC','COEF_FLEX_XZ',IOC,IARG,1,
     &               XFLZ     ,NFZ )
         CALL GETVR8('DEFI_ARC','INDI_SIGM',IOC,IARG,1,
     &               XSI      ,NS  )
         CALL GETVR8('DEFI_ARC','INDI_SIGM_XY',IOC,IARG,1,
     &               XSIY     ,NSY )
         CALL GETVR8('DEFI_ARC','INDI_SIGM_XZ',IOC,IARG,1,
     &               XSIZ     ,NSZ )
         CALL GETVTX('DEFI_ARC','CRITERE',IOC,IARG,1,
     &               CRIT     ,N1  )
         CALL GETVR8('DEFI_ARC','PRECISION',IOC,IARG,1,
     &               EPSI     ,N2  )
C
         CALL UTCONO ( 'DEFI_ARC', MCLEPT, IOC, NOMA, NDIM, XTAN, NP )
C
         CALL UTCONO ( 'DEFI_ARC', MCLEPC, IOC, NOMA, NDIM, XCEN, NC )
C
         IF (NF.EQ.0.AND.NFY.NE.0) XFL = 0.0D0
C         ZR(JDVC)   = XRC
C         ZR(JDVC+1) = XANG * DGRD
         ZR(JDVC+2) = XFL
         ZR(JDVC+3) = XSI
         ZR(JDVC+4) = XFLY
         ZR(JDVC+5) = XSIY
         ZR(JDVC+6) = XFLZ
         ZR(JDVC+7) = XSIZ
C
C ---    "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
         IF (NG.GT.0) THEN
C ON STOCKE DIRECTEMENT LES DONNEES UTILISATEUR : RAYON ET ORIE_ARC
            IF ( NC.EQ.0 .AND. NP.EQ.0 ) THEN
              DO 556 IGM=1,NG
               CALL JEVEUO(JEXNOM(MLGGMA,ZK24(JDLS-1+IGM)),'L',JDGM)
               CALL JELIRA(JEXNOM(MLGGMA,ZK24(JDLS-1+IGM)),'LONUTI',
     &                     NMG,ZK8BID)
                 DO 576 IJM=1,NMG
                  IMG = ZI(JDGM+IJM-1)
                  ZR(JDVC)   = XRC
                  ZR(JDVC+1) = XANG * DGRD
                  CALL NOCART(CARTAR,3,' ','NUM',1,' ',IMG,' ',8)
576               CONTINUE
556           CONTINUE

            ELSEIF ( NC.NE.0 .OR. NP.NE.0 ) THEN
               CALL ACNOEX(NOMA,'GRMA',ZK24(JDLS),NG,NO1,NO2)
               DO 32 I = 1 , 3
                  X1(I) = ZR(JDCO+(NO1-1)*3+I-1)
                  X2(I) = ZR(JDCO+(NO2-1)*3+I-1)
 32            CONTINUE
               IF ( NC.NE.0 ) THEN
                  CALL ORIEN2 ( X1, X2, XCEN, ANGL )
                  DM   = PADIST( 3, X1, X2 ) / 2.D0
                  XRC1 = PADIST( 3, X1, XCEN )
                  XRC2 = PADIST( 3, X2, XCEN )
                  IF (CRIT(1:4).EQ.'RELA') THEN
                     TOLE = EPSI*DM
                  ELSE
                     TOLE = EPSI
                  ENDIF
                  IF (ABS(XRC1-XRC2).GT.TOLE) THEN
                  IER = IER + 1
                     VALI = IOC
                     VALK (1) = ZK24(JDLS)
                  CALL U2MESG('E', 'MODELISA8_9',2,VALK,1,VALI,0,0.D0)
                  XRC1 = ZERO
                  ENDIF
                  PHI = 2.D0*ASIN(DM/XRC1)
                  RR         = XRC1
C                 ZR(JDVC)   = RR
C                 ZR(JDVC+1) = ANGL(3) + PI
C CHAQUE MAILLE DE LA LISTE PEUT AVOIR UN GAMMA DIFFERENT
                  DO 347 I=1,3
                     XC1(I)= X1(I) - XCEN(I)
                     XC2(I)= X2(I) - XCEN(I)
347               CONTINUE
                  CALL PROVEC(XC1,XC2,V1)
                  CALL ACNOCE(NOMA,'GRMA',ZK24(JDLS),NG,ZR(JDCO),
     &                           RR,XCEN,TOLE,V1,ISPV)
                  DO 557 IGM=1,NG
                   CALL JEVEUO(JEXNOM(MLGGMA,ZK24(JDLS-1+IGM)),'L',JDGM)
                   CALL JELIRA(JEXNOM(MLGGMA,ZK24(JDLS-1+IGM)),'LONUTI',
     &                         NMG,ZK8BID)
                     DO 57 IJM=1,NMG
                      IMG = ZI(JDGM+IJM-1)
                      CALL JEVEUO(JEXNUM(MLGCNX,IMG),'L',JDNO)
                      NN1 = ZI(JDNO)
                      NN2 = ZI(JDNO+1)
                      DO 427 I = 1 , 3
                        X1(I) = ZR(JDCO+(NN1-1)*3+I-1)
                        X2(I) = ZR(JDCO+(NN2-1)*3+I-1)
427                   CONTINUE
                      CALL ORIEN2 ( X1, X2, XCEN, ANGL )
                      ZR(JDVC)   = RR
                      ZR(JDVC+1) = ANGL(3) + PI
                      CALL NOCART(CARTAR,3,' ','NUM',1,' ',IMG,' ',8)
57                   CONTINUE
557               CONTINUE
               ELSE
                  CALL ORIEN2 ( X1, X2, XTAN, ANGL )
                  DM  = PADIST( 3, X1, X2 ) / 2.D0
                  TX1 = PADIST( 3, X1, XTAN )
                  TX2 = PADIST( 3, X2, XTAN )
                  IF (CRIT(1:4).EQ.'RELA') THEN
                     TOLE = EPSI*DM
                  ELSE
                     TOLE = EPSI
                  ENDIF
                  IF (ABS(TX1-TX2).GT.TOLE) THEN
                     IER = IER + 1
                     VALI = IOC
                     VALK (1) = ZK24(JDLS)
                     VALK (2) = ' '
      CALL U2MESG('E', 'MODELISA8_10',2,VALK,1,VALI,0,0.D0)
                  ENDIF
                  PHIS2 = PI / 2.D0 - ASIN( DM / TX1 )
                  RR   = DM / SIN( PHIS2 )
                  ZR(JDVC)   = RR
                  ZR(JDVC)   = DM / SIN( PHIS2 )
                  ZR(JDVC+1) = ANGL(3)
                  PHI=2.D0*PHIS2
C
C COORDONNEES DU CENTRE
C
                  DO 33 I=1,3
                     XM(I)=(X1(I)+X2(I))/2.D0
                     TM(I)=XM(I)-XTAN(I)
                     XCEN(I)=XM(I)+TM(I)/(TAN(PHIS2)*TAN(PHIS2))
  33              CONTINUE

C CHAQUE MAILLE DE LA LISTE PEUT AVOIR UN GAMMA DIFFERENT
                  DO 348 I=1,3
                     XC1(I)= X1(I) - XCEN(I)
                     XC2(I)= X2(I) - XCEN(I)
348               CONTINUE
                  CALL PROVEC(XC1,XC2,V1)
                  CALL ACNOCE(NOMA,'LIMA',ZK24(JDLS),NM,ZR(JDCO),RR,
     &                  XCEN,TOLE,V1,ISPV)
                  DO 558 IGM=1,NG
                   CALL JEVEUO(JEXNOM(MLGGMA,ZK24(JDLS-1+IGM)),'L',JDGM)
                   CALL JELIRA(JEXNOM(MLGGMA,ZK24(JDLS-1+IGM)),'LONUTI',
     &                         NMG,ZK8BID)
                     DO 58 IJM=1,NMG
                      IMG = ZI(JDGM+IJM-1)
                      CALL JEVEUO(JEXNUM(MLGCNX,IMG),'L',JDNO)
                      NN1 = ZI(JDNO)
                      NN2 = ZI(JDNO+1)
                      DO 428 I = 1 , 3
                        X1(I) = ZR(JDCO+(NN1-1)*3+I-1)
                        X2(I) = ZR(JDCO+(NN2-1)*3+I-1)
428                   CONTINUE
                      CALL ORIEN2 ( X1, X2, XCEN, ANGL )
                      ZR(JDVC)   = RR
                      ZR(JDVC+1) = ANGL(3) + PI
                      CALL NOCART(CARTAR,3,' ','NUM',1,' ',IMG,' ',8)
58                   CONTINUE
558               CONTINUE
               ENDIF
               WRITE(IFM,*)'MOT CLE FACTEUR DEFI_ARC, MOT CLE GROUP_MA'
               WRITE(IFM,*)' RCOURB: ',ZR(JDVC)
               WRITE(IFM,*)' ANGLE_ARC: ',PHI*RDDG
               WRITE(IFM,*)' CENTRE: ',(XCEN(I),I=1,3)
            ENDIF
         ENDIF
C
C ---    "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
         IF (NM.GT.0) THEN
            IF ( NC.EQ.0 .AND. NP.EQ.0 ) THEN
C ON STOCKE DIRECTEMENT LES DONNEES UTILISATEUR : RAYON ET ORIE_ARC
              DO 559 IJM=1,NM
                ZR(JDVC)   = XRC
                ZR(JDVC+1) = XANG * DGRD
                NOMAIL = ZK24(JDLS-1+IJM)
                CALL NOCART(CARTAR,3,' ','NOM',1,NOMAIL,
     &                           0,' ',8)
559           CONTINUE

            ELSEIF ( NC.NE.0 .OR. NP.NE.0 ) THEN
               CALL ACNOEX(NOMA,'LIMA',ZK24(JDLS),NM,NO1,NO2)
               DO 42 I = 1 , 3
                  X1(I) = ZR(JDCO+(NO1-1)*3+I-1)
                  X2(I) = ZR(JDCO+(NO2-1)*3+I-1)
 42            CONTINUE
               IF ( NC.NE.0 ) THEN
                  CALL ORIEN2 ( X1, X2, XCEN, ANGL )
                  DM   = PADIST( 3, X1, X2 ) / 2.D0
                  XRC1 = PADIST( 3, X1, XCEN )
                  XRC2 = PADIST( 3, X2, XCEN )
                  IF (CRIT(1:4).EQ.'RELA') THEN
                     TOLE = EPSI*DM
                  ELSE
                     TOLE = EPSI
                  ENDIF
                  IF (ABS(XRC1-XRC2).GT.TOLE) THEN
                     IER = IER + 1
                     VALI = IOC
                     VALK (1) = ZK24(JDLS)
                     VALK (2) = ' '
      CALL U2MESG('E', 'MODELISA8_11',2,VALK,1,VALI,0,0.D0)
                     XRC1 = ZERO
                  ENDIF
                  PHI = 2.D0*ASIN(DM/XRC1)
                  RR   = XRC1

C CHAQUE MAILLE DE LA LISTE PEUT AVOIR UN GAMMA DIFFERENT
                  DO 645 I=1,3
                     XC1(I)= X1(I) - XCEN(I)
                     XC2(I)= X2(I) - XCEN(I)
645               CONTINUE
                  CALL PROVEC(XC1,XC2,V1)
                  CALL ACNOCE(NOMA,'LIMA',ZK24(JDLS),NM,ZR(JDCO),RR,
     &                        XCEN,TOLE,V1,ISPV)

                  DO 55 IJM=1,NM
                     CALL JENONU(JEXNOM(MLGNMA,ZK24(JDLS-1+IJM)),NUMMAI)
                     CALL JEVEUO(JEXNUM(MLGCNX,NUMMAI),'L',JDNO)
                     NN1 = ZI(JDNO)
                     NN2 = ZI(JDNO+1)
                     DO 425 I = 1 , 3
                        X1(I) = ZR(JDCO+(NN1-1)*3+I-1)
                        X2(I) = ZR(JDCO+(NN2-1)*3+I-1)
425                  CONTINUE
                     CALL ORIEN2 ( X1, X2, XCEN, ANGL )
                     ZR(JDVC)   = RR
                     ZR(JDVC+1) = ANGL(3) + PI
                     NOMAIL = ZK24(JDLS-1+IJM)
                     CALL NOCART(CARTAR,3,' ','NOM',1,NOMAIL,
     &                           0,' ',8)
55                CONTINUE
               ELSE
                  CALL ORIEN2 ( X1, X2, XTAN, ANGL )
                  DM  = PADIST( 3, X1, X2 ) / 2.D0
                  TX1 = PADIST( 3, X1, XTAN )
                  TX2 = PADIST( 3, X2, XTAN )
                  IF (CRIT(1:4).EQ.'RELA') THEN
                     TOLE = EPSI*DM
                  ELSE
                     TOLE = EPSI
                  ENDIF
                  IF (ABS(TX1-TX2).GT.TOLE) THEN
                     IER = IER + 1
                     VALI = IOC
                     VALK (1) = ZK24(JDLS)
                     VALK (2) = ' '
      CALL U2MESG('E', 'MODELISA8_10',2,VALK,1,VALI,0,0.D0)
                  ENDIF
                  PHIS2 = PI / 2.D0 - ASIN( DM / TX1 )
                  RR   = DM / SIN( PHIS2 )
                  PHI=2.D0*PHIS2
C
C COORDONNEES DU CENTRE
C
                  DO 63 I=1,3
                     XM(I)=(X1(I)+X2(I))/2.D0
                     TM(I)=XM(I)-XTAN(I)
                     XCEN(I)=XM(I)+TM(I)/(TAN(PHIS2)*TAN(PHIS2))
  63              CONTINUE

C CHAQUE MAILLE DE LA LISTE PEUT AVOIR UN GAMMA DIFFERENT
                  DO 646 I=1,3
                     XC1(I)= X1(I) - XCEN(I)
                     XC2(I)= X2(I) - XCEN(I)
646               CONTINUE
                  CALL PROVEC(XC1,XC2,V1)
                  CALL ACNOCE(NOMA,'LIMA',ZK24(JDLS),NM,ZR(JDCO),RR,
     &                        XCEN,TOLE,V1,ISPV)

                  DO 56 IJM=1,NM
                     CALL JENONU(JEXNOM(MLGNMA,ZK24(JDLS-1+IJM)),NUMMAI)
                     CALL JEVEUO(JEXNUM(MLGCNX,NUMMAI),'L',JDNO)
                     NN1 = ZI(JDNO)
                     NN2 = ZI(JDNO+1)
                     DO 426 I = 1 , 3
                        X1(I) = ZR(JDCO+(NN1-1)*3+I-1)
                        X2(I) = ZR(JDCO+(NN2-1)*3+I-1)
426                  CONTINUE
                     CALL ORIEN2 ( X1, X2, XCEN, ANGL )
                     ZR(JDVC)   = RR
                     ZR(JDVC+1) = ANGL(3) + PI
                     NOMAIL = ZK24(JDLS-1+IJM)
                     CALL NOCART(CARTAR,3,' ','NOM',1,NOMAIL,
     &                           0,' ',8)
56                CONTINUE
               ENDIF
               WRITE(IFM,*)'MOT CLE FACTEUR DEFI_ARC, MOT CLE MAILLE'
               WRITE(IFM,*)' RCOURB: ',ZR(JDVC)
               WRITE(IFM,*)' ANGLE_ARC: ',PHI*RDDG
               WRITE(IFM,*)' CENTRE: ',(XCEN(I),I=1,3)
            ENDIF
         ENDIF
C
 10   CONTINUE
C
      CALL JEDETR('&&TMPPOUTRE_COURBE')
      CALL JEDETR(TMPNAR)
      CALL JEDETR(TMPVAR)
      IF (IER.NE.0) THEN
         CALL U2MESS('F','MODELISA_13')
      ENDIF
C
      CALL JEDEMA()
      END
