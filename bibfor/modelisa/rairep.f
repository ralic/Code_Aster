      SUBROUTINE RAIREP(NOMA,IOC,KM,RIGI,NBGR,LIGRMA,NBNO,
     &  TABNOE,RIGNOE,RIGTO,AMOTO,RIROT,NDIM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      IOC, NBGR, NBNO,NDIM
      CHARACTER*8  NOMA, LIGRMA(NBGR), TABNOE(*),KM
      REAL*8       RIGNOE(*), RIGTO(*), AMOTO(*),RIROT(3)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
      CHARACTER*8  K8B
      CHARACTER*8  NOMGR, NOMNOE,TYPM
      CHARACTER*24 MAGRNO, MANONO, MAGRMA, MANOMA,MATYMA
      REAL*8       R8B, ZERO, X(9), Y(9), Z(9), RIGI(6)
      REAL*8       A(3), B(3), C(3), U(3)
      LOGICAL      LFONC, TRANS
      INTEGER      IARG,APPUI
C
C-----------------------------------------------------------------------
      INTEGER I ,ICOEF ,ICOEGR ,IDNO ,IER ,IFONGR ,II 
      INTEGER IJ ,IM ,IN ,INOE ,IRET ,ISURMA ,JCOOR 
      INTEGER LDGM ,LDGN ,LDNM ,LTYP ,NB ,NBMA ,NCG 
      INTEGER NFG ,NGN ,NM ,NN ,NNO ,NOEMAX ,NTOPO 
      INTEGER NUMA 
      REAL*8 COEF ,DDOT ,DIST ,HC ,R1 ,R2 ,R3 
      REAL*8 R4 ,R5 ,R6 ,RIG3 ,RIG4 ,RIG45 ,RIG46 
      REAL*8 RIG5 ,RIG56 ,RIG6 ,SURF ,SURTOT ,XC ,XG 
      REAL*8 XX ,YC ,YG ,YY ,ZG ,ZZ 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      ZERO = 0.D0
      LFONC = .FALSE.

C
C     --- ON RECUPERE LES POINTS D'ANCRAGE ---
C
C
C        --- ON ECLATE LE GROUP_NO EN NOEUDS ---
      CALL COMPMA(NOMA,NBGR,LIGRMA,NBMA)
      MAGRNO = NOMA//'.GROUPENO'
      MANONO = NOMA//'.NOMNOE'
      MAGRMA = NOMA//'.GROUPEMA'
      MANOMA = NOMA//'.CONNEX'
      MATYMA = NOMA//'.TYPMAIL'

      NOEMAX = 0

C
C     --- DESCRIPTION NOEUDS STRUCTURE ---
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
C
C       RECUPERATION DU CENTRE
C
      CALL GETVR8('RIGI_PARASOL','COOR_CENTRE',IOC,IARG,0,R8B,NCG)
      CALL GETVEM(NOMA,'NOEUD','RIGI_PARASOL','NOEUD_CENTRE',
     &                   IOC,IARG,0,K8B,NNO)
      CALL GETVEM(NOMA,'GROUP_NO','RIGI_PARASOL','GROUP_NO_CENTRE',
     &                      IOC,IARG,0,K8B,NGN)
      IF (NCG.NE.0) THEN
        CALL GETVR8('RIGI_PARASOL','COOR_CENTRE',IOC,IARG,3,C,NCG)
        XG = C(1)
        YG = C(2)
        ZG = C(3)
      ELSEIF (NNO.NE.0) THEN
        CALL GETVEM(NOMA,'NOEUD','RIGI_PARASOL','NOEUD_CENTRE',
     &                     IOC,IARG,1,NOMNOE,NNO)
        CALL JENONU(JEXNOM(MANONO,NOMNOE),INOE)
        XG = ZR(JCOOR+3*(INOE-1)+1-1)
        YG = ZR(JCOOR+3*(INOE-1)+2-1)
        ZG = ZR(JCOOR+3*(INOE-1)+3-1)
      ELSEIF (NGN.NE.0) THEN
        CALL GETVEM(NOMA,'GROUP_NO','RIGI_PARASOL','GROUP_NO_CENTRE',
     &                        IOC,IARG,1,NOMGR,NGN)
        CALL JEVEUO(JEXNOM(MAGRNO,NOMGR),'L',LDGN)
        INOE = ZI(LDGN)
        CALL JENUNO(JEXNUM(MANONO,INOE),NOMNOE)
        XG = ZR(JCOOR+3*(INOE-1)+1-1)
        YG = ZR(JCOOR+3*(INOE-1)+2-1)
        ZG = ZR(JCOOR+3*(INOE-1)+3-1)
      ENDIF
C
C       RECUPERATION DES COEFS OU FONCTIONS DE GROUPE
C
      CALL GETVR8('RIGI_PARASOL','COEF_GROUP',IOC,IARG,0,R8B,NCG)
      IF (NCG.NE.0) THEN
        CALL WKVECT('&&RAIREP.COEGRO','V V R',NBGR,ICOEGR)
        CALL GETVR8('RIGI_PARASOL','COEF_GROUP',IOC,IARG,NBGR,
     &   ZR(ICOEGR),NCG)
      ELSE
        CALL WKVECT('&&RAIREP.FONGRO','V V K8',NBGR,IFONGR)
        LFONC = .TRUE.
        CALL GETVID('RIGI_PARASOL','FONC_GROUP',IOC,IARG,NBGR,
     &              ZK8(IFONGR),
     &  NFG)
      ENDIF
C


      IF (NDIM.EQ.2)THEN
        APPUI=1
      ELSE
C     LA DIMENSION DE L'APPUI N'EST PAS ENCORE DETERMINEE
        APPUI=-1
      ENDIF

      CALL JEVEUO(MATYMA,'L',LTYP)
      DO 20 I = 1,NBGR
         CALL JELIRA(JEXNOM(MAGRMA,LIGRMA(I)),'LONUTI',NB,K8B)
         CALL JEVEUO(JEXNOM(MAGRMA,LIGRMA(I)),'L',LDGM)
         DO 22 IN = 0,NB-1
           NUMA=ZI(LDGM+IN)
           CALL ASSERT(NUMA.GT.0)
           CALL JELIRA(JEXNUM(MANOMA,NUMA),'LONMAX',NM,K8B)
           CALL JEVEUO(JEXNUM(MANOMA,NUMA),'L',LDNM)

           CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(LTYP-1+NUMA)),TYPM)
           CALL DISMOI('F','DIM_TOPO',TYPM,'TYPE_MAILLE',NTOPO,K8B,IER)

           IF (APPUI.EQ.-1)THEN
C            LA DIMENSION DE LA PREMIERE MAILLE DEFINIT L'APPUI
             APPUI=NTOPO
           ELSEIF ((APPUI.EQ.1).OR.(APPUI.EQ.2))THEN
             IF (APPUI.NE.NTOPO)THEN
               CALL U2MESS('F','MODELISA6_35')
             ENDIF
           ELSE
             CALL U2MESS('F','MODELISA6_29')
           ENDIF
           DO 24 NN = 1, NM
              INOE = ZI(LDNM+NN-1)
              NOEMAX = MAX(NOEMAX,INOE)
 24        CONTINUE
 22      CONTINUE
 20   CONTINUE
      CALL ASSERT(APPUI.NE.-1)

      CALL WKVECT('&&RAIREP.COENO','V V R',NOEMAX,ICOEF)
C
C        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
C
      CALL WKVECT('&&RAIREP.PARNO','V V I',NOEMAX,IDNO)
C
C
C     CALCUL DES SURFACES ELEMENTAIRES ET DE LA SURFACE TOTALE
C
      CALL WKVECT('&&RAIREP.SURMAI','V V R',NBMA,ISURMA)
      IM = 0
      SURTOT = ZERO
      DO 21 I = 1,NBGR
         CALL JELIRA(JEXNOM(MAGRMA,LIGRMA(I)),'LONUTI',NB,K8B)
         CALL JEVEUO(JEXNOM(MAGRMA,LIGRMA(I)),'L',LDGM)
         DO 23 IN = 0,NB-1
           IM = IM + 1
           CALL JELIRA(JEXNUM(MANOMA,ZI(LDGM+IN)),'LONMAX',NM,K8B)
           CALL JEVEUO(JEXNUM(MANOMA,ZI(LDGM+IN)),'L',LDNM)
           XC = ZERO
           YC = ZERO
           HC = ZERO
           DO 25 NN = 1, NM
             INOE = ZI(LDNM+NN-1)
             ZI(IDNO+INOE-1) = ZI(IDNO+INOE-1) + 1
             X(NN) = ZR(JCOOR+3*(INOE-1)+1-1)
             Y(NN) = ZR(JCOOR+3*(INOE-1)+2-1)
             Z(NN) = ZR(JCOOR+3*(INOE-1)+3-1)
             XC = XC + X(NN)
             YC = YC + Y(NN)
             HC = HC + Z(NN)
 25        CONTINUE
            XC = XC/NM
            YC = YC/NM
            HC = HC/NM

           IF (APPUI.EQ.1) THEN
             A(1) = X(2) - X(1)
             A(2) = Y(2) - Y(1)
             A(3) = Z(2) - Z(1)
             SURF = DDOT(2,A,1,A,1)
             ZR(ISURMA+IM-1)= SQRT(SURF)
           ELSEIF (APPUI.EQ.2)THEN
             A(1) = X(3) - X(1)
             A(2) = Y(3) - Y(1)
             A(3) = Z(3) - Z(1)
             IF (NM.EQ.3.OR.NM.EQ.6.OR.NM.EQ.7) THEN
               B(1) = X(2) - X(1)
               B(2) = Y(2) - Y(1)
               B(3) = Z(2) - Z(1)
             ELSEIF (NM.EQ.4.OR.NM.EQ.8.OR.NM.EQ.9) THEN
               B(1) = X(4) - X(2)
               B(2) = Y(4) - Y(2)
               B(3) = Z(4) - Z(2)
             ELSE
               CALL ASSERT(.FALSE.)
             ENDIF
             CALL PROVEC(A,B,C)
             SURF=DDOT(3,C,1,C,1)
             ZR(ISURMA+IM-1) = SQRT(SURF)*0.5D0
           ELSE
             CALL ASSERT(.FALSE.)
           ENDIF
           IF (LFONC) THEN
             U(1) = XG - XC
             U(2) = YG - YC
             U(3) = ZG - HC
             DIST=DDOT(3,U,1,U,1)
             DIST = SQRT(DIST)
             CALL FOINTE('F ',ZK8(IFONGR+I-1),1,'X',DIST,COEF,IRET)
             ZR(ISURMA+IM-1) = ZR(ISURMA+IM-1)*COEF
           ELSE
             ZR(ISURMA+IM-1) = ZR(ISURMA+IM-1)*ZR(ICOEGR+I-1)
           ENDIF
           SURTOT = SURTOT + ZR(ISURMA+IM-1)
           ZR(ISURMA+IM-1) = ZR(ISURMA+IM-1)/NM
 23      CONTINUE
 21   CONTINUE
C
C     CALCUL DES PONDERATIONS ELEMENTAIRES
C
      IM = 0
      DO 31 I = 1,NBGR
         CALL JELIRA(JEXNOM(MAGRMA,LIGRMA(I)),'LONUTI',NB,K8B)
         CALL JEVEUO(JEXNOM(MAGRMA,LIGRMA(I)),'L',LDGM)
         DO 33 IN = 0,NB-1
           IM = IM + 1
           CALL JELIRA(JEXNUM(MANOMA,ZI(LDGM+IN)),'LONMAX',NM,K8B)
           CALL JEVEUO(JEXNUM(MANOMA,ZI(LDGM+IN)),'L',LDNM)
           DO 35 NN = 1, NM
             DO 37 IJ = 1, NOEMAX
               IF (ZI(IDNO+IJ-1).EQ.0) GOTO 37
               IF (ZI(LDNM+NN-1).EQ.IJ) THEN
                  ZR(ICOEF+IJ-1) = ZR(ICOEF+IJ-1) +
     &             ZR(ISURMA+IM-1)/SURTOT
               ENDIF
 37          CONTINUE
 35        CONTINUE
 33      CONTINUE
 31   CONTINUE
      NBMA = IM
C
C     CALCUL DES RAIDEURS DE ROTATION
C
      II = 0
      RIG4 = ZERO
      RIG5 = ZERO
      RIG6 = ZERO
      RIG45 = ZERO
      RIG46 = ZERO
      RIG56 = ZERO
      DO 50 IJ = 1, NOEMAX
         IF (ZI(IDNO+IJ-1).EQ.0) GOTO 50
         II = II + 1
         XX = ZR(JCOOR+3*(IJ-1)+1-1) - XG
         YY = ZR(JCOOR+3*(IJ-1)+2-1) - YG
         ZZ = ZR(JCOOR+3*(IJ-1)+3-1) - ZG
         IF (NDIM.EQ.3)THEN
           RIG4 = RIG4 + (RIGI(2)*ZZ**2+RIGI(3)*YY**2)*ZR(ICOEF+IJ-1)
           RIG5 = RIG5 + (RIGI(1)*ZZ**2+RIGI(3)*XX**2)*ZR(ICOEF+IJ-1)
           RIG6 = RIG6 + (RIGI(2)*XX**2+RIGI(1)*YY**2)*ZR(ICOEF+IJ-1)
           RIG45 = RIG45 - RIGI(3)*XX*YY*ZR(ICOEF+IJ-1)
           RIG46 = RIG46 - RIGI(2)*XX*ZZ*ZR(ICOEF+IJ-1)
           RIG56 = RIG56 - RIGI(1)*YY*ZZ*ZR(ICOEF+IJ-1)
         ELSE
           RIG3 = RIG3 + (RIGI(2)*XX**2+RIGI(1)*YY**2)*ZR(ICOEF+IJ-1)
         ENDIF

 50   CONTINUE
      NBNO = II

      TRANS=(KM(1:7) .EQ. 'K_T_D_N').OR.(KM(1:7) .EQ. 'K_T_D_L').OR.
     &      (KM(1:7) .EQ. 'A_T_D_N').OR.(KM(1:7) .EQ. 'A_T_D_L')

      IF ( TRANS ) THEN
C        PAS DE RAIDEUR EN ROTATION SUR LES DISCRETS
         IF (NDIM.EQ.2)THEN
           RIGI(3)=ZERO
           RIG3   =ZERO
         ENDIF
         RIGI(4) = ZERO
         RIGI(5) = ZERO
         RIGI(6) = ZERO
         RIG4 = ZERO
         RIG5 = ZERO
         RIG6 = ZERO
         RIROT(1) = ZERO
         RIROT(2) = ZERO
         RIROT(3) = ZERO
      ELSE
         RIG3 = RIGI(3) - RIG3
         RIG4 = RIGI(4) - RIG4
         RIG5 = RIGI(5) - RIG5
         RIG6 = RIGI(6) - RIG6
         IF (NDIM.EQ.3)THEN
          RIROT(1) = RIG4
          RIROT(2) = RIG5
          RIROT(3) = RIG6
         ELSE
          RIROT(1) = RIG3
         ENDIF
      ENDIF
C
      II = 0
      DO 51 IJ = 1, NOEMAX
         IF (ZI(IDNO+IJ-1).EQ.0) GOTO 51
         II = II + 1
         R1 = RIGI(1)*ZR(ICOEF+IJ-1)
         R2 = RIGI(2)*ZR(ICOEF+IJ-1)
         IF (NDIM.EQ.3)THEN
           R3 = RIGI(3)*ZR(ICOEF+IJ-1)
           R4 = RIG4*ZR(ICOEF+IJ-1)
           R5 = RIG5*ZR(ICOEF+IJ-1)
           R6 = RIG6*ZR(ICOEF+IJ-1)
         ELSE
           R3 = RIG3*ZR(ICOEF+IJ-1)
           R4 = ZERO
           R5 = ZERO
           R6 = ZERO
         ENDIF
         CALL JENUNO(JEXNUM(MANONO,IJ),NOMNOE)
         IF (KM(1:1).EQ.'K') THEN
           RIGTO(6*(IJ-1)+1) = R1 + RIGTO(6*(IJ-1)+1)
           RIGTO(6*(IJ-1)+2) = R2 + RIGTO(6*(IJ-1)+2)
           RIGTO(6*(IJ-1)+3) = R3 + RIGTO(6*(IJ-1)+3)
           RIGTO(6*(IJ-1)+4) = R4 + RIGTO(6*(IJ-1)+4)
           RIGTO(6*(IJ-1)+5) = R5 + RIGTO(6*(IJ-1)+5)
           RIGTO(6*(IJ-1)+6) = R6 + RIGTO(6*(IJ-1)+6)
           R1 = RIGTO(6*(IJ-1)+1)
           R2 = RIGTO(6*(IJ-1)+2)
           R3 = RIGTO(6*(IJ-1)+3)
           R4 = RIGTO(6*(IJ-1)+4)
           R5 = RIGTO(6*(IJ-1)+5)
           R6 = RIGTO(6*(IJ-1)+6)
         ELSEIF (KM(1:1).EQ.'A') THEN
           AMOTO(6*(IJ-1)+1) = R1 + AMOTO(6*(IJ-1)+1)
           AMOTO(6*(IJ-1)+2) = R2 + AMOTO(6*(IJ-1)+2)
           AMOTO(6*(IJ-1)+3) = R3 + AMOTO(6*(IJ-1)+3)
           AMOTO(6*(IJ-1)+4) = R4 + AMOTO(6*(IJ-1)+4)
           AMOTO(6*(IJ-1)+5) = R5 + AMOTO(6*(IJ-1)+5)
           AMOTO(6*(IJ-1)+6) = R6 + AMOTO(6*(IJ-1)+6)
           R1 = AMOTO(6*(IJ-1)+1)
           R2 = AMOTO(6*(IJ-1)+2)
           R3 = AMOTO(6*(IJ-1)+3)
           R4 = AMOTO(6*(IJ-1)+4)
           R5 = AMOTO(6*(IJ-1)+5)
           R6 = AMOTO(6*(IJ-1)+6)
         ENDIF
         RIGNOE(6*(II-1)+1) = R1
         RIGNOE(6*(II-1)+2) = R2
         RIGNOE(6*(II-1)+3) = R3
         RIGNOE(6*(II-1)+4) = R4
         RIGNOE(6*(II-1)+5) = R5
         RIGNOE(6*(II-1)+6) = R6
         TABNOE(II) = NOMNOE
 51   CONTINUE
C
      CALL JEDETR('&&RAIREP.COEGRO')
      CALL JEDETR('&&RAIREP.FONGRO')
      CALL JEDETR('&&RAIREP.COENO')
      CALL JEDETR('&&RAIREP.PARNO')
      CALL JEDETR('&&RAIREP.SURMAI')
C
      CALL JEDEMA()
      END
