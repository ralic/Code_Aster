      SUBROUTINE RAIREP(NOMA,IOC,KM,RIGI,NBGR,LIGRMA,NBNO,
     &  TABNOE,RIGNOE,RIGTO,AMOTO,RIROT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER      IOC, NBGR, NBNO
      CHARACTER*8  NOMA, LIGRMA(NBGR), TABNOE(*),KM
      REAL*8       RIGNOE(*), RIGTO(*), AMOTO(*),RIROT(3)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 19/10/2010   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8  K8B
      CHARACTER*8  NOMGR, NOMNOE
      CHARACTER*24 MAGRNO, MANONO, MAGRMA, MANOMA
      REAL*8       R8B, ZERO, X(9), Y(9), Z(9), RIGI(6)
      REAL*8       A(3), B(3), C(3), U(3)
      LOGICAL      LFONC, TRANS
C
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
      NOEMAX = 0
C
C     --- DESCRIPTION NOEUDS STRUCTURE ---
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
C
C       RECUPERATION DU CENTRE
C
      CALL GETVR8('RIGI_PARASOL','COOR_CENTRE',IOC,1,0,R8B,NCG)
      CALL GETVEM(NOMA,'NOEUD','RIGI_PARASOL','NOEUD_CENTRE',
     &                   IOC,1,0,K8B,NNO)
      CALL GETVEM(NOMA,'GROUP_NO','RIGI_PARASOL','GROUP_NO_CENTRE',
     &                      IOC,1,0,K8B,NGN)
      IF (NCG.NE.0) THEN
        CALL GETVR8('RIGI_PARASOL','COOR_CENTRE',IOC,1,3,C,NCG)
        XG = C(1)
        YG = C(2)
        ZG = C(3)
      ELSEIF (NNO.NE.0) THEN
        CALL GETVEM(NOMA,'NOEUD','RIGI_PARASOL','NOEUD_CENTRE',
     &                     IOC,1,1,NOMNOE,NNO)
        CALL JENONU(JEXNOM(MANONO,NOMNOE),INOE)
        XG = ZR(JCOOR+3*(INOE-1)+1-1)
        YG = ZR(JCOOR+3*(INOE-1)+2-1)
        ZG = ZR(JCOOR+3*(INOE-1)+3-1)
      ELSEIF (NGN.NE.0) THEN
        CALL GETVEM(NOMA,'GROUP_NO','RIGI_PARASOL','GROUP_NO_CENTRE',
     &                        IOC,1,1,NOMGR,NGN)
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
      CALL GETVR8('RIGI_PARASOL','COEF_GROUP',IOC,1,0,R8B,NCG)
      IF (NCG.NE.0) THEN
        CALL WKVECT('&&RAIREP.COEGRO','V V R',NBGR,ICOEGR)
        CALL GETVR8('RIGI_PARASOL','COEF_GROUP',IOC,1,NBGR,
     &   ZR(ICOEGR),NCG)
      ELSE
        CALL WKVECT('&&RAIREP.FONGRO','V V K8',NBGR,IFONGR)
        LFONC = .TRUE.
        CALL GETVID('RIGI_PARASOL','FONC_GROUP',IOC,1,NBGR,ZK8(IFONGR),
     &  NFG)
      ENDIF
C
      DO 20 I = 1,NBGR
         CALL JELIRA(JEXNOM(MAGRMA,LIGRMA(I)),'LONUTI',NB,K8B)
         CALL JEVEUO(JEXNOM(MAGRMA,LIGRMA(I)),'L',LDGM)
         DO 22 IN = 0,NB-1
           CALL JELIRA(JEXNUM(MANOMA,ZI(LDGM+IN)),'LONMAX',NM,K8B)
           CALL JEVEUO(JEXNUM(MANOMA,ZI(LDGM+IN)),'L',LDNM)
           DO 24 NN = 1, NM
              INOE = ZI(LDNM+NN-1)
              NOEMAX = MAX(NOEMAX,INOE)
 24        CONTINUE
 22      CONTINUE
 20   CONTINUE
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
             CALL U2MESS('F','MODELISA6_35')
           ENDIF
           CALL PROVEC(A,B,C)
           SURF=DDOT(3,C,1,C,1)
           ZR(ISURMA+IM-1) = SQRT(SURF)*0.5D0
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
         RIG4 = RIG4 + (RIGI(2)*ZZ**2+RIGI(3)*YY**2)*ZR(ICOEF+IJ-1)
         RIG5 = RIG5 + (RIGI(1)*ZZ**2+RIGI(3)*XX**2)*ZR(ICOEF+IJ-1)
         RIG6 = RIG6 + (RIGI(2)*XX**2+RIGI(1)*YY**2)*ZR(ICOEF+IJ-1)
         RIG45 = RIG45 - RIGI(3)*XX*YY*ZR(ICOEF+IJ-1)
         RIG46 = RIG46 - RIGI(2)*XX*ZZ*ZR(ICOEF+IJ-1)
         RIG56 = RIG56 - RIGI(1)*YY*ZZ*ZR(ICOEF+IJ-1)
 50   CONTINUE
      NBNO = II

      TRANS=(KM(1:7) .EQ. 'K_T_D_N').OR.(KM(1:7) .EQ. 'K_T_D_L').OR.
     &      (KM(1:7) .EQ. 'A_T_D_N').OR.(KM(1:7) .EQ. 'A_T_D_L')

      IF ( TRANS ) THEN
C        PAS DE RAIDEUR EN ROTATION SUR LES DISCRETS
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
         RIG4 = RIGI(4) - RIG4
         RIG5 = RIGI(5) - RIG5
         RIG6 = RIGI(6) - RIG6
         RIROT(1) = RIG4
         RIROT(2) = RIG5
         RIROT(3) = RIG6
      ENDIF
C
      II = 0
      DO 51 IJ = 1, NOEMAX
         IF (ZI(IDNO+IJ-1).EQ.0) GOTO 51
         II = II + 1
         R1 = RIGI(1)*ZR(ICOEF+IJ-1)
         R2 = RIGI(2)*ZR(ICOEF+IJ-1)
         R3 = RIGI(3)*ZR(ICOEF+IJ-1)
         R4 = RIG4*ZR(ICOEF+IJ-1)
         R5 = RIG5*ZR(ICOEF+IJ-1)
         R6 = RIG6*ZR(ICOEF+IJ-1)
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
 9999 CONTINUE
      CALL JEDETR('&&RAIREP.COEGRO')
      CALL JEDETR('&&RAIREP.FONGRO')
      CALL JEDETR('&&RAIREP.COENO')
      CALL JEDETR('&&RAIREP.PARNO')
      CALL JEDETR('&&RAIREP.SURMAI')
C
      CALL JEDEMA()
      END
