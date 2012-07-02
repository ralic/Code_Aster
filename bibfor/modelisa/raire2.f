      SUBROUTINE RAIRE2(NOMA,RIGI,NBGR,LIGRMA,NBNOEU,NBNO,
     &  TABNOE,RIGNOE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      NBGR, NBNO, NBNOEU, TABNOE(NBNOEU)
      CHARACTER*8  NOMA, LIGRMA(NBGR)
      REAL*8       RIGNOE(6*NBNOEU)
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
      CHARACTER*8  NOMGR, NOMNOE
      CHARACTER*24 MAGRNO, MANONO, MAGRMA, MANOMA
      REAL*8       R8B, ZERO, X(8), Y(8), Z(8), RIGI(6)
      REAL*8       A(3), B(3), C(3), U(3)
      LOGICAL      LFONC
      INTEGER      IARG
C
C-----------------------------------------------------------------------
      INTEGER I ,ICOEF ,ICOEGR ,IDNO ,IFONGR ,IFR ,II 
      INTEGER IJ ,IM ,IN ,INOE ,IRET ,ISURMA ,IUNIFI 
      INTEGER JCOOR ,LDGM ,LDGN ,LDNM ,NB ,NBMA ,NCF 
      INTEGER NCG ,NFG ,NGN ,NM ,NN ,NNO ,NOEMAX 

      REAL*8 COEF ,DDOT ,DIST ,HC ,R1 ,R2 ,R3 
      REAL*8 R4 ,R5 ,R6 ,RIG4 ,RIG45 ,RIG46 ,RIG5 
      REAL*8 RIG56 ,RIG6 ,SURF ,SURTOT ,XC ,XG ,XX 
      REAL*8 YC ,YG ,YY ,ZG ,ZZ 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      ZERO = 0.D0
      IFR = IUNIFI('RESULTAT')
      LFONC = .FALSE.
C
C
C     --- ON RECUPERE LES POINTS D'ANCRAGE ---
C
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
      XG = ZERO
      YG = ZERO
      ZG = ZERO
      CALL GETVR8('ENER_SOL','COOR_CENTRE',1,IARG,0,R8B,NCG)
      CALL GETVEM(NOMA,'NOEUD','ENER_SOL','NOEUD_CENTRE',
     &               1,IARG,0,K8B,NNO)
      CALL GETVEM(NOMA,'GROUP_NO','ENER_SOL','GROUP_NO_CENTRE',
     &                  1,IARG,0,K8B,NGN)
      IF (NCG.NE.0) THEN
        CALL GETVR8('ENER_SOL','COOR_CENTRE',1,IARG,3,C,NCG)
        XG = C(1)
        YG = C(2)
        ZG = C(3)
      ELSEIF (NNO.NE.0) THEN
        CALL GETVEM(NOMA,'NOEUD','ENER_SOL','NOEUD_CENTRE',
     &                 1,IARG,1,NOMNOE,NNO)
        CALL JENONU(JEXNOM(MANONO,NOMNOE),INOE)
        XG = ZR(JCOOR+3*(INOE-1)+1-1)
        YG = ZR(JCOOR+3*(INOE-1)+2-1)
        ZG = ZR(JCOOR+3*(INOE-1)+3-1)
      ELSEIF (NGN.NE.0) THEN
        CALL GETVEM(NOMA,'GROUP_NO','ENER_SOL','GROUP_NO_CENTRE',
     &                    1,IARG,1,NOMGR,NGN)
        CALL JEVEUO(JEXNOM(MAGRNO,NOMGR),'L',LDGN)
        INOE = ZI(LDGN)
        XG = ZR(JCOOR+3*(INOE-1)+1-1)
        YG = ZR(JCOOR+3*(INOE-1)+2-1)
        ZG = ZR(JCOOR+3*(INOE-1)+3-1)
      ENDIF
C
C       RECUPERATION DES COEFS OU FONCTIONS DE GROUPE
C
      CALL GETVR8('ENER_SOL','COEF_GROUP',1,IARG,0,R8B,NCG)
      IF (NCG.NE.0) THEN
        CALL WKVECT('&&RAIRE2.COEGRO','V V R',NBGR,ICOEGR)
        CALL GETVR8('ENER_SOL','COEF_GROUP',1,IARG,NBGR,
     &   ZR(ICOEGR),NCG)
      ELSE
        CALL GETVID('ENER_SOL','FONC_GROUP',1,IARG,0,K8B,NCF)
        IF (NCF.EQ.0) CALL U2MESS('F','MODELISA6_33')
        CALL WKVECT('&&RAIRE2.FONGRO','V V K8',NBGR,IFONGR)
        LFONC = .TRUE.
        CALL GETVID('ENER_SOL','FONC_GROUP',1,IARG,NBGR,ZK8(IFONGR),NFG)
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
      CALL WKVECT('&&RAIRE2.COENO','V V R',NOEMAX,ICOEF)
C
C        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
C
      CALL WKVECT('&&RAIRE2.PARNO','V V I',NOEMAX,IDNO)
C
C
C     CALCUL DES SURFACES ELEMENTAIRES ET DE LA SURFACE TOTALE
C
      CALL WKVECT('&&RAIRE2.SURMAI','V V R',NBMA,ISURMA)
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
           IF (NM.EQ.3.OR.NM.EQ.6) THEN
             B(1) = X(2) - X(1)
             B(2) = Y(2) - Y(1)
             B(3) = Z(2) - Z(1)
           ELSEIF (NM.EQ.4.OR.NM.EQ.8) THEN
             B(1) = X(4) - X(2)
             B(2) = Y(4) - Y(2)
             B(3) = Z(4) - Z(2)
           ELSE
             CALL U2MESS('F','MODELISA6_34')
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
C     CALCUL DES RAIDEURS DE TORSION
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
      RIG4 = RIGI(4) - RIG4
      RIG5 = RIGI(5) - RIG5
      RIG6 = RIGI(6) - RIG6
      WRITE(IFR,1001) RIG4,RIG5,RIG6
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
         RIGNOE(6*(II-1)+1) = R1
         RIGNOE(6*(II-1)+2) = R2
         RIGNOE(6*(II-1)+3) = R3
         RIGNOE(6*(II-1)+4) = R4
         RIGNOE(6*(II-1)+5) = R5
         RIGNOE(6*(II-1)+6) = R6
         TABNOE(II) = IJ
 51   CONTINUE
C
 1001 FORMAT(1X,'RAIDEURS DE ROTATION A REPARTIR:',/
     &      1X,' KRX: ',1X,1PE12.5,' KRY: ',1X,1PE12.5,
     &      ' KRZ: ',1X,1PE12.5)
      CALL JEDETR('&&RAIRE2.COEGRO')
      CALL JEDETR('&&RAIRE2.FONGRO')
      CALL JEDETR('&&RAIRE2.COENO')
      CALL JEDETR('&&RAIRE2.PARNO')
      CALL JEDETR('&&RAIRE2.SURMAI')
C
      CALL JEDEMA()
      END
