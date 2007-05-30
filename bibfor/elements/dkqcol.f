      SUBROUTINE DKQCOL ( FAMI,XYZL, OPTION, PGL, ICOU, INIV, DEPL, CDL,
     &                     NPG )
      IMPLICIT  NONE
      INTEGER       ICOU, INIV,NPG
      REAL*8        XYZL(3,*),PGL(3,*), DEPL(*), CDL(*)
      CHARACTER*16  OPTION
      CHARACTER*4   FAMI
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/05/2007   AUTEUR COURTOIS M.COURTOIS 
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
C     CONTRAINTES ET DEFORMATIONS DE L'ELEMENT DE PLAQUE DKQ
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  OPTION : NOM DE L'OPTION DE CALCUL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  ICOU   : NUMERO DE LA COUCHE
C     IN  INIV   : NIVEAU DANS LA COUCHE (-1:INF , 0:MOY , 1:SUP)
C     IN  DEPL   : DEPLACEMENTS
C     OUT CDL    : CONTRAINTES OU DEFORMATIONS AUX NOEUDS DANS LE REPERE
C                  INTRINSEQUE A L'ELEMENT
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER  NDIM,NNO,NNOS,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER       MULTIC,NE,JCACO,I,J,K,IE,JMATE,IC,ICPG,IG
      REAL*8        R8BID,ZIC,HIC,ZMIN,DEUX,X3I,EPAIS,EXCEN
      REAL*8        EPS(3),SIG(3),DCIS(2),CIST(2),DEPF(12),DEPM(8)
      REAL*8        DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2)
      REAL*8        H(3,3),D1I(2,2),D2I(2,4),VT(2),LAMBDA(4)
      REAL*8        BF(3,12),BM(3,8),SM(3),SF(3),HFT2(2,6),HLT2(4,6)
      REAL*8        QSI,ETA,CARAQ4(25),JACOB(5),T2EV(4),T2VE(4),T1VE(9)
      CHARACTER*2   VAL, CODRET
      CHARACTER*3   NUM
      CHARACTER*8   NOMRES
C     ------------------------------------------------------------------
C
      IF (OPTION(6:9).EQ.'ELGA') THEN
        CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
        NE  = NPG
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
        CALL ELREF5(' ','NOEU',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
        NE  = NNO
      END IF

      DEUX = 2.D0

C     ----- RAPPEL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
      CALL GQUAD4 ( XYZL, CARAQ4 )

C     ----- CARACTERISTIQUES DES MATERIAUX --------
      CALL DMATEL(FAMI,DF,DM,DMF,DC,DCI,NNO,PGL,MULTIC,ICOU,.FALSE.,
     +                                       T2EV,T2VE,T1VE,NPG)

C     -------- CALCUL DE D1I ET D2I ------------------------------------
      IF (MULTIC.EQ.0) THEN
        CALL JEVECH('PCACOQU','L',JCACO)
        EPAIS = ZR(JCACO)
        X3I = 0.D0
        EXCEN = ZR(JCACO+5-1)
        IF (INIV.LT.0) THEN
          X3I = X3I - EPAIS/DEUX + EXCEN
        ELSE IF (INIV.GT.0) THEN
          X3I = X3I + EPAIS/DEUX + EXCEN
        END IF
        DO 10 K = 1,9
          H(K,1) = DM(K,1)/EPAIS
   10   CONTINUE
        D1I(1,1) = 3.D0/ (DEUX*EPAIS) -
     +             X3I*X3I*6.D0/ (EPAIS*EPAIS*EPAIS)
        D1I(2,2) = D1I(1,1)
        D1I(1,2) = 0.D0
        D1I(2,1) = 0.D0
      ELSE
        CALL DXDMUL(ICOU,INIV,T1VE,T2VE,H,D1I,D2I,X3I)
      END IF
C     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
      DO 30 J = 1,4
        DO 20 I = 1,2
          DEPM(I+2* (J-1)) = DEPL(I+6* (J-1))
   20   CONTINUE
        DEPF(1+3* (J-1)) = DEPL(1+2+6* (J-1))
        DEPF(2+3* (J-1)) = DEPL(3+2+6* (J-1))
        DEPF(3+3* (J-1)) = -DEPL(2+2+6* (J-1))
   30 CONTINUE
C
      IF (OPTION(1:4).EQ.'EPSI') THEN
C         ---------------------
        DO 100 IE = 1,NE
          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4 ( XYZL, QSI, ETA, JACOB )
C           ----- CALCUL DES MATRICES BM ET BF AU POINT QSI ETA --------
          CALL DXQBM( QSI, ETA, JACOB(2), BM )
          CALL DKQBF( QSI, ETA, JACOB(2), CARAQ4, BF )
C           ------ SM = BM.DEPM ----------------------------------------
          DO 110 I = 1,3
            SM(I) = 0.D0
 110      CONTINUE
          DO 120 I = 1,3
            DO 122 J = 1,8
              SM(I) = SM(I) + BM(I,J)*DEPM(J)
 122        CONTINUE
 120      CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 130 I = 1,3
            SF(I) = 0.D0
 130      CONTINUE
          DO 140 I = 1,3
            DO 142 J = 1,12
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
 142        CONTINUE
 140      CONTINUE
          DO 150 I = 1,3
            EPS(I) = SM(I) + X3I*SF(I)
 150      CONTINUE
C           ------- CALCUL DU PRODUIT HF.T2 ----------------------------
          CALL DSXHFT ( DF, JACOB(2), HFT2 )
C           ------ VT = HFT2.TKT.DEPF ----------------------------------
          CALL DKQTXY ( QSI, ETA, HFT2, DEPF, CARAQ4(13),
     +                                            CARAQ4(9), VT )
C           ------ DCIS = DCI.VT --------------------------------------
          DCIS(1) = DCI(1,1)*VT(1) + DCI(1,2)*VT(2)
          DCIS(2) = DCI(2,1)*VT(1) + DCI(2,2)*VT(2)
          CDL(1+6* (IE-1)) = EPS(1)
          CDL(2+6* (IE-1)) = EPS(2)
          CDL(3+6* (IE-1)) = 0.D0
C           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
          CDL(4+6* (IE-1)) = EPS(3)/DEUX
          CDL(5+6* (IE-1)) = DCIS(1)/DEUX
          CDL(6+6* (IE-1)) = DCIS(2)/DEUX
  100   CONTINUE
C
      ELSE IF (OPTION(1:4).EQ.'SIGM') THEN
C              ---------------------
        DO 200 IE = 1,NE
          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4 ( XYZL, QSI, ETA, JACOB )
C           ----- CALCUL DES MATRICES BM ET BF AU POINT QSI ETA --------
          CALL DXQBM( QSI, ETA, JACOB(2), BM )
          CALL DKQBF( QSI, ETA, JACOB(2), CARAQ4, BF )
C           ------ SM = BM.DEPM ----------------------------------------
          DO 210 I = 1,3
            SM(I) = 0.D0
 210      CONTINUE
          DO 220 I = 1,3
            DO 222 J = 1,8
              SM(I) = SM(I) + BM(I,J)*DEPM(J)
 222        CONTINUE
 220      CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 230 I = 1,3
            SF(I) = 0.D0
 230      CONTINUE
          DO 240 I = 1,3
            DO 242 J = 1,12
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
 242        CONTINUE
 240      CONTINUE
          DO 250 I = 1,3
            EPS(I) = SM(I) + X3I*SF(I)
            SIG(I) = 0.D0
 250      CONTINUE
          DO 260 I = 1,3
            DO 262 J = 1,3
              SIG(I) = SIG(I) + H(I,J)*EPS(J)
 262        CONTINUE
 260      CONTINUE
C           ------- CALCUL DU PRODUIT HF.T2 ----------------------------
          CALL DSXHFT ( DF, JACOB(2), HFT2 )
C           ------ VT = HFT2.TKT.DEPF ----------------------------------
          CALL DKQTXY ( QSI, ETA, HFT2, DEPF, CARAQ4(13),
     +                                            CARAQ4(9), VT )
C           ------ CIST = D1I.VT ( + D2I.LAMBDA SI MULTICOUCHES ) ------
          CIST(1) = D1I(1,1)*VT(1) + D1I(1,2)*VT(2)
          CIST(2) = D1I(2,1)*VT(1) + D1I(2,2)*VT(2)
          IF (MULTIC.GT.0) THEN
C              ------- CALCUL DU PRODUIT HL.T2 ------------------------
            CALL DSXHLT ( DF, JACOB(2), HLT2 )
C              ------ LAMBDA = HLT2.TKT.DEPF ---------------------------
            CALL DKQLXY(QSI, ETA, HLT2, DEPF, CARAQ4(13),
     +                                            CARAQ4(9), LAMBDA )
            DO 270 J = 1,4
              CIST(1) = CIST(1) + D2I(1,J)*LAMBDA(J)
              CIST(2) = CIST(2) + D2I(2,J)*LAMBDA(J)
 270        CONTINUE
          END IF
          CDL(1+6* (IE-1)) = SIG(1)
          CDL(2+6* (IE-1)) = SIG(2)
          CDL(3+6* (IE-1)) = 0.D0
          CDL(4+6* (IE-1)) = SIG(3)
          CDL(5+6* (IE-1)) = CIST(1)
          CDL(6+6* (IE-1)) = CIST(2)
 200    CONTINUE
C
      ELSE IF (OPTION(1:4).EQ.'SIEF') THEN
C              ---------------------
        IF (MULTIC.EQ.0) THEN
          CALL JEVECH ( 'PCACOQU', 'L', JCACO )
          EPAIS = ZR(JCACO)
        ELSE
          CALL JEVECH ( 'PMATERC', 'L', JMATE )
        ENDIF
C
        DO 300 IE = 1,NE
          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4 ( XYZL, QSI, ETA, JACOB )
C           ----- CALCUL DES MATRICES BM ET BF AU POINT QSI ETA --------
          CALL DXQBM( QSI, ETA, JACOB(2), BM )
          CALL DKQBF( QSI, ETA, JACOB(2), CARAQ4, BF )
C           ------ SM = BM.DEPM ----------------------------------------
          DO 310 I = 1,3
            SM(I) = 0.D0
 310      CONTINUE
          DO 320 I = 1,3
            DO 322 J = 1,8
              SM(I) = SM(I) + BM(I,J)*DEPM(J)
 322        CONTINUE
 320      CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 330 I = 1,3
            SF(I) = 0.D0
 330      CONTINUE
          DO 340 I = 1,3
            DO 342 J = 1,12
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
 342        CONTINUE
 340      CONTINUE

          DO 350 IC = 1 , ICOU

            IF (MULTIC.NE.0) THEN
              CALL CODENT ( IC, 'G', NUM )
              CALL CODENT (  1, 'G', VAL )
              NOMRES = 'C'//NUM//'_V'//VAL
              CALL RCVALA(ZI(JMATE),' ', 'ELAS_COQMU', 0, ' ', R8BID,
     +                      1, NOMRES, EPAIS, CODRET, 'FM' )
            END IF
            HIC  =  EPAIS/ICOU
            ZMIN = -EPAIS/DEUX

            DO 360, IG = 1 , INIV

              ICPG = 6*INIV*ICOU*(IE-1) + 6*INIV*(IC-1) + 6*(IG-1)

C             -- COTE DES POINTS D'INTEGRATION
C             --------------------------------
              IF (IG.EQ.1) THEN
                ZIC = ZMIN + (IC-1)*HIC
              ELSE IF (IG.EQ.2) THEN
                ZIC = ZMIN + HIC/DEUX + (IC-1)*HIC
              ELSE
                ZIC = ZMIN + HIC + (IC-1)*HIC
              END IF

              DO 362 I = 1,3
                EPS(I) = SM(I) + ZIC*SF(I)
                SIG(I) = 0.D0
 362          CONTINUE
              DO 364 I = 1,3
                DO 366 J = 1,3
                  SIG(I) = SIG(I) + H(I,J)*EPS(J)
 366            CONTINUE
 364          CONTINUE
C             ------- CALCUL DU PRODUIT HF.T2 -------------------------
              CALL DSXHFT ( DF, JACOB(2), HFT2 )
C             ------ VT = HFT2.TKT.DEPF -------------------------------
              CALL DKQTXY ( QSI, ETA, HFT2, DEPF, CARAQ4(13),
     +                                                CARAQ4(9), VT )
C             ------ CIST = D1I.VT ( + D2I.LAMBDA SI MULTICOUCHES ) ---
              CIST(1) = D1I(1,1)*VT(1) + D1I(1,2)*VT(2)
              CIST(2) = D1I(2,1)*VT(1) + D1I(2,2)*VT(2)
              IF (MULTIC.GT.0) THEN
C               ------- CALCUL DU PRODUIT HL.T2 -----------------------
                CALL DSXHLT ( DF, JACOB(2), HLT2 )
C               ------ LAMBDA = HLT2.TKT.DEPF -------------------------
                CALL DKQLXY(QSI, ETA, HLT2, DEPF, CARAQ4(13),
     +                                               CARAQ4(9), LAMBDA )
                DO 368 J = 1,4
                  CIST(1) = CIST(1) + D2I(1,J)*LAMBDA(J)
                  CIST(2) = CIST(2) + D2I(2,J)*LAMBDA(J)
 368            CONTINUE
              END IF
              CDL(ICPG+1) = SIG(1)
              CDL(ICPG+2) = SIG(2)
              CDL(ICPG+3) = 0.D0
              CDL(ICPG+4) = SIG(3)
              CDL(ICPG+5) = CIST(1)
              CDL(ICPG+6) = CIST(2)
 360        CONTINUE
 350      CONTINUE
 300    CONTINUE
      END IF
C
      END
