      SUBROUTINE DSQCOD(XYZL,OPTION,PGL,ICOU,INIV,DEPL,CDL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/12/2000   AUTEUR CIBHHGB G.BERTRAND 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XYZL(3,*),PGL(3,*)
      REAL*8 DEPL(*),CDL(*)
      CHARACTER*16 OPTION
      INTEGER ICOU
      INTEGER INIV
C     ------------------------------------------------------------------
C     CONTRAINTES ET DEFORMATIONS DE L'ELEMENT DE PLAQUE DSQ
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
C     IN  OPTION : NOM DE L'OPTION DE CALCUL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  ICOU   : NUMERO DE LA COUCHE
C     IN  INIV   : NIVEAU DANS LA COUCHE (-1:INF , 0:MOY , 1:SUP)
C     IN  DEPL   : DEPLACEMENTS
C     OUT CDL    : CONTRAINTES OU DEFORMATIONS AUX NOEUDS DANS LE REPERE
C                  INTRINSEQUE A L'ELEMENT
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      CHARACTER*8 TYPELE
      CHARACTER*24 DESR
      REAL*8 DEPF(12),DEPM(8),META,MQSI
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 H(3,3),D1I(2,2),D2I(2,4)
      REAL*8 HFT2(2,6),HLT2(4,6),AN(4,12),HMFT2(2,6)
      REAL*8 BFB(3,12),BFA(3,4),BFN(3,12),BF(3,12)
      REAL*8 BCB(2,12),BCA(2,4),BCN(2,12),BC(2,12),BCM(2,8)
      REAL*8 BM(3,8)
      REAL*8 TA(6,4),TB(6,12)
      REAL*8 BLB(4,12),BLA(4,4),BLN(4,12)
      REAL*8 SF(3),SM(3),VT(2),LAMBDA(4)
      REAL*8 EPS(3),SIG(3),DCIS(2),CIST(2),X3I,EPAIS
      REAL*8 C(4),S(4)
      INTEGER MULTIC
      LOGICAL ELASCO
C     ------------------ PARAMETRAGE QUADRANGLE ------------------------
      INTEGER NPG,NC,NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN,LAIRE,
     +        LAIRN,LT1VE,LT2VE
      PARAMETER (NPG=4)
      PARAMETER (NNO=4)
      PARAMETER (NC=4)
      PARAMETER (LJACO=2)
      PARAMETER (LTOR=LJACO+4)
      PARAMETER (LQSI=LTOR+1)
      PARAMETER (LETA=LQSI+NPG+NNO+2*NC)
      PARAMETER (LWGT=LETA+NPG+NNO+2*NC)
      PARAMETER (LXYC=LWGT+NPG)
      PARAMETER (LCOTE=LXYC+2*NC)
      PARAMETER (LCOS=LCOTE+NC)
      PARAMETER (LSIN=LCOS+NC)
      PARAMETER (LAIRE=LSIN+NC)
      PARAMETER (LAIRN=LAIRE+1)
      PARAMETER (LT1VE=LAIRN+4)
      PARAMETER (LT2VE=LT1VE+9)
C     ------------------------------------------------------------------
      CALL JEMARQ()
      TYPELE = 'MEDSQU4 '
      DESR = '&INEL.'//TYPELE//'.DESR'
      CALL JEVETE(DESR,' ',LZR)
      IF (OPTION(6:9).EQ.'ELGA') THEN
        NE = NPG
        INE = 0
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
        NE = NNO
        INE = NPG
      END IF
C     ----- RAPPEL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
      CALL GQUAD4(XYZL,ZR(LZR))
C     ----- CARACTERISTIQUES DES MATERIAUX --------
      CALL DXMATE(DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,ZR(LZR),MULTIC,
     +           .FALSE.,ELASCO)
C     -------- CALCUL DE D1I ET D2I ------------------------------------
      IF (MULTIC.EQ.0) THEN
        CALL JEVECH('PCACOQU','L',JCACO)
        EPAIS = ZR(JCACO)
        X3I = 0.D0
        IF (INIV.LT.0) THEN
          X3I = X3I - EPAIS/2.D0
        ELSE IF (INIV.GT.0) THEN
          X3I = X3I + EPAIS/2.D0
        END IF
        DO 10 K = 1,9
          H(K,1) = DM(K,1)/EPAIS
   10   CONTINUE
        D1I(1,1) = 3.D0/ (2.D0*EPAIS) -
     +             X3I*X3I*6.D0/ (EPAIS*EPAIS*EPAIS)
        D1I(2,2) = D1I(1,1)
        D1I(1,2) = 0.D0
        D1I(2,1) = 0.D0
      ELSE
        CALL DXDMUL(ICOU,INIV,ZR(LZR-1+LT1VE),ZR(LZR-1+LT2VE),H,D1I,D2I,
     +              X3I)
      END IF
C     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
      DO 30 J = 1,NNO
        DO 20 I = 1,2
          DEPM(I+2* (J-1)) = DEPL(I+6* (J-1))
   20   CONTINUE
        DEPF(1+3* (J-1)) = DEPL(1+2+6* (J-1))
        DEPF(2+3* (J-1)) = DEPL(3+2+6* (J-1))
        DEPF(3+3* (J-1)) = -DEPL(2+2+6* (J-1))
   30 CONTINUE
C     ---- CALCUL DE LA MATRICE AN -------------------------------------
      CALL DSQDIS(XYZL,DF,DCI,AN)
      IF (OPTION(1:4).EQ.'EPSI') THEN
        DO 210 IE = 1,NE
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4(IE+INE,XYZL,ZR(LZR))
C
C ---     CALCUL DU PRODUIT HMF.T2 :
C         ------------------------
          CALL DSXHFT(DF,ZR(LZR),HFT2)
C
C ---     CALCUL DU PRODUIT HMF.T2 :
C         ------------------------
          CALL DXHMFT(DMF,ZR(LZR),HMFT2)
C
C ---     CALCUL DES MATRICES BCB, BCA ET BCM :
C       -------------------------------------
          CALL DSQCIS(IE+INE,ZR(LZR),HMFT2,HFT2,BCM,BCB,BCA)
C
C           ------ BC = BCB + BCA.AN -----------------------------------
          DO 40 K = 1,24
            BCN(K,1) = 0.D0
   40     CONTINUE
          DO 70 I = 1,2
            DO 60 J = 1,12
              DO 50 K = 1,4
                BCN(I,J) = BCN(I,J) + BCA(I,K)*AN(K,J)
   50         CONTINUE
              BC(I,J) = BCB(I,J) + BCN(I,J)
   60       CONTINUE
   70     CONTINUE
C           ------ VT = BC.DEPF ---------------------------------------
          VT(1) = 0.D0
          VT(2) = 0.D0
          DO 90 I = 1,2
            DO 80 J = 1,12
              VT(I) = VT(I) + BC(I,J)*DEPF(J)
   80       CONTINUE
   90     CONTINUE
C           ----- CALCUL DE LA MATRICE BM AU POINT QSI ETA -------------
          CALL DXQBM(IE+INE,ZR(LZR),BM)
C           ------ SM = BM.DEPM ----------------------------------------
          DO 100 I = 1,3
            SM(I) = 0.D0
  100     CONTINUE
          DO 120 I = 1,3
            DO 110 J = 1,8
              SM(I) = SM(I) + BM(I,J)*DEPM(J)
  110       CONTINUE
  120     CONTINUE
C
C ---     COORDONNEES DU POINT D'INTEGRATION COURANT :
C         ------------------------------------------
          QSI = ZR(LZR-1+LQSI+IE+INE-1)
          ETA = ZR(LZR-1+LETA+IE+INE-1)

C           ----- CALCUL DE LA MATRICE BFB AU POINT QSI ETA -----------
          CALL DSQBFB(IE+INE,ZR(LZR),BFB)
C           ----- CALCUL DE LA MATRICE BFA AU POINT QSI ETA -----------
          CALL DSQBFA(QSI,ETA,ZR(LZR),BFA)
C           ------ BF = BFB + BFA.AN ----------------------------------
          DO 130 K = 1,36
            BFN(K,1) = 0.D0
  130     CONTINUE
          DO 160 I = 1,3
            DO 150 J = 1,12
              DO 140 K = 1,4
                BFN(I,J) = BFN(I,J) + BFA(I,K)*AN(K,J)
  140         CONTINUE
              BF(I,J) = BFB(I,J) + BFN(I,J)
  150       CONTINUE
  160     CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 170 I = 1,3
            SF(I) = 0.D0
  170     CONTINUE
          DO 190 I = 1,3
            DO 180 J = 1,12
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
  180       CONTINUE
  190     CONTINUE
          DO 200 I = 1,3
            EPS(I) = SM(I) + X3I*SF(I)
  200     CONTINUE
C           ------ DCIS = DCI.VT --------------------------------------
          DCIS(1) = DCI(1,1)*VT(1) + DCI(1,2)*VT(2)
          DCIS(2) = DCI(2,1)*VT(1) + DCI(2,2)*VT(2)
          CDL(1+6* (IE-1)) = EPS(1)
          CDL(2+6* (IE-1)) = EPS(2)
          CDL(3+6* (IE-1)) = 0.D0
C           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
          CDL(4+6* (IE-1)) = EPS(3)/2.D0
          CDL(5+6* (IE-1)) = DCIS(1)/2.D0
          CDL(6+6* (IE-1)) = DCIS(2)/2.D0
  210   CONTINUE
      ELSE IF (OPTION(1:4).EQ.'SIGM') THEN
        IF (MULTIC.GT.0) THEN
C           ---- CALCUL DE LA MATRICE TB -------------------------------
          DO 220 K = 1,72
            TB(K,1) = 0.D0
  220     CONTINUE
          TB(3,2) = 0.25D0
          TB(3,5) = -0.25D0
          TB(3,8) = 0.25D0
          TB(3,11) = -0.25D0
          TB(6,3) = 0.25D0
          TB(6,6) = -0.25D0
          TB(6,9) = 0.25D0
          TB(6,12) = -0.25D0
          C(1) = ZR(LZR-1+LCOS)
          C(2) = ZR(LZR-1+LCOS+1)
          C(3) = ZR(LZR-1+LCOS+2)
          C(4) = ZR(LZR-1+LCOS+3)
          S(1) = ZR(LZR-1+LSIN)
          S(2) = ZR(LZR-1+LSIN+1)
          S(3) = ZR(LZR-1+LSIN+2)
          S(4) = ZR(LZR-1+LSIN+3)
        END IF
        DO 570 IE = 1,NE
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4(IE+INE,XYZL,ZR(LZR))
C
C ---     CALCUL DU PRODUIT HF.T2 :
C         ------------------------
          CALL DSXHFT(DF,ZR(LZR),HFT2)
C
C ---     CALCUL DU PRODUIT HMF.T2 :
C         ------------------------
          CALL DXHMFT(DMF,ZR(LZR),HMFT2)
C
C ---     CALCUL DES MATRICES BCB, BCA ET BCM :
C         -----------------------------------
          CALL DSQCIS(IE+INE,ZR(LZR),HMFT2,HFT2,BCM,BCB,BCA)
C
C           ------ BC = BCB + BCA.AN -----------------------------------
C
          DO 230 K = 1,24
            BCN(K,1) = 0.D0
  230     CONTINUE
          DO 260 I = 1,2
            DO 250 J = 1,12
              DO 240 K = 1,4
                BCN(I,J) = BCN(I,J) + BCA(I,K)*AN(K,J)
  240         CONTINUE
              BC(I,J) = BCB(I,J) + BCN(I,J)
  250       CONTINUE
  260     CONTINUE
C           ------ VT = BC.DEPF ---------------------------------------
          VT(1) = 0.D0
          VT(2) = 0.D0
          DO 280 I = 1,2
            DO 270 J = 1,12
              VT(I) = VT(I) + BC(I,J)*DEPF(J)
  270       CONTINUE
  280     CONTINUE
C           ------ CIST = D1I.VT ( + D2I.LAMBDA SI MULTICOUCHES ) ------
          CIST(1) = D1I(1,1)*VT(1) + D1I(1,2)*VT(2)
          CIST(2) = D1I(2,1)*VT(1) + D1I(2,2)*VT(2)
          IF (MULTIC.GT.0) THEN
C              ------- CALCUL DU PRODUIT HL.T2 ------------------------
            CALL DSXHLT(DF,ZR(LZR),HLT2)
            QSI = ZR(LZR-1+LQSI+IE+INE-1)
            ETA = ZR(LZR-1+LETA+IE+INE-1)
            PETA = 1.D0 + ETA
            META = 1.D0 - ETA
            PQSI = 1.D0 + QSI
            MQSI = 1.D0 - QSI
            DO 290 K = 1,24
              TA(K,1) = 0.D0
  290       CONTINUE
            TA(1,1) = -META*C(1)
            TA(1,3) = -PETA*C(3)
            TA(2,2) = -PQSI*C(2)
            TA(2,4) = -MQSI*C(4)
            TA(3,1) = QSI*C(1)
            TA(3,2) = -ETA*C(2)
            TA(3,3) = -QSI*C(3)
            TA(3,4) = ETA*C(4)
            TA(4,1) = -META*S(1)
            TA(4,3) = -PETA*S(3)
            TA(5,2) = -PQSI*S(2)
            TA(5,4) = -MQSI*S(4)
            TA(6,1) = QSI*S(1)
            TA(6,2) = -ETA*S(2)
            TA(6,3) = -QSI*S(3)
            TA(6,4) = ETA*S(4)
C              -------------- BLA = HLT2.TA ----------------------------
            DO 300 K = 1,16
              BLA(K,1) = 0.D0
  300       CONTINUE
            DO 330 I = 1,4
              DO 320 J = 1,4
                DO 310 K = 1,6
                  BLA(I,J) = BLA(I,J) + HLT2(I,K)*TA(K,J)
  310           CONTINUE
  320         CONTINUE
  330       CONTINUE
C              -------------- BLB = HLT2.TB ----------------------------
            DO 340 K = 1,48
              BLB(K,1) = 0.D0
  340       CONTINUE
            DO 370 I = 1,4
              DO 360 J = 1,12
                DO 350 K = 1,6
                  BLB(I,J) = BLB(I,J) + HLT2(I,K)*TB(K,J)
  350           CONTINUE
  360         CONTINUE
  370       CONTINUE
C              -------- LAMBDA = (BLB + BLA.AN).DEPF ------------------
            DO 380 I = 1,4
              LAMBDA(I) = 0.D0
  380       CONTINUE
            DO 390 K = 1,48
              BLN(K,1) = 0.D0
  390       CONTINUE
            DO 420 I = 1,4
              DO 410 J = 1,12
                DO 400 K = 1,4
                  BLN(I,J) = BLN(I,J) + BLA(I,K)*AN(K,J)
  400           CONTINUE
                LAMBDA(I) = LAMBDA(I) + (BLB(I,J)+BLN(I,J))*DEPF(J)
  410         CONTINUE
  420       CONTINUE
            DO 430 J = 1,4
              CIST(1) = CIST(1) + D2I(1,J)*LAMBDA(J)
              CIST(2) = CIST(2) + D2I(2,J)*LAMBDA(J)
  430       CONTINUE
          END IF
C           ----- CALCUL DE LA MATRICE BM AU POINT QSI ETA -------------
          CALL DXQBM(IE+INE,ZR(LZR),BM)
C           ------ SM = BM.DEPM ----------------------------------------
          DO 440 I = 1,3
            SM(I) = 0.D0
  440     CONTINUE
          DO 460 I = 1,3
            DO 450 J = 1,8
              SM(I) = SM(I) + BM(I,J)*DEPM(J)
  450       CONTINUE
  460     CONTINUE
C
C ---     COORDONNEES DU POINT D'INTEGRATION COURANT :
C         ------------------------------------------
          QSI = ZR(LZR-1+LQSI+IE+INE-1)
          ETA = ZR(LZR-1+LETA+IE+INE-1)

C           ----- CALCUL DE LA MATRICE BFB AU POINT QSI ETA -----------
          CALL DSQBFB(IE+INE,ZR(LZR),BFB)
C           ----- CALCUL DE LA MATRICE BFA AU POINT QSI ETA -----------
          CALL DSQBFA(QSI,ETA,ZR(LZR),BFA)
C           ------ BF = BFB + BFA.AN ----------------------------------
          DO 470 K = 1,36
            BFN(K,1) = 0.D0
  470     CONTINUE
          DO 500 I = 1,3
            DO 490 J = 1,12
              DO 480 K = 1,4
                BFN(I,J) = BFN(I,J) + BFA(I,K)*AN(K,J)
  480         CONTINUE
              BF(I,J) = BFB(I,J) + BFN(I,J)
  490       CONTINUE
  500     CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 510 I = 1,3
            SF(I) = 0.D0
  510     CONTINUE
          DO 530 I = 1,3
            DO 520 J = 1,12
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
  520       CONTINUE
  530     CONTINUE
          DO 540 I = 1,3
            EPS(I) = SM(I) + X3I*SF(I)
            SIG(I) = 0.D0
  540     CONTINUE
          DO 560 I = 1,3
            DO 550 J = 1,3
              SIG(I) = SIG(I) + H(I,J)*EPS(J)
  550       CONTINUE
  560     CONTINUE
          CDL(1+6* (IE-1)) = SIG(1)
          CDL(2+6* (IE-1)) = SIG(2)
          CDL(3+6* (IE-1)) = 0.D0
          CDL(4+6* (IE-1)) = SIG(3)
          CDL(5+6* (IE-1)) = CIST(1)
          CDL(6+6* (IE-1)) = CIST(2)
  570   CONTINUE
      END IF
      CALL JEDEMA()
      END
