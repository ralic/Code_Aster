      SUBROUTINE DKQCOD ( NOMTE, XYZL, OPTION, PGL, ICOU, INIV, DEPL,
     +                    CDL )
      IMPLICIT  NONE
      INTEGER       ICOU, INIV
      REAL*8        XYZL(3,*), PGL(3,*), DEPL(*), CDL(*)
      CHARACTER*16  NOMTE, OPTION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2004   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER MULTIC,LZR,NE,INE,JCACO,K,J,I,IE
      REAL*8 DEPF(12),DEPM(8)
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 H(3,3),D1I(2,2),D2I(2,4)
      REAL*8 BF(3,12),BM(3,8)
      REAL*8 SM(3),SF(3)
      REAL*8 HFT2(2,6),HLT2(4,6)
      REAL*8 VT(2),LAMBDA(4)
      REAL*8 EPS(3),SIG(3),DCIS(2),CIST(2),X3I,EPAIS
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

      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ',LZR)
      IF (OPTION(6:9).EQ.'ELGA') THEN
        NE  = NPG
        INE = 0
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
        NE  = NNO
        INE = NPG
      END IF
C     ----- RAPPEL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
      CALL GQUAD4(XYZL,ZR(LZR))
C     ----- CARACTERISTIQUES DES MATERIAUX --------
      CALL DXMATE(DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,ZR(LZR),MULTIC,
     +            .FALSE.,ELASCO)
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
      DO 30 J = 1,4
        DO 20 I = 1,2
          DEPM(I+2* (J-1)) = DEPL(I+6* (J-1))
   20   CONTINUE
        DEPF(1+3* (J-1)) = DEPL(1+2+6* (J-1))
        DEPF(2+3* (J-1)) = DEPL(3+2+6* (J-1))
        DEPF(3+3* (J-1)) = -DEPL(2+2+6* (J-1))
   30 CONTINUE
      IF (OPTION(1:4).EQ.'EPSI') THEN
        DO 110 IE = 1,NE
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4(IE+INE,XYZL,ZR(LZR))
C           ----- CALCUL DES MATRICES BM ET BF AU POINT QSI ETA --------
          CALL DXQBM(IE+INE,ZR(LZR),BM)
          CALL DKQBF(IE+INE,ZR(LZR),BF)
C           ------ SM = BM.DEPM ----------------------------------------
          DO 40 I = 1,3
            SM(I) = 0.D0
   40     CONTINUE
          DO 60 I = 1,3
            DO 50 J = 1,8
              SM(I) = SM(I) + BM(I,J)*DEPM(J)
   50       CONTINUE
   60     CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 70 I = 1,3
            SF(I) = 0.D0
   70     CONTINUE
          DO 90 I = 1,3
            DO 80 J = 1,12
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
   80       CONTINUE
   90     CONTINUE
          DO 100 I = 1,3
            EPS(I) = SM(I) + X3I*SF(I)
  100     CONTINUE
C           ------- CALCUL DU PRODUIT HF.T2 ----------------------------
          CALL DSXHFT(DF,ZR(LZR),HFT2)
C           ------ VT = HFT2.TKT.DEPF ----------------------------------
          CALL DKQTXY(IE+INE,HFT2,DEPF,ZR(LZR),VT)
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
  110   CONTINUE
      ELSE IF (OPTION(1:4).EQ.'SIGM') THEN
        DO 220 IE = 1,NE
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4(IE+INE,XYZL,ZR(LZR))
C           ----- CALCUL DES MATRICES BM ET BF AU POINT QSI ETA --------
          CALL DXQBM(IE+INE,ZR(LZR),BM)
          CALL DKQBF(IE+INE,ZR(LZR),BF)
C           ------ SM = BM.DEPM ----------------------------------------
          DO 120 I = 1,3
            SM(I) = 0.D0
  120     CONTINUE
          DO 140 I = 1,3
            DO 130 J = 1,8
              SM(I) = SM(I) + BM(I,J)*DEPM(J)
  130       CONTINUE
  140     CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 150 I = 1,3
            SF(I) = 0.D0
  150     CONTINUE
          DO 170 I = 1,3
            DO 160 J = 1,12
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
  160       CONTINUE
  170     CONTINUE
          DO 180 I = 1,3
            EPS(I) = SM(I) + X3I*SF(I)
            SIG(I) = 0.D0
  180     CONTINUE
          DO 200 I = 1,3
            DO 190 J = 1,3
              SIG(I) = SIG(I) + H(I,J)*EPS(J)
  190       CONTINUE
  200     CONTINUE
C           ------- CALCUL DU PRODUIT HF.T2 ----------------------------
          CALL DSXHFT(DF,ZR(LZR),HFT2)
C           ------ VT = HFT2.TKT.DEPF ----------------------------------
          CALL DKQTXY(IE+INE,HFT2,DEPF,ZR(LZR),VT)
C           ------ CIST = D1I.VT ( + D2I.LAMBDA SI MULTICOUCHES ) ------
          CIST(1) = D1I(1,1)*VT(1) + D1I(1,2)*VT(2)
          CIST(2) = D1I(2,1)*VT(1) + D1I(2,2)*VT(2)
          IF (MULTIC.GT.0) THEN
C              ------- CALCUL DU PRODUIT HL.T2 ------------------------
            CALL DSXHLT(DF,ZR(LZR),HLT2)
C              ------ LAMBDA = HLT2.TKT.DEPF ---------------------------
            CALL DKQLXY(IE+INE,HLT2,DEPF,ZR(LZR),LAMBDA)
            DO 210 J = 1,4
              CIST(1) = CIST(1) + D2I(1,J)*LAMBDA(J)
              CIST(2) = CIST(2) + D2I(2,J)*LAMBDA(J)
  210       CONTINUE
          END IF
          CDL(1+6* (IE-1)) = SIG(1)
          CDL(2+6* (IE-1)) = SIG(2)
          CDL(3+6* (IE-1)) = 0.D0
          CDL(4+6* (IE-1)) = SIG(3)
          CDL(5+6* (IE-1)) = CIST(1)
          CDL(6+6* (IE-1)) = CIST(2)
  220   CONTINUE
      END IF
      CALL JEDEMA()
      END
