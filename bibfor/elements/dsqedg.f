      SUBROUTINE DSQEDG ( XYZL, OPTION, PGL, DEPL, EDGL )
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'
      REAL*8        XYZL(3,*), PGL(3,*), DEPL(*), EDGL(*)
      CHARACTER*16  OPTION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     EFFORTS ET DEFORMATIONS GENERALISES DE L'ELEMENT DE PLAQUE DSQ
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  OPTION : NOM DE L'OPTION DE CALCUL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  DEPL   : DEPLACEMENTS
C     OUT EDGL   : EFFORTS OU DEFORMATIONS GENERALISES AUX NOEUDS DANS
C                  LE REPERE INTRINSEQUE A L'ELEMENT
      INTEGER  NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER  MULTIC,NE,K,J,I,IE,JCARA
      REAL*8   DEPF(12),DEPM(8)
      REAL*8   DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2)
      REAL*8   HFT2(2,6),AN(4,12),HMFT2(2,6),DFC(3,2)
      REAL*8   BFB(3,12),BFA(3,4),BFN(3,12),BF(3,12)
      REAL*8   BCB(2,12),BCA(2,4),BCN(2,12),BC(2,12),BCM(2,8)
      REAL*8   BM(3,8),AM(4,8),BDF(3),BDM(3),DCIS(2),BTM(2,8)
      REAL*8   BDFM(3),BDM1(3),R8MIEM
      REAL*8   VF(3),VM(3),VT(2),EXCENT,BFAM(3,8),BCAM(2,8)
      REAL*8   VFM(3),VMF(3),VMC(3),VFC(3),VCM(2),VCF(2)
      REAL*8   QSI,ETA,JACOB(5),CARAQ4(25),T2EV(4),T2VE(4),T1VE(9)
      LOGICAL  COUPMF,EXCEN
      CHARACTER*4 FAMI
C     ------------------------------------------------------------------
C
      IF (OPTION(6:9).EQ.'ELGA') THEN
        CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
        NE  = NPG
        FAMI='RIGI'
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
        CALL ELREF5(' ','NOEU',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
        NE  = NNO
        FAMI='NOEU'
      END IF
C
      CALL JEVECH('PCACOQU','L',JCARA)
      EXCENT = ZR(JCARA+4)
      EXCEN = .FALSE.
      IF(EXCENT.GT.R8MIEM()) THEN
         EXCEN=.TRUE.
      ENDIF
C     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------

C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
      CALL GQUAD4 ( XYZL, CARAQ4 )
C     ----- CARACTERISTIQUES DES MATERIAUX --------
      CALL DXMATE(FAMI,DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     +                                         COUPMF,T2EV,T2VE,T1VE)
C     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
      DO 20 J = 1,NNO
        DO 10 I = 1,2
          DEPM(I+2*(J-1)) = DEPL(I+6*(J-1))
   10   CONTINUE
        DEPF(1+3* (J-1)) =  DEPL(1+2+6*(J-1))
        DEPF(2+3* (J-1)) =  DEPL(3+2+6*(J-1))
        DEPF(3+3* (J-1)) = -DEPL(2+2+6*(J-1))
   20 CONTINUE
C     ---- CALCUL DE LA MATRICE AN -------------------------------------
      CALL DSQDI2 ( XYZL, DF, DCI, DMF, DFC, DMC, AN, AM )
C
      IF (OPTION(1:4).EQ.'DEGE') THEN
C         ---------------------
        DO 180 IE = 1,NE
C
C ---     COORDONNEES DU POINT D'INTEGRATION COURANT :
          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)
C
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4 ( XYZL, QSI, ETA, JACOB )
C
C           ----- CALCUL DE LA MATRICE BM ------------------------------
          CALL DXQBM ( QSI, ETA, JACOB(2), BM )
C
C ---     CALCUL DU PRODUIT HF.T2 :
          CALL DSXHFT ( DF, JACOB(2), HFT2 )
C
C ---     CALCUL DU PRODUIT HMF.T2 :
          CALL DXHMFT ( DMF, JACOB(2), HMFT2 )
C
C ---     CALCUL DES MATRICES BCB, BCA ET BCM :
          CALL DSQCIS ( QSI, ETA, CARAQ4, HMFT2, HFT2, BCM, BCB, BCA )
C
C           ------ BC = BCB + BCA.AN -----------------------------------
          DO 30 K = 1,24
            BCN(K,1) = 0.D0
   30     CONTINUE
          DO 60 I = 1,2
            DO 50 J = 1,12
              DO 40 K = 1,4
                BCN(I,J) = BCN(I,J) + BCA(I,K)*AN(K,J)
   40         CONTINUE
              BC(I,J) = BCB(I,J) + BCN(I,J)
   50       CONTINUE
   60     CONTINUE
C           ------ VT = BC.DEPF ---------------------------------------
          VT(1) = 0.D0
          VT(2) = 0.D0
          DO 80 I = 1,2
            DO 70 J = 1,12
              VT(I) = VT(I) + BC(I,J)*DEPF(J)
   70       CONTINUE
   80     CONTINUE
C           ----- CALCUL DE LA MATRICE BFB AU POINT QSI ETA -----------
          CALL DSQBFB ( QSI, ETA, JACOB(2), BFB )
C           ----- CALCUL DE LA MATRICE BFA AU POINT QSI ETA -----------
          CALL DSQBFA ( QSI, ETA , JACOB(2) , CARAQ4 , BFA )
C           ------ BF = BFB + BFA.AN ----------------------------------
          DO 90 K = 1,36
            BFN(K,1) = 0.D0
   90     CONTINUE
          DO 120 I = 1,3
            DO 110 J = 1,12
              DO 100 K = 1,4
                BFN(I,J) = BFN(I,J) + BFA(I,K)*AN(K,J)
  100         CONTINUE
              BF(I,J) = BFB(I,J) + BFN(I,J)
  110       CONTINUE
  120     CONTINUE
          DO 130 K = 1,3
            BDF(K) = 0.D0
            BDM(K) = 0.D0
  130     CONTINUE
          DO 160 I = 1,3
            DO 140 J = 1,12
              BDF(I) = BDF(I) + BF(I,J)*DEPF(J)
  140       CONTINUE
            DO 150 J = 1,8
              BDM(I) = BDM(I) + BM(I,J)*DEPM(J)
  150       CONTINUE
  160     CONTINUE
C           ------ DCIS = DCI.VT --------------------------------------
          DCIS(1) = DCI(1,1)*VT(1) + DCI(1,2)*VT(2)
          DCIS(2) = DCI(2,1)*VT(1) + DCI(2,2)*VT(2)
          DO 170 I = 1,3
            EDGL(I+8* (IE-1)) = BDM(I)
            EDGL(I+3+8* (IE-1)) = BDF(I)
  170     CONTINUE
C           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
          EDGL(3+8* (IE-1)) = EDGL(3+8* (IE-1))/2.D0
          EDGL(6+8* (IE-1)) = EDGL(6+8* (IE-1))/2.D0
          EDGL(7+8* (IE-1)) = DCIS(1)/2.D0
          EDGL(8+8* (IE-1)) = DCIS(2)/2.D0
  180   CONTINUE
C
      ELSE
        DO 360 IE = 1,NE
C
C ---     COORDONNEES DU POINT D'INTEGRATION COURANT :
          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)
C
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4 ( XYZL, QSI, ETA, JACOB )
C
C           ----- CALCUL DE LA MATRICE BM ------------------------------
          CALL DXQBM ( QSI, ETA, JACOB(2), BM )
C
C ---     CALCUL DU PRODUIT HF.T2 :
          CALL DSXHFT ( DF, JACOB(2), HFT2 )
C
C ---     CALCUL DU PRODUIT HMF.T2 :
          CALL DXHMFT ( DMF, JACOB(2), HMFT2 )
C
C ---     CALCUL DES MATRICES BCB, BCA ET BCM :
          CALL DSQCIS ( QSI, ETA, CARAQ4, HMFT2, HFT2, BCM, BCB, BCA )
C
C           ------ BC = BCB + BCA.AN -----------------------------------
          DO 190 K = 1,24
            BCN(K,1) = 0.D0
            BFAM(K,1) = 0.D0
  190     CONTINUE
          DO 191 K = 1,16
            BTM(K,1) = 0.D0
            BCAM(K,1) = 0.D0
  191     CONTINUE
          DO 220 I = 1,2
            DO 210 J = 1,12
              DO 200 K = 1,4
                BCN(I,J) = BCN(I,J) + BCA(I,K)*AN(K,J)
  200         CONTINUE
              BC(I,J) = BCB(I,J) + BCN(I,J)
  210       CONTINUE
  220     CONTINUE
C           ------ VT = BC.DEPF ---------------------------------------
          VT(1) = 0.D0
          VT(2) = 0.D0
          DO 240 I = 1,2
            DO 230 J = 1,12
              VT(I) = VT(I) + BC(I,J)*DEPF(J)
  230       CONTINUE
  240     CONTINUE
C
        IF(EXCEN) THEN
          DO 221 I = 1,2
            DO 211 J = 1,8
              DO 201 K = 1,4
                BCAM(I,J) = BCAM(I,J) + BCA(I,K)*AM(K,J)
  201         CONTINUE
              BTM(I,J) = BCM(I,J) + BCAM(I,J)
  211       CONTINUE
  221     CONTINUE
          DO 241 I = 1,2
            DO 231 J = 1,8
              VT(I) = VT(I) + BTM(I,J)*DEPM(J)
  231       CONTINUE
  241     CONTINUE
        ENDIF
C
C           ----- CALCUL DE LA MATRICE BFB AU POINT QSI ETA -----------
          CALL DSQBFB ( QSI, ETA, JACOB(2), BFB )
C           ----- CALCUL DE LA MATRICE BFA AU POINT QSI ETA -----------
          CALL DSQBFA ( QSI, ETA , JACOB(2) , CARAQ4 , BFA )
C           ------ BF = BFB + BFA.AN ----------------------------------
          DO 250 K = 1,36
            BFN(K,1) = 0.D0
  250     CONTINUE
          DO 280 I = 1,3
            DO 270 J = 1,12
              DO 260 K = 1,4
                BFN(I,J) = BFN(I,J) + BFA(I,K)*AN(K,J)
  260         CONTINUE
              BF(I,J) = BFB(I,J) + BFN(I,J)
  270       CONTINUE
  280     CONTINUE
          DO 281 I = 1,3
            DO 271 J = 1,8
              DO 261 K = 1,4
                BFAM(I,J) = BFAM(I,J) + BFA(I,K)*AM(K,J)
  261         CONTINUE
  271       CONTINUE
  281     CONTINUE
          DO 290 K = 1,3
            BDF(K) = 0.D0
            BDFM(K) = 0.D0
            BDM(K) = 0.D0
            BDM1(K) = 0.D0
            VF(K) = 0.D0
            VM(K) = 0.D0
            VFM(K) = 0.D0
            VMF(K) = 0.D0
            VMC(K) = 0.0D0
            VFC(K) = 0.0D0
  290     CONTINUE
          VCM(1) = 0.0D0
          VCM(2) = 0.0D0
          VCF(1) = 0.0D0
          VCF(2) = 0.0D0
C           ------ VF = DF.BF.DEPF , VFM = DMF.BM.DEPM ----------------
C           ------ VM = DM.BM.DEPM , VMF = DMF.BF.DEPF ----------------
          DO 320 I = 1,3
            DO 300 J = 1,12
              BDF(I) = BDF(I) + BF(I,J)*DEPF(J)
  300       CONTINUE
            DO 310 J = 1,8
              BDFM(I) = BDFM(I) + BFAM(I,J)*DEPM(J)
              BDM(I) = BDM(I) + BM(I,J)*DEPM(J)
              BDM1(I) = BDM1(I) + (BM(I,J)+EXCENT*BFAM(I,J))*DEPM(J)
  310       CONTINUE
  320     CONTINUE
          DO 340 I = 1,3
            DO 330 J = 1,3
              VF(I) = VF(I) + DF(I,J)*BDF(J)
              VFM(I) = VFM(I) + DMF(I,J)*BDM(J)
              VM(I) = VM(I) + DM(I,J)*BDM1(J)
              VMF(I) = VMF(I) + DMF(I,J)*BDF(J)
  330       CONTINUE
  340     CONTINUE
        IF(EXCEN) THEN
          DO 341 I = 1,3
            DO 331 J = 1,3
              VFM(I) = VFM(I) + DF(I,J)*BDFM(J)
  331       CONTINUE
  341     CONTINUE
        ENDIF
C
          DCIS(1) = DCI(1,1)*VT(1) + DCI(1,2)*VT(2)
          DCIS(2) = DCI(2,1)*VT(1) + DCI(2,2)*VT(2)
C
          VMC(1)  = DMC(1,1)*DCIS(1) + DMC(1,2)*DCIS(2)
          VMC(2)  = DMC(2,1)*DCIS(1) + DMC(2,2)*DCIS(2)
          VMC(3)  = DMC(3,1)*DCIS(1) + DMC(3,2)*DCIS(2)
C
          VCM(1)  = DMC(1,1)*VM(1) + DMC(2,1)*VM(2) + DMC(3,1)*VM(3)
          VCM(2)  = DMC(1,2)*VM(1) + DMC(2,2)*VM(2) + DMC(3,2)*VM(3)
C
          VFC(1)  = DFC(1,1)*DCIS(1) + DFC(1,2)*DCIS(2)
          VFC(2)  = DFC(2,1)*DCIS(1) + DFC(2,2)*DCIS(2)
          VFC(3)  = DFC(3,1)*DCIS(1) + DFC(3,2)*DCIS(2)
C
          VCF(1)  = DFC(1,1)*VF(1) + DFC(2,1)*VF(2) + DFC(3,1)*VF(3)
          VCF(2)  = DFC(1,2)*VF(1) + DFC(2,2)*VF(2) + DFC(3,2)*VF(3)
C
          DO 350 I = 1,3
            EDGL(I+8* (IE-1))   = VM(I) + VMF(I) + VMC(I)
            EDGL(I+3+8* (IE-1)) = VF(I) + VFM(I) + VFC(I)
  350     CONTINUE
          EDGL(7+8* (IE-1)) = VT(1) + VCM(1) + VCF(1)
          EDGL(8+8* (IE-1)) = VT(2) + VCM(2) + VCF(2)
  360   CONTINUE
      END IF
C
      END
