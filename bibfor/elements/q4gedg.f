      SUBROUTINE Q4GEDG ( XYZL, OPTION, PGL, DEPL, EDGL )
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
C     EFFORTS ET DEFORMATIONS GENERALISES DE L'ELEMENT DE PLAQUE Q4GAMMA
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  OPTION : NOM DE L'OPTION DE CALCUL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  DEPL   : DEPLACEMENTS
C     OUT EDGL   : EFFORTS OU DEFORMATIONS GENERALISES AUX NOEUDS DANS
C                  LE REPERE INTRINSEQUE A L'ELEMENT
      INTEGER  NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER  MULTIC,NE,K,J,I,IE
      REAL*8   DEPF(12),DEPM(8)
      REAL*8   DF(3,3),DM(3,3),DMF(3,3),DMC(3,2),DFC(3,2)
      REAL*8   DCI(2,2),DC(2,2)
      REAL*8   BF(3,12),BM(3,8),BC(2,12)
      REAL*8   BDM(3),BDF(3),BCDF(2),DCIS(2)
      REAL*8   VF(3),VM(3),VT(2)
      REAL*8   VFM(3),VMF(3),VMC(3),VFC(3),VCM(2),VCF(2),CARAQ4(25)
      REAL*8   T2EV(4), T2VE(4), T1VE(9), JACOB(5), QSI, ETA
      LOGICAL  COUPMF
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
C     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
C
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
      CALL GQUAD4(XYZL,CARAQ4)
C     ----- CARACTERISTIQUES DES MATERIAUX --------
      CALL DXMATE(FAMI,DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     +                                         COUPMF,T2EV,T2VE,T1VE)
C     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
      DO 20 J = 1,NNO
        DO 10 I = 1,2
          DEPM(I+2* (J-1)) = DEPL(I+6* (J-1))
   10   CONTINUE
        DEPF(1+3* (J-1)) = DEPL(1+2+6* (J-1))
        DEPF(2+3* (J-1)) = DEPL(3+2+6* (J-1))
        DEPF(3+3* (J-1)) = -DEPL(2+2+6* (J-1))
   20 CONTINUE
      IF (OPTION(1:4).EQ.'DEGE') THEN
        DO 90 IE = 1,NE

          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)

C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4(XYZL,QSI,ETA,JACOB)
C           ----- CALCUL DE LA MATRICE BM ------------------------------
          CALL DXQBM(QSI,ETA,JACOB(2),BM)
C           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA -------------
          CALL DSQBFB(QSI,ETA,JACOB(2),BF)
C           ---- CALCUL DE LA MATRICE BC AU POINT QSI ETA --------------
          CALL Q4GBC(QSI,ETA,JACOB(2),CARAQ4,BC)
C           ------ BCDF = BC.DEPF -------------------------------------
          BCDF(1) = 0.D0
          BCDF(2) = 0.D0
          DO 30 J = 1,12
            BCDF(1) = BCDF(1) + BC(1,J)*DEPF(J)
            BCDF(2) = BCDF(2) + BC(2,J)*DEPF(J)
   30     CONTINUE
          DO 40 K = 1,3
            BDF(K) = 0.D0
            BDM(K) = 0.D0
   40     CONTINUE
          DO 70 I = 1,3
            DO 50 J = 1,12
              BDF(I) = BDF(I) + BF(I,J)*DEPF(J)
   50       CONTINUE
            DO 60 J = 1,8
              BDM(I) = BDM(I) + BM(I,J)*DEPM(J)
   60       CONTINUE
   70     CONTINUE
          DO 80 I = 1,3
            EDGL(I+8* (IE-1)) = BDM(I)
            EDGL(I+3+8* (IE-1)) = BDF(I)
   80     CONTINUE
          EDGL(7+8* (IE-1)) = BCDF(1)
          EDGL(8+8* (IE-1)) = BCDF(2)
C           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
          EDGL(3+8* (IE-1)) = EDGL(3+8* (IE-1))/2.D0
          EDGL(6+8* (IE-1)) = EDGL(6+8* (IE-1))/2.D0
          EDGL(7+8* (IE-1)) = BCDF(1)/2.D0
          EDGL(8+8* (IE-1)) = BCDF(2)/2.D0
   90   CONTINUE
      ELSE
        DO 180 IE = 1,NE

          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)

C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4(XYZL,QSI,ETA,JACOB)
C           ----- CALCUL DE LA MATRICE BM ------------------------------
          CALL DXQBM(QSI,ETA,JACOB(2),BM)
C           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA -------------
          CALL DSQBFB(QSI,ETA,JACOB(2),BF)
C           ---- CALCUL DE LA MATRICE BC AU POINT QSI ETA --------------
          CALL Q4GBC(QSI,ETA,JACOB(2),CARAQ4,BC)
C           ------ VT = DC.BC.DEPF -------------------------------------
          VT(1) = 0.D0
          VT(2) = 0.D0
          BCDF(1) = 0.D0
          BCDF(2) = 0.D0
          DO 100 J = 1,12
            BCDF(1) = BCDF(1) + BC(1,J)*DEPF(J)
            BCDF(2) = BCDF(2) + BC(2,J)*DEPF(J)
  100     CONTINUE
          VT(1) = DC(1,1)*BCDF(1) + DC(1,2)*BCDF(2)
          VT(2) = DC(2,1)*BCDF(1) + DC(2,2)*BCDF(2)
          DO 110 K = 1,3
            BDF(K) = 0.D0
            BDM(K) = 0.D0
            VF(K) = 0.D0
            VM(K) = 0.D0
            VFM(K) = 0.D0
            VMF(K) = 0.D0
            VMC(K) = 0.0D0
            VFC(K) = 0.0D0
  110     CONTINUE
          VCM(1) = 0.0D0
          VCM(2) = 0.0D0
          VCF(1) = 0.0D0
          VCF(2) = 0.0D0
C           ------ VF = DF.BF.DEPF , VFM = DMF.BM.DEPM ----------------
C           ------ VM = DM.BM.DEPM , VMF = DMF.BF.DEPF ----------------
          DO 140 I = 1,3
            DO 120 J = 1,12
              BDF(I) = BDF(I) + BF(I,J)*DEPF(J)
  120       CONTINUE
            DO 130 J = 1,8
              BDM(I) = BDM(I) + BM(I,J)*DEPM(J)
  130       CONTINUE
  140     CONTINUE
          DO 160 I = 1,3
            DO 150 J = 1,3
              VF(I) = VF(I) + DF(I,J)*BDF(J)
              VFM(I) = VFM(I) + DMF(I,J)*BDM(J)
              VM(I) = VM(I) + DM(I,J)*BDM(J)
              VMF(I) = VMF(I) + DMF(I,J)*BDF(J)
  150       CONTINUE
  160     CONTINUE
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
          DO 170 I = 1,3
            EDGL(I+8* (IE-1))   = VM(I) + VMF(I) + VMC(I)
            EDGL(I+3+8* (IE-1)) = VF(I) + VFM(I) + VFC(I)
  170     CONTINUE
          EDGL(7+8* (IE-1)) = VT(1) + VCM(1) + VCF(1)
          EDGL(8+8* (IE-1)) = VT(2) + VCM(2) + VCF(2)
  180   CONTINUE
      END IF
C
      END
