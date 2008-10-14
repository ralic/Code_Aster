      SUBROUTINE DSTEDG ( XYZL, OPTION, PGL, DEPL, EDGL )
      IMPLICIT  NONE
      REAL*8        XYZL(3,*), PGL(3,*), DEPL(*), EDGL(*)
      CHARACTER*16  OPTION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2008   AUTEUR REZETTE C.REZETTE 
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
C     EFFORTS ET DEFORMATIONS GENERALISES DE L'ELEMENT DE PLAQUE DST
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
C     IN  OPTION : NOM DE L'OPTION DE CALCUL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  DEPL   : DEPLACEMENTS
C     OUT EDGL   : EFFORTS OU DEFORMATIONS GENERALISES AUX NOEUDS DANS
C                  LE REPERE INTRINSEQUE A L'ELEMENT
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
      INTEGER  NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER  MULTIC,NE,K,J,I,IE
      REAL*8   DEPF(9),DEPM(6)
      REAL*8   DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2)
      REAL*8   BFB(3,9),BFA(3,3),BFN(3,9),BF(3,9),DFC(3,2)
      REAL*8   BCA(2,3),BCN(2,9),HFT2(2,6),AN(3,9)
      REAL*8   BM(3,6),BDM(3),BDF(3),DCIS(2),VF(3),VM(3),VT(2)
      REAL*8   VFM(3),VMF(3),VMC(3),VFC(3),VCM(2),VCF(2)
      REAL*8 QSI, ETA, CARAT3(21), T2EV(4), T2VE(4), T1VE(9)
      LOGICAL  ELASCO
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

C     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------

C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
      CALL GTRIA3 ( XYZL, CARAT3 )
C     ----- CARACTERISTIQUES DES MATERIAUX --------
      CALL DXMATE(FAMI,DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     +                                         ELASCO,T2EV,T2VE,T1VE)
C     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
      DO 20 J = 1,NNO
        DO 10 I = 1,2
          DEPM(I+2* (J-1)) = DEPL(I+6* (J-1))
   10   CONTINUE
        DEPF(1+3* (J-1)) = DEPL(1+2+6* (J-1))
        DEPF(2+3* (J-1)) = DEPL(3+2+6* (J-1))
        DEPF(3+3* (J-1)) = -DEPL(2+2+6* (J-1))
   20 CONTINUE
C     ------ CALCUL DE LA MATRICE BM -----------------------------------
      CALL DXTBM ( CARAT3(9), BM )
      DO 30 K = 1,3
        BDM(K) = 0.D0
   30 CONTINUE
      DO 50 I = 1,3
        DO 40 J = 1,6
          BDM(I) = BDM(I) + BM(I,J)*DEPM(J)
   40   CONTINUE
   50 CONTINUE
C     ------- CALCUL DU PRODUIT HF.T2 ----------------------------------
      CALL DSXHFT ( DF, CARAT3(9), HFT2 )
C     ------- CALCUL DES MATRICES BCA ET AN ----------------------------
      CALL DSTCIS ( DCI , CARAT3 , HFT2 , BCA , AN )
C     ------ VT = BCA.AN.DEPF ------------------------------------------
      DO 60 K = 1,18
        BCN(K,1) = 0.D0
   60 CONTINUE
      VT(1) = 0.D0
      VT(2) = 0.D0
      DO 90 I = 1,2
        DO 80 J = 1,9
          DO 70 K = 1,3
            BCN(I,J) = BCN(I,J) + BCA(I,K)*AN(K,J)
   70     CONTINUE
          VT(I) = VT(I) + BCN(I,J)*DEPF(J)
   80   CONTINUE
   90 CONTINUE
C     ------- CALCUL DE LA MATRICE BFB ---------------------------------
      CALL DSTBFB ( CARAT3(9) , BFB )
C
      IF (OPTION(1:4).EQ.'DEGE') THEN
        DO 180 IE = 1,NE
C ---     COORDONNEES DU POINT D'INTEGRATION COURANT :
C         ------------------------------------------
          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)
C           ----- CALCUL DE LA MATRICE BFA AU POINT QSI ETA -----------
          CALL DSTBFA ( QSI, ETA , CARAT3 , BFA )
C           ------ BF = BFB + BFA.AN -----------------------------------
          DO 100 K = 1,27
            BFN(K,1) = 0.D0
  100     CONTINUE
          DO 130 I = 1,3
            DO 120 J = 1,9
              DO 110 K = 1,3
                BFN(I,J) = BFN(I,J) + BFA(I,K)*AN(K,J)
  110         CONTINUE
              BF(I,J) = BFB(I,J) + BFN(I,J)
  120       CONTINUE
  130     CONTINUE
          DO 140 K = 1,3
            BDF(K) = 0.D0
  140     CONTINUE
          DO 160 I = 1,3
            DO 150 J = 1,9
              BDF(I) = BDF(I) + BF(I,J)*DEPF(J)
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
      ELSE
        DO 190 K = 1,3
          VM(K) = 0.D0
          VFM(K) = 0.D0
          VMC(K) = 0.0D0
  190   CONTINUE
        VCM(1) = 0.0D0
        VCM(2) = 0.0D0
        DO 210 I = 1,3
          DO 200 J = 1,3
            VM(I) = VM(I) + DM(I,J)*BDM(J)
            VFM(I) = VFM(I) + DMF(I,J)*BDM(J)
  200     CONTINUE
  210   CONTINUE
        DO 320 IE = 1,NE
C ---     COORDONNEES DU POINT D'INTEGRATION COURANT :
C         ------------------------------------------
          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)
C           ----- CALCUL DE LA MATRICE BFA AU POINT QSI ETA ------------
          CALL DSTBFA ( QSI, ETA , CARAT3 , BFA )
C           ------ BF = BFB + BFA.AN -----------------------------------
          DO 220 K = 1,27
            BFN(K,1) = 0.D0
  220     CONTINUE
          DO 250 I = 1,3
            DO 240 J = 1,9
              DO 230 K = 1,3
                BFN(I,J) = BFN(I,J) + BFA(I,K)*AN(K,J)
  230         CONTINUE
              BF(I,J) = BFB(I,J) + BFN(I,J)
  240       CONTINUE
  250     CONTINUE
          DO 260 K = 1,3
            BDF(K) = 0.D0
            VF(K) = 0.D0
            VMF(K) = 0.D0
            VFC(K) = 0.0D0
  260     CONTINUE
          VCF(1) = 0.0D0
          VCF(2) = 0.0D0
C           ------ VF = DF.BF.DEPF , VMF = DMF.BF.DEPF -------------
          DO 280 I = 1,3
            DO 270 J = 1,9
              BDF(I) = BDF(I) + BF(I,J)*DEPF(J)
  270       CONTINUE
  280     CONTINUE
          DO 300 I = 1,3
            DO 290 J = 1,3
              VF(I) = VF(I) + DF(I,J)*BDF(J)
              VMF(I) = VMF(I) + DMF(I,J)*BDF(J)
  290       CONTINUE
  300     CONTINUE
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
          DO 310 I = 1,3
            EDGL(I+8* (IE-1))   = VM(I) + VMF(I) + VMC(I)
            EDGL(I+3+8* (IE-1)) = VF(I) + VFM(I) + VFC(I)
  310     CONTINUE
          EDGL(7+8* (IE-1)) = VT(1) + VCM(1) + VCF(1)
          EDGL(8+8* (IE-1)) = VT(2) + VCM(2) + VCF(2)
  320   CONTINUE
      END IF
C
      END
