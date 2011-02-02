      SUBROUTINE DKTCOD ( XYZL, OPTION, PGL, ICOU, INIV, DEPL,
     +                    CDL, MULTIC )
      IMPLICIT NONE
      INTEGER       ICOU, INIV
      REAL*8        XYZL(3,*),PGL(3,*), DEPL(*), CDL(*)
      CHARACTER*16  OPTION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 02/02/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       DEFORMATIONS DE L'ELEMENT DE PLAQUE DKT
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
C     IN  OPTION : NOM DE L'OPTION DE CALCUL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  ICOU   : NUMERO DE LA COUCHE
C     IN  INIV   : NIVEAU DANS LA COUCHE (-1:INF , 0:MOY , 1:SUP)
C     IN  DEPL   : DEPLACEMENTS
C     OUT CDL    : DEFORMATIONS AUX NOEUDS 'EPSI_ELNO'
C                   DANS LE REPERE INTRINSEQUE A L'ELEMENT
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
      INTEGER  MULTIC,NE,JCACO,K,J,I,IE,NBCOU,JNBSP
      REAL*8   DEPF(9),DEPM(6),HIC,ZMIN
      REAL*8   DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2)
      REAL*8   H(3,3),D1I(2,2),D2I(2,4),DFC(3,2)
      REAL*8   BF(3,9),BM(3,6)
      REAL*8   SM(3),SF(3)
      REAL*8   HFT2(2,6),HLT2(4,6)
      REAL*8   VT(2),LAMBDA(4)
      REAL*8   EPS(3),SIG(3),DCIS(2),CIST(2),X3I,EPAIS,EXCEN
      REAL*8   QSI, ETA, CARAT3(21), T2EV(4), T2VE(4), T1VE(9)
      LOGICAL  ELASCO
      CHARACTER*4 FAMI
C     ------------------------------------------------------------------
C
      CALL ELREF5(' ','NOEU',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
      NE  = NNO
      FAMI='NOEU'
C
C     ----- RAPPEL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
      CALL GTRIA3 ( XYZL, CARAT3 )
C     ----- CARACTERISTIQUES DES MATERIAUX --------

      CALL DXMATE(FAMI,DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     +                                         ELASCO,T2EV,T2VE,T1VE)
C     -------- CALCUL DE D1I ET D2I ------------------------------------

      IF (MULTIC.EQ.0) THEN

        CALL JEVECH('PCACOQU','L',JCACO)
        EPAIS = ZR(JCACO)
        EXCEN = ZR(JCACO+5-1)
        CALL JEVECH('PNBSP_I','L',JNBSP)
        NBCOU = ZI(JNBSP)
        HIC = EPAIS/NBCOU
        ZMIN = -EPAIS/2.0D0
        IF (INIV.LT.0) THEN
          X3I = ZMIN + (ICOU-1)*HIC + EXCEN
        ELSE IF (INIV.EQ.0) THEN
          X3I = ZMIN + (ICOU-1)*HIC + HIC/2.0D0 + EXCEN
        ELSE IF (INIV.GT.0) THEN
          X3I = ZMIN + (ICOU-1)*HIC + HIC + EXCEN
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
        CALL DXDMUL(ICOU,INIV,T1VE,T2VE,H,D1I,D2I,X3I)
      END IF
C     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
      DO 30 J = 1,3
        DO 20 I = 1,2
          DEPM(I+2* (J-1)) = DEPL(I+6* (J-1))
   20   CONTINUE
        DEPF(1+3* (J-1)) = DEPL(1+2+6* (J-1))
        DEPF(2+3* (J-1)) = DEPL(3+2+6* (J-1))
        DEPF(3+3* (J-1)) = -DEPL(2+2+6* (J-1))
   30 CONTINUE
C     ------ CALCUL DE LA MATRICE BM -----------------------------------
      CALL DXTBM ( CARAT3(9), BM )
C     ------ SM = BM.DEPM ----------------------------------------------
      DO 40 I = 1,3
        SM(I) = 0.D0
   40 CONTINUE
      DO 60 I = 1,3
        DO 50 J = 1,6
          SM(I) = SM(I) + BM(I,J)*DEPM(J)
   50   CONTINUE
   60 CONTINUE

C     ------- CALCUL DU PRODUIT HF.T2 ----------------------------------
      CALL DSXHFT ( DF, CARAT3(9), HFT2 )
C     ------ VT = HFT2.TKT.DEPF ---------------------------------------
      CALL DKTTXY ( CARAT3(16), CARAT3(13), HFT2, DEPF, VT )

        DO 120 IE = 1,NE

          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)

C           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
          CALL DKTBF ( QSI, ETA, CARAT3, BF )
          DO 80 I = 1,3
            SF(I) = 0.D0
   80     CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 100 I = 1,3
            DO 90 J = 1,9
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
   90       CONTINUE
  100     CONTINUE
          DO 110 I = 1,3
            EPS(I) = SM(I) + X3I*SF(I)
  110     CONTINUE
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
  120   CONTINUE
C
C
      END
