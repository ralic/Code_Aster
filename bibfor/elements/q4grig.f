      SUBROUTINE Q4GRIG ( NOMTE, XYZL, OPTION, PGL, RIG, ENER )
      IMPLICIT NONE
      REAL*8         XYZL(4,*), PGL(*), RIG(*), ENER(*)
      CHARACTER*16   OPTION , NOMTE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C
C     MATRICE DE RIGIDITE DE L'ELEMENT Q4GAMMA (AVEC CISAILLEMENT)
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  OPTION : OPTION RIGI_MECA, RIGI_MECA_SENS* OU EPOT_ELEM_DEPL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
C     OUT RIG    : MATRICE DE RIGIDITE
C     OUT ENER   : TERMES POUR ENER_POT (EPOT_ELEM_DEPL)
C     ------------------------------------------------------------------
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
      INTEGER INT, MULTIC
      REAL*8 WGT,DEPL(24)
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 BF(3,12)
      REAL*8 BC(2,12)
      REAL*8 BM(3,8)
      REAL*8 XAB1(3,12),XAB2(2,12),XAB3(3,8)
      REAL*8 XAB4(3,2)
C                   ---(12,12)---
      REAL*8 KF(144)
      REAL*8 KC(144)
C                   -----(12,12) ----(12,12)
      REAL*8 FLEXI(144),FLEX(144)
C                   -----(8,8)   -----(8,8)
      REAL*8 MEMBI(64),MEMB(64)
C                   -----(8,12)  -----(8,12)
      REAL*8 MEFLI(96),MEFL(96),KMC(96),KFC(144)
      REAL*8 BSIGTH(24),ENERTH,CARAQ4(25)
      REAL*8 T2EV(4), T2VE(4), T1VE(9), JACOB(5), QSI, ETA
      LOGICAL ELASCO,INDITH
      INTEGER   I, JCOQU, JDEPG, K
      REAL*8   CTOR, EXCENT, ZERO
      INTEGER   NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
C     ------------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     &                                         IVF,IDFDX,IDFD2,JGANO)
C
      ZERO = 0.0D0
      ENERTH = ZERO
C
      CALL JEVECH('PCACOQU','L',JCOQU)
      CTOR   = ZR(JCOQU+3)
      EXCENT = ZR(JCOQU+4)
C
C --- ON NE CALCULE PAS ENCORE LA MATRICE DE RIGIDITE D'UN ELEMENT
C --- Q4G EXCENTRE, ON S'ARRETE EN ERREUR FATALE :
C     ------------------------------------------
      IF (EXCENT.NE.ZERO) THEN
        CALL U2MESS('F','ELEMENTS2_57')
      ENDIF

      CALL R8INIR(96 ,ZERO,KMC,1)
      CALL R8INIR(144,ZERO,KFC,1)

C     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEE --------------------------
      CALL DXMATE(DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,.FALSE.,
     &                                         ELASCO,T2EV,T2VE,T1VE)
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
      CALL GQUAD4 ( XYZL, CARAQ4 )

      CALL R8INIR(144,ZERO,FLEX,1)
      CALL R8INIR( 64,ZERO,MEMB,1)
      CALL R8INIR( 96,ZERO,MEFL,1)

      DO 80 INT = 1,NPG
        QSI = ZR(ICOOPG-1+NDIM*(INT-1)+1)
        ETA = ZR(ICOOPG-1+NDIM*(INT-1)+2)
C        ---------------------------------------------------------------
C        CALCUL DE LA MATRICE DE RIGIDITE DE L'ELEMENT EN FLEXION
C        ---------------------------------------------------------------
C        ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE --------------------
        CALL JQUAD4(XYZL,QSI,ETA,JACOB)
C        ---- CALCUL DE LA MATRICE BF ----------------------------------
        CALL DSQBFB(QSI,ETA,JACOB(2),BF)
C        ---- CALCUL DU PRODUIT BFT.DF.BF ------------------------------
        CALL UTBTAB('ZERO',3,12,DF,BF,XAB1,KF)
C        ---- CALCUL DE LA MATRICE BC ----------------------------------
        CALL Q4GBC(QSI,ETA,JACOB(2),CARAQ4,BC)
C        ---- CALCUL DU PRODUIT BCT.DC.BC -----------------------------
        CALL UTBTAB('ZERO',2,12,DC,BC,XAB2,KC)
        IF (ELASCO) THEN
C        ----- CALCUL DU PRODUIT BFT.DFC.BC ----------------------
          CALL UTDTAB('ZERO',3,2,12,12,DFC,BC,BF,XAB4,KFC)
        ENDIF
C        ----- CALCUL DE LA SOMME KF + KC = FLEXI ----------------------
        DO 40 K = 1,144
          FLEXI(K) = KF(K) + KC(K) + KFC(K)
   40   CONTINUE
        WGT = ZR(IPOIDS+INT-1)*JACOB(1)
        DO 50 K = 1,144
          FLEX(K) = FLEX(K) + FLEXI(K)*WGT
   50   CONTINUE
C        ---------------------------------------------------------------
C        CALCUL DE LA MATRICE DE RIGIDITE DE L'ELEMENT EN MEMBRANE
C        ---------------------------------------------------------------
C        ----- CALCUL DE LA MATRICE BM ---------------------------------
        CALL DXQBM(QSI,ETA,JACOB(2),BM)
C        ----- CALCUL DU PRODUIT BMT.DM.BM -----------------------------
        CALL UTBTAB('ZERO',3,8,DM,BM,XAB3,MEMBI)
C        ----- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE ------------
        DO 60 K = 1,64
          MEMB(K) = MEMB(K) + MEMBI(K)*WGT
   60   CONTINUE
        IF (ELASCO) THEN
C        ----- CALCUL DU PRODUIT BMT.DMC.BC ----------------------
          CALL UTDTAB('ZERO',3,2,12,8,DMC,BC,BM,XAB4,KMC)
        ENDIF
        IF (MULTIC.EQ.2) THEN
C           ------------------------------------------------------------
C           CALCUL DES MATRICES DE COUPLAGE MEMBRANE/FLEXION
C           ------------------------------------------------------------
C           ----- CALCUL DU PRODUIT BMT.DMF.BF -------------------------
          CALL UTCTAB('ZERO',3,12,8,DMF,BF,BM,XAB1,MEFLI)
          DO 70 K = 1,96
            MEFL(K) = MEFL(K) + (MEFLI(K)+KMC(K))*WGT
   70     CONTINUE
        END IF
   80 CONTINUE
C
      IF ( OPTION.EQ.'RIGI_MECA'      .OR.
     &     OPTION.EQ.'RIGI_MECA_SENSI' .OR.
     &     OPTION.EQ.'RIGI_MECA_SENS_C' ) THEN
        CALL DXQLOC(FLEX,MEMB,MEFL,CTOR,RIG)
C
      ELSE IF (OPTION.EQ.'EPOT_ELEM_DEPL') THEN
        CALL JEVECH('PDEPLAR','L',JDEPG)
        CALL UTPVGL(4,6,PGL,ZR(JDEPG),DEPL)
        CALL DXQLOE(FLEX,MEMB,MEFL,CTOR,MULTIC,DEPL,ENER)
        CALL BSTHPL(NOMTE(1:8),BSIGTH,INDITH)
        IF (INDITH) THEN
          DO 90 I = 1, 24
            ENERTH = ENERTH + DEPL(I)*BSIGTH(I)
  90      CONTINUE
          ENER(1) = ENER(1) - ENERTH
        ENDIF
      END IF
C
      END
