      SUBROUTINE T3GRIG ( NOMTE, XYZL, OPTION, PGL, RIG, ENER )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      REAL*8         XYZL(3,*), PGL(*), RIG(*), ENER(*)
      CHARACTER*16   OPTION , NOMTE
C     ------------------------------------------------------------------
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C     MATRICE DE RIGIDITE DE L'ELEMENT T3GAMMA (AVEC CISAILLEMENT)
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  OPTION : OPTION RIGI_MECA, RIGI_MECA_SENS* OU EPOT_ELEM
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
C     OUT RIG    : MATRICE DE RIGIDITE
C     OUT ENER   : TERMES POUR ENER_POT (EPOT_ELEM)
C     ------------------------------------------------------------------
      INTEGER INT, MULTIC
      REAL*8 WGT,DEPL(18)
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 BFB(3,9)
      REAL*8 BC(2,9)
      REAL*8 BM(3,6)
      REAL*8 XAB1(3,6),XAB2(3,9),XAB3(2,9),XAB4(3,9)
C         ---(9,9)---
      REAL*8 KF(81)
      REAL*8 KC(81)
C         -----(9,9) ----(9,9)
      REAL*8 FLEXI(81),FLEX(81)
C          -----(6,6)
      REAL*8 MEMB(36)
C                   -----(6,9)  -----(6,9)
      REAL*8 MEFLI(54),MEFL(54),KMC(54),KFC(81)
      REAL*8 BSIGTH(24),ENERTH,CARAT3(25)
      REAL*8 T2EV(4), T2VE(4), T1VE(9), JACOB(5), QSI, ETA
      LOGICAL COUPMF,INDITH
      INTEGER   I, JCOQU, JDEPG, K
      REAL*8   CTOR, EXCENT, ZERO
      REAL*8   AIRE
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

      CALL R8INIR(81,ZERO,KC,1)
      CALL R8INIR(81,ZERO,FLEX,1)
      CALL R8INIR(36,ZERO,MEMB,1)
      CALL R8INIR(54,ZERO,MEFL,1)

C     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEE --------------------------
      CALL DXMATE('RIGI',DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     &                                   COUPMF,T2EV,T2VE,T1VE)

C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE --------
      CALL GTRIA3 ( XYZL, CARAT3 )

C     ------------------------------------------------------------------
C     CALCUL DE LA MATRICE DE RIGIDITE DE L'ELEMENT EN MEMBRANE
C     ------------------------------------------------------------------
C     ------ CALCUL DE LA MATRICE BM -----------------------------------
      CALL DXTBM ( CARAT3(9), BM )
C     ------ CALCUL DU PRODUIT BMT.DM.BM -------------------------------
      CALL UTBTAB('ZERO',3,6,DM,BM,XAB1,MEMB)
      AIRE = CARAT3(8)
      DO 10 K = 1,36
        MEMB(K) = MEMB(K)* AIRE
10    CONTINUE

C     ------------------------------------------------------------------
C     CALCUL DES MATRICES DE RIGIDITE DE L'ELEMENT EN FLEXION ET
C     COUPLAGE MEMBRANE/FLEXION
C     ------------------------------------------------------------------

C     ------- CALCUL DE LA MATRICE BFB -------------------------------
      CALL DSTBFB ( CARAT3(9) , BFB )

C     ------- CALCUL DU PRODUIT BFBT.DF.BFB --------------------------
      CALL UTBTAB('ZERO',3,9,DF,BFB,XAB2,FLEX)

C        ---- CALCUL DE LA MATRICE BC ----------------------------------
      QSI = 1.D0/3.D0
      ETA = QSI
      CALL T3GBC(XYZL,  QSI, ETA, BC)

C        ---- CALCUL DU PRODUIT BCT.DC.BC -----------------------------
      CALL UTBTAB('ZERO',2,9,DC,BC,XAB3,KC)
C
      DO 40 K = 1,81
        FLEXI(K) = (FLEX(K)+ KC(K))*AIRE
   40 CONTINUE
C
      IF ( OPTION.EQ.'RIGI_MECA'      .OR.
     +     OPTION.EQ.'RIGI_MECA_SENSI' .OR.
     +     OPTION.EQ.'RIGI_MECA_SENS_C' ) THEN
        CALL DXTLOC(FLEXI,MEMB,MEFL,CTOR,RIG)
      ELSE IF (OPTION.EQ.'EPOT_ELEM') THEN
        CALL JEVECH('PDEPLAR','L',JDEPG)
        CALL UTPVGL(3,6,PGL,ZR(JDEPG),DEPL)
        CALL DXQLOE(FLEX,MEMB,MEFL,CTOR,COUPMF,DEPL,ENER)
        CALL BSTHPL(NOMTE(1:8),BSIGTH,INDITH)
        IF (INDITH) THEN
          DO 90 I = 1, 24
            ENERTH = ENERTH + DEPL(I)*BSIGTH(I)
  90      CONTINUE
          ENER(1) = ENER(1) - ENERTH
        ENDIF
      END IF
C
C
      END
