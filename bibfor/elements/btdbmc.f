      SUBROUTINE  BTDBMC(B,D,JACOB,NDIM,NNO,NBSIG,PHENOZ,BTDB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 27/09/2010   AUTEUR PROIX J-M.PROIX 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C       BTDBMC -- CALCUL DU PRODUIT BT*D*B DONNANT LA MATRICE
C                 DE RIGIDITE ELEMENTAIRE POUR TOUS LES
C                 ELEMENTS ISOPARAMETRIQUES
C
C   ARGUMENT        E/S  TYPE         ROLE
C    B(6,81)        IN     R        MATRICE (B) CALCULEE AU POINT
C                                   D'INTEGRATION COURANT ET RELIANT
C                                   LES DEFORMATIONS DU PREMIER ORDRE
C                                   AUX DEPLACEMENTS
C    D(6,6)         IN     R        MATRICE DE HOOKE DANS LE REPERE
C                                   GLOBAL
C    JACOB          IN     R        PRODUIT JACOBIEN*POIDS AU POINT
C                                   D'INTEGRATION COURANT
C    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE A
C                                   L'ELEMENT
C    PHENOM         IN     K16      TYPE DU MATERIAU ('ELAS' OU
C                                   'ELAS_ORTH' OU 'ELAS_ISTR')
C    BTDB(81,81)    OUT    R        MATRICE ELEMENTAIRE DE RIGIDITE
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*(*) PHENOZ
           REAL*8        B(NBSIG,*),D(NBSIG,*),JACOB,BTDB(81,81)
C -----  VARIABLES LOCALES
           CHARACTER*16 PHENOM
           LOGICAL      LTEATT
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C ---- INITIALISATIONS
C      ---------------
      PHENOM = PHENOZ
      NBINCO = NNO*NDIM
C
C      ------------
C ---- CAS ISOTROPE
C      ------------
      IF (PHENOM.EQ.'ELAS'.OR.PHENOM.EQ.'ELAS_HYPER') THEN
C
C ----   CAS CONTRAINTES PLANES ET DEFORMATIONS PLANES
C        ---------------------------------------------
        IF (LTEATT(' ','C_PLAN','OUI').OR.
     &      LTEATT(' ','D_PLAN','OUI')) THEN
C
          L1 = NBINCO - 1
C
          D1 = D(1,1)*JACOB
          D2 = D(1,2)*JACOB
          D3 = D(4,4)*JACOB
C
          DO 10 I1 = 1, L1, 2
          DO 10 I2 = 1, L1, 2
C
              R11 = B(1,I1)  *B(1,I2)
              R21 = B(2,I1+1)*B(1,I2)
              R12 = B(1,I1)  *B(2,I2+1)
              R22 = B(2,I1+1)*B(2,I2+1)
C
              BTDB(I1,I2)     = BTDB(I1,I2)     + D1*R11 + D3*R22
              BTDB(I1+1,I2)   = BTDB(I1+1,I2)   + D2*R21 + D3*R12
              BTDB(I1,  I2+1) = BTDB(I1,  I2+1) + D2*R12 + D3*R21
              BTDB(I1+1,I2+1) = BTDB(I1+1,I2+1) + D1*R22 + D3*R11
C
 10       CONTINUE
C
C ----   CAS AXI
C        -------
       ELSEIF (LTEATT(' ','AXIS','OUI').AND.
     &        (.NOT.LTEATT(' ','FOURIER','OUI'))) THEN
C
          L1 = NBINCO - 1
C
          D1 = D(1,1)*JACOB
          D2 = D(1,2)*JACOB
          D3 = D(4,4)*JACOB
C
          DO 20 I1 = 1, L1, 2
          DO 20 I2 = 1, L1, 2
C
              R11 = B(1,I1)  *B(1,I2)
              R21 = B(2,I1+1)*B(1,I2)
              R31 = B(3,I1)  *B(1,I2)
C
              R12 = B(1,I1)  *B(2,I2+1)
              R22 = B(2,I1+1)*B(2,I2+1)
              R32 = B(3,I1)  *B(2,I2+1)
C
              R13 = B(1,I1)  *B(3,I2)
              R23 = B(2,I1+1)*B(3,I2)
              R33 = B(3,I1)  *B(3,I2)
C
              BTDB(I1,I2)     = BTDB(I1,I2)     + D1*(R11+R33)
     &                          + D2*(R31+R13)  + D3*R22
              BTDB(I1+1,I2)   = BTDB(I1+1,I2)   + D2*(R21+R23)
     &                          + D3*R12
              BTDB(I1,  I2+1) = BTDB(I1,  I2+1) + D2*(R12+R32)
     &                          + D3*R21
              BTDB(I1+1,I2+1) = BTDB(I1+1,I2+1) + D1*R22 + D3*R11
C
 20       CONTINUE
C
C ----   CAS 3D
C        ------
        ELSEIF (LTEATT(' ','DIM_TOPO_MAILLE','3')) THEN
C
          L2 = NBINCO - 2
C
          D1 = D(1,1)*JACOB
          D2 = D(1,2)*JACOB
          D3 = D(4,4)*JACOB
C
          DO 30 I1 = 1, L2, 3
          DO 30 I2 = 1, L2, 3
C
              R11 = B(1,I1)*B(1,I2)
              R12 = B(1,I1)*B(2,I2+1)
              R13 = B(1,I1)*B(3,I2+2)
C
              R21 = B(2,I1+1)*B(1,I2)
              R22 = B(2,I1+1)*B(2,I2+1)
              R23 = B(2,I1+1)*B(3,I2+2)
C
              R31 = B(3,I1+2)*B(1,I2)
              R32 = B(3,I1+2)*B(2,I2+1)
              R33 = B(3,I1+2)*B(3,I2+2)
C
              BTDB(I1,I2)   = BTDB(I1,I2)   + D1*R11 + D3*(R22+R33)
              BTDB(I1+1,I2) = BTDB(I1+1,I2) + D2*R21 + D3*R12
              BTDB(I1+2,I2) = BTDB(I1+2,I2) + D2*R31 + D3*R13
C
              BTDB(I1,  I2+1) = BTDB(I1,  I2+1) + D2*R12 + D3*R21
              BTDB(I1+1,I2+1) = BTDB(I1+1,I2+1) + D1*R22 + D3*(R11+R33)
              BTDB(I1+2,I2+1) = BTDB(I1+2,I2+1) + D2*R32 + D3*R23
C
              BTDB(I1,  I2+2) = BTDB(I1,  I2+2) + D2*R13 + D3*R31
              BTDB(I1+1,I2+2) = BTDB(I1+1,I2+2) + D2*R23 + D3*R32
              BTDB(I1+2,I2+2) = BTDB(I1+2,I2+2) + D1*R33 + D3*(R11+R22)
C
 30       CONTINUE
C
C ----   CAS FOURIER
C        -----------
        ELSEIF ( LTEATT(' ','FOURIER','OUI')) THEN
C
          L2 = NBINCO - 2
C
          D1 = D(1,1)*JACOB
          D2 = D(1,2)*JACOB
          D3 = D(4,4)*JACOB
C
          DO 40 I1 = 1, L2, 3
          DO 40 I2 = 1, L2, 3
C
              R11 = B(1,I1)*B(1,I2)
              R12 = B(1,I1)*B(2,I2+1)
              R13 = B(1,I1)*B(3,I2+2)
C
              R21 = B(2,I1+1)*B(1,I2)
              R22 = B(2,I1+1)*B(2,I2+1)
              R23 = B(2,I1+1)*B(3,I2+2)
C
              R31 = B(3,I1+2)*B(1,I2)
              R32 = B(3,I1+2)*B(2,I2+1)
              R33 = B(3,I1+2)*B(3,I2+2)
C
              RS13 = B(1,I1)  *B(3,I2)
              RS23 = B(2,I1+1)*B(3,I2)
              SR31 = B(3,I1)  *B(1,I2)
              SR32 = B(3,I1)  *B(2,I2+1)
              SR33 = B(3,I1)  *B(3,I2+2)
              RS33 = B(3,I1+2)*B(3,I2)
C
              S33 = B(3,I1)  *B(3,I2)
C
              BTDB(I1,I2)   = BTDB(I1,I2)   + D1*(R11+S33)
     &                        + D2*(RS13+SR31) + D3*(R22+R33)
              BTDB(I1+1,I2) = BTDB(I1+1,I2) + D2*(R21+RS23) + D3*R12
              BTDB(I1+2,I2) = BTDB(I1+2,I2) + D1*RS33+       D2*R31
     &                        + D3*(-R13+SR33)
C
              BTDB(I1,  I2+1) = BTDB(I1,  I2+1) + D2*(R12+SR32) + D3*R21
              BTDB(I1+1,I2+1) = BTDB(I1+1,I2+1) + D1*R22 + D3*(R11+R33)
              BTDB(I1+2,I2+1) = BTDB(I1+2,I2+1) + D2*R32 - D3*R23
C
              BTDB(I1,  I2+2) = BTDB(I1,  I2+2) + D1*SR33 + D2*R13
     &                          + D3*(-R31+RS33)
              BTDB(I1+1,I2+2) = BTDB(I1+1,I2+2) + D2*R23 - D3*R32
              BTDB(I1+2,I2+2) = BTDB(I1+2,I2+2) + D1*R33
     &                          + D3*(R11+R22+S33-SR31-RS13)
C
 40       CONTINUE
C
        ENDIF
C      -------------------------------------
C ---- CAS ORTHOTROPE ET ISOTROPE TRANSVERSE
C      -------------------------------------
      ELSEIF (PHENOM.EQ.'ELAS_ORTH'.OR.PHENOM.EQ.'ELAS_ISTR') THEN
C
           CALL BTDBPR(B,D,JACOB,NBSIG,NBINCO,BTDB)
C
      ELSE
          CALL U2MESK('F','ELEMENTS_15',1,PHENOM)
      ENDIF
C.============================ FIN DE LA ROUTINE ======================
      END
