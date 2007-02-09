      SUBROUTINE MMMAB2(NDIM,NNE,NNM,
     &                  HPG,FFPC,FFPR,JACOBI,
     &                  LAMBDA,TYALGF,COEFFA,COEFFS,COEFFP,
     &                  COEFFF,TAU1,TAU2,RESE,NRESE,MPROJ,
     &                  MMAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2007   AUTEUR TORKHANI M.TORKHANI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER  NDIM,NNE,NNM,TYALGF
      REAL*8   HPG,FFPC(9),FFPR(9),JACOBI  
      REAL*8   LAMBDA,COEFFF,COEFFA,COEFFS,COEFFP    
      REAL*8   TAU1(3),TAU2(3),RESE(3),NRESE  
      REAL*8   MMAT(81,81)  
      REAL*8   MPROJ(3,3)
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0364
C ----------------------------------------------------------------------
C
C CALCUL DE B ET DE BT POUR LE CONTACT METHODE CONTINUE
C SANS ADHERENCE
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFPC   : FONCTIONS DE FORME DU POINT DE CONTACT
C IN  FFPR   : FONCTIONS DE FORME DE LA PROJECTION DU POINT DE CONTACT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  LAMBDA : VALEUR DU SEUIL_INIT
C IN  COEFFA : COEF_REGU_FROT
C IN  COEFFS : COEF_STAB_FROT
C IN  COEFFP : COEF_PENA_FROT
C IN  TYALGF : TYPE D'ALGORITHME DE FROTTEMENT
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C IN  TAU1   : PREMIERE TANGENTE
C IN  TAU2   : SECONDE TANGENTE
C IN  RESE   : PROJECTION DE LA BOULE UNITE POUR LE FROTTEMENT
C IN  NRESE  : RACINE DE LA NORME DE RESE
C IN  MPROJ  : MATRICE DE L'OPERATEUR DE PROJECTION
C I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER I,J,K,L,II,JJ
      REAL*8  C1(3),C2(3),C3(3),D1(3),D2(3),D3(3),H1(3),H2(3)
      REAL*8  G(3,3),D(3,3),H(3,3),B(3,3),R(3,3)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      DO 300 I = 1,3    
        DO 290 J = 1,3
          G(I,J) = 0.D0
          D(I,J) = 0.D0
          H(I,J) = 0.D0
          B(I,J) = 0.D0
          R(I,J) = 0.D0
  290   CONTINUE
  300 CONTINUE   
      DO 3 K = 1,3
        C1(K) = MPROJ(K,1)
        C2(K) = MPROJ(K,2)
        C3(K) = MPROJ(K,3)
        D1(K) = 0.D0
        D2(K) = 0.D0
        D3(K) = 0.D0
        H1(K) = 0.D0
        H2(K) = 0.D0
3     CONTINUE    
C
C --- CALCUL  DE IK(/\,U)
C 
      CALL MKKVEC(RESE,NRESE,NDIM,C1,D1)
      CALL MKKVEC(RESE,NRESE,NDIM,C2,D2)
      CALL MKKVEC(RESE,NRESE,NDIM,C3,D3)

      DO 12 K = 1,3
        G(K,1) = D1(K)
        G(K,2) = D2(K)
        G(K,3) = D3(K)
12    CONTINUE
C
C --- D:K(-)C.C
C
      DO 13 I = 1,NDIM
        DO 14 J = 1,NDIM
          DO 15 K = 1,NDIM
            D(I,J) = G(K,I)*MPROJ(K,J) + D(I,J)
  15      CONTINUE
  14    CONTINUE
  13  CONTINUE

      CALL MKKVEC(RESE,NRESE,NDIM,TAU1,H1)
      CALL MKKVEC(RESE,NRESE,NDIM,TAU2,H2)

      DO 16 K = 1,3
        H(K,1) = H1(K)
        H(K,2) = H2(K)
 16   CONTINUE
C
C --- B:K(-)T.C
C
      DO  23 I = 1,NDIM-1
        DO 24 J = 1,NDIM
          DO 25  K = 1,NDIM
            B(I,J) = H(K,I)*MPROJ(K,J)+B(I,J)
  25      CONTINUE
  24    CONTINUE
  23  CONTINUE      
C
C --- CALCUL DE B (1ER BLOC) ESCLAVE - ESCLAVE ET SA TRANSPOSEE
C 
      DO 661 I=1,NNE
        DO 662 J=1,NNE
          DO 663 L=1,NDIM-1
            DO 664 K=1,NDIM
              II = (2*NDIM)*(I-1)+NDIM+1+L
              JJ = (J-1)*(2*NDIM)+K
              MMAT(II,JJ) = - LAMBDA*COEFFF*HPG*FFPC(I)*FFPC(J)*
     &                        JACOBI*B(L,K)
              MMAT(JJ,II) = MMAT(II,JJ)
 664        CONTINUE
 663      CONTINUE
 662    CONTINUE
 661  CONTINUE
C
C --- CALCUL DE B (SECOND BLOC) ESCLAVE - MAITRE ET SA TRANSPOSEE
C 
      DO 665 I=1,NNE
        DO 666 J=1,NNM
          DO 667 L=1,NDIM-1
            DO 668 K=1,NDIM
              II = (2*NDIM)*(I-1)+NDIM+1+L
              JJ = 2*NDIM*NNE+(J-1)*NDIM+K     
              MMAT(II,JJ) = LAMBDA*COEFFF*HPG*FFPC(I)*FFPR(J)*
     &                      JACOBI*B(L,K)
              MMAT(JJ,II) = MMAT(II,JJ)     
 668        CONTINUE
 667      CONTINUE
 666    CONTINUE
 665  CONTINUE
C
C  --- ON CALCULE LA MATRICE B_U (1ER BLOC E-E)
C 
      DO 700 I = 1,NNE
        DO 690 J = 1,NNE
          DO 680 L = 1,NDIM
            DO 670 K = 1,NDIM
              II = (2*NDIM)*(I-1)+L
              JJ = (J-1)*(2*NDIM)+K
              IF (TYALGF .EQ. 1) THEN
                MMAT(II,JJ) = - COEFFA*COEFFF*HPG*LAMBDA*FFPC(I)*
     &                          FFPC(J)*JACOBI*D(L,K)
              ELSE
                MMAT(II,JJ) = - COEFFS*COEFFF*HPG*LAMBDA*FFPC(I)*
     &                          FFPC(J)*JACOBI*D(L,K)
              END IF
  670       CONTINUE
  680     CONTINUE
  690   CONTINUE
  700 CONTINUE
C
C  --- ON CALCULE LE SECOND BLOC DE B_U (E-M)
C
      DO 940 I = 1,NNE
        DO 930 J = 1,NNM
          DO 920 L = 1,NDIM
            DO 910 K = 1,NDIM
              II = (2*NDIM)*(I-1)+L
              JJ = (2*NDIM)*NNE+(J-1)*NDIM+K        
              IF (TYALGF .EQ. 1) THEN
                MMAT(II,JJ) = COEFFA*COEFFF*LAMBDA*HPG*FFPC(I)*
     &                        FFPR(J)*JACOBI*D(L,K)
              ELSE
                MMAT(II,JJ) = COEFFS*COEFFF*LAMBDA*HPG*FFPC(I)*
     &                        FFPR(J)*JACOBI*D(L,K)
              END IF
 910        CONTINUE
 920      CONTINUE
 930    CONTINUE
 940  CONTINUE
C
C --- ON CALCULE LE TROISIEME  BLOC DE B_U (M-E)
C
      DO 980 I = 1,NNM
        DO 970 J = 1,NNE
          DO 960 L = 1,NDIM
            DO 950 K = 1,NDIM
              II = (2*NDIM)*NNE+NDIM*(I-1)+L
              JJ = (2*NDIM)*(J-1)+K         
              IF (TYALGF .EQ. 1) THEN
                MMAT(II,JJ) = COEFFA*COEFFF*LAMBDA*HPG*FFPR(I)*
     &                        FFPC(J)*JACOBI*D(L,K)
              ELSE
                MMAT(II,JJ) = COEFFS*COEFFF*LAMBDA*HPG*FFPR(I)*
     &                        FFPC(J)*JACOBI*D(L,K)
              END IF
 950        CONTINUE
 960      CONTINUE
 970    CONTINUE
 980  CONTINUE
C
C --- ON CALCULE LE QUATRIEME  BLOC DE B_U (M-M)
C
      DO 820 I = 1,NNM
        DO 810 J = 1,NNM
          DO 800 L = 1,NDIM
            DO 890 K = 1,NDIM
              II = NNE*(2*NDIM)+NDIM*(I-1)+L
              JJ = NNE*(2*NDIM)+NDIM*(J-1)+K
              IF (TYALGF .EQ. 1) THEN
                MMAT(II,JJ) = - COEFFA*COEFFF*LAMBDA*HPG*FFPR(I)*
     &                          FFPR(J)*JACOBI*D(L,K)
              ELSE
                MMAT(II,JJ) = - COEFFS*COEFFF*LAMBDA*HPG*FFPR(I)*
     &                          FFPR(J)*JACOBI*D(L,K)
              END IF
 890        CONTINUE
 800      CONTINUE
 810    CONTINUE
 820  CONTINUE
C
C --- ATTENTION F EST CALCULEE SAUF S'IL YA  GLISSEMENT
C --- R(I,J)= H_I . TAU_J
C
      DO 857 K = 1,NDIM
        R(1,1) = (TAU1(K)-H1(K))*TAU1(K) + R(1,1)
        R(1,2) = (TAU1(K)-H1(K))*TAU2(K) + R(1,2)
        R(2,1) = (TAU2(K)-H2(K))*TAU1(K) + R(2,1)
        R(2,2) = (TAU2(K)-H2(K))*TAU2(K) + R(2,2)
 857  CONTINUE
C
      DO 831 I = 1,NNE
        DO 832 J = 1,NNE
          DO 833 L = 1,NDIM-1
            DO 834 K = 1,NDIM-1
              II = 2*NDIM*(I-1)+NDIM+1+L
              JJ = 2*NDIM*(J-1)+NDIM+1+K              
              IF (TYALGF .EQ. 1) THEN
                MMAT(II,JJ) = COEFFF*LAMBDA*HPG*FFPC(I)*FFPC(J)*
     &                        JACOBI*R(L,K) / COEFFA
              ELSE
                MMAT(II,JJ) = COEFFF*LAMBDA*HPG*FFPC(I)*FFPC(J)*
     &                        JACOBI*R(L,K) / COEFFS
              END IF
 834        CONTINUE
 833      CONTINUE
 832    CONTINUE
 831  CONTINUE 
C
      CALL JEDEMA()      
      END
