      SUBROUTINE MMMTFF(PHASE ,NDIM  ,NBCPS ,NNL   ,HPG   ,
     &                  FFL   ,JACOBI,TAU1  ,TAU2  ,RESE  ,
     &                  NRESE ,LAMBDA,NDEXFR,COEFFS,COEFFF,
     &                  MATRFF)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 02/02/2010   AUTEUR DESOZA T.DESOZA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*4  PHASE
      INTEGER      NDIM,NNL,NBCPS,NDEXFR
      REAL*8       HPG,FFL(9),JACOBI
      REAL*8       TAU1(3),TAU2(3)
      REAL*8       RESE(3),NRESE  
      REAL*8       LAMBDA
      REAL*8       COEFFS,COEFFF      
      REAL*8       MATRFF(18,18)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE LAGR_F/LAGR_F
C
C ----------------------------------------------------------------------
C
C
C IN  PHASE  : PHASE DE CALCUL
C              'SANS' - PAS DE CONTACT
C              'GLIS' - CONTACT GLISSANT
C              'EXFR' - EXCLUSION D'UNE DIRECTION DE FROTTEMENT
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNL    : NOMBRE DE NOEUDS LAGRANGE 
C IN  NBCPS  : NB DE DDL DE LAGRANGE
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFL    : FONCTIONS DE FORMES LAGR. 
C IN  TAU1   : PREMIER VECTEUR TANGENT
C IN  TAU2   : SECOND VECTEUR TANGENT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  NDEXFR : ENTIER CODE POUR EXCLUSION DIRECTION DE FROTTEMENT
C IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT 
C               GTK = LAMBDAF + COEFFR*VITESSE
C IN  NRESE  : RACINE DE LA NORME DE RESE
C IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C IN  COEFFS : COEF_STAB_FROT
C OUT MATRFF : MATRICE ELEMENTAIRE LAGR_F/LAGR_F
C
C ----------------------------------------------------------------------
C
      INTEGER   I, J, K , L,II,JJ,IDIM,NBCPF,NDEXCL(9)
      REAL*8    TT(3,3)  
      REAL*8    H1(3),H2(3)
      REAL*8    R(2,2)         
C
C ----------------------------------------------------------------------
C
C
C --- INITIALISATIONS
C
      CALL MATINI( 3, 3,0.D0,TT    )
      CALL MATINI( 2, 2,0.D0,R     )
      CALL VECINI(3,0.D0,H1)
      CALL VECINI(3,0.D0,H2)
      NBCPF = NBCPS - 1     
C
C --- MATRICE DE CHANGEMENT DE REPERE [T]. [TT] = [T]t*[T]
C
      DO 301 K = 1,NDIM                        
        TT(1,1) = TAU1(K)*TAU1(K) + TT(1,1)    
        TT(1,2) = TAU1(K)*TAU2(K) + TT(1,2)    
        TT(2,1) = TAU2(K)*TAU1(K) + TT(2,1)    
        TT(2,2) = TAU2(K)*TAU2(K) + TT(2,2)    
 301  CONTINUE                                 
C
      IF (PHASE.EQ.'SANS') THEN
        DO 284 I = 1,NNL
          DO 283 J = 1,NNL
            DO 282 L = 1,NBCPF
              DO 281 K = 1,NBCPF
                II = (NDIM-1)*(I-1)+L
                JJ = (NDIM-1)*(J-1)+K
                MATRFF(II,JJ) = MATRFF(II,JJ)-
     &                          HPG*FFL(I)*FFL(J)*JACOBI*TT(L,K)
 281          CONTINUE
 282        CONTINUE
 283      CONTINUE
 284    CONTINUE 
      ELSEIF (PHASE.EQ.'GLIS') THEN 
C       --- VECTEUR PROJ. BOULE SUR TGT1: {H1} = [K].{T1}
        CALL MKKVEC(RESE  ,NRESE ,NDIM  ,TAU1  ,H1    )
C       --- VECTEUR PROJ. BOULE SUR TGT1: {H2} = [K].{T2}        
        CALL MKKVEC(RESE  ,NRESE ,NDIM  ,TAU2  ,H2    )       
C      --- MATRICE [R] = [T]t*[T]-[T]t*[H]
        DO 857 IDIM = 1,NDIM
          R(1,1) = (TAU1(IDIM)-H1(IDIM))*TAU1(IDIM) + R(1,1)
          R(1,2) = (TAU1(IDIM)-H1(IDIM))*TAU2(IDIM) + R(1,2)
          R(2,1) = (TAU2(IDIM)-H2(IDIM))*TAU1(IDIM) + R(2,1)
          R(2,2) = (TAU2(IDIM)-H2(IDIM))*TAU2(IDIM) + R(2,2)
 857    CONTINUE
C
        DO 24 I = 1,NNL
          DO 23 J = 1,NNL
            DO 22 L = 1,NBCPF
              DO 21 K = 1,NBCPF
                II = (NDIM-1)*(I-1)+L
                JJ = (NDIM-1)*(J-1)+K
                MATRFF(II,JJ) = MATRFF(II,JJ)+
     &                          HPG*FFL(I)*FFL(J)*JACOBI*
     &                          COEFFF*LAMBDA*R(L,K) / COEFFS
 21           CONTINUE
 22         CONTINUE
 23       CONTINUE
 24     CONTINUE
C   
      ELSEIF (PHASE.EQ.'EXFR') THEN
        CALL ISDECO(NDEXFR,NDEXCL,9)
        DO 121 I = 1,NNL
          IF (NDEXCL(I).EQ.1) THEN
C           ON ANNULE QUE LE PREMIER LAGRANGE DE FROTTEMENT
C           DEVELOPPEMENT NON GENERIQUE ENCORE EN 3D
            II = NBCPF*(I-1)+1
            MATRFF(II,II) = 1.D0
          ENDIF
 121    CONTINUE                   
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
