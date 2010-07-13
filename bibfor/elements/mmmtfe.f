      SUBROUTINE MMMTFE(PHASE ,NDIM  ,NNE   ,NNL   ,NBCPS ,
     &                  HPG   ,JACOBI,FFE   ,FFL   ,TAU1  ,
     &                  TAU2  ,MPROJT,RESE  ,NRESE ,LAMBDA,
     &                  NDEXFR,COEFFF,COEFFS,COEFFR,MATRFE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/07/2010   AUTEUR MASSIN P.MASSIN 
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
      INTEGER      NDIM,NNE,NNL,NDEXFR,NBCPS
      REAL*8       FFE(9),FFL(9)
      REAL*8       HPG,JACOBI
      REAL*8       TAU1(3),TAU2(3)
      REAL*8       RESE(3),NRESE       
      REAL*8       MPROJT(3,3)
      REAL*8       LAMBDA
      REAL*8       COEFFS,COEFFR,COEFFF       
      REAL*8       MATRFE(18,27)       
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE LAGR_F/DEPL_ESCL 
C
C ----------------------------------------------------------------------
C
C
C IN  PHASE  : PHASE DE CALCUL
C              'GLIS' - CONTACT GLISSANT METHODE LAGRANGIENNE
C              'ADHE' - CONTACT ADHERENT METHODE LAGRANGIENNE
C              'PGLI' - CONTACT GLISSANT METHODE PENALISEE
C              'PADH' - CONTACT ADHERENT METHODE PENALISEE
C              'EXFR' - EXCLUSION D'UNE DIRECTION DE FROTTEMENT
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NBCPS  : NB DE DDL DE LAGRANGE
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE 
C IN  TAU1   : PREMIER VECTEUR TANGENT
C IN  TAU2   : SECOND VECTEUR TANGENT
C IN  MPROJT : MATRICE DE PROJECTION TANGENTE
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
C IN  FFL    : FONCTIONS DE FORMES LAGR. 
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  NDEXFR : ENTIER CODE POUR EXCLUSION DIRECTION DE FROTTEMENT
C IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT 
C               GTK = LAMBDAF + COEFFR*VITESSE
C IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT 
C IN  COEFFR : COEF_REGU_FROT
C IN  COEFFS : COEF_STAB_FROT
C IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C OUT MATRFE : MATRICE ELEMENTAIRE LAGR_F/DEPL_E
C
C ----------------------------------------------------------------------
C
      INTEGER   INOF,INOE,ICMP,IDIM,I,J,K,II,JJ,NBCPF
      INTEGER   NDEXCL(9),CMP
      REAL*8    A(2,3),B(2,3)
      REAL*8    H1(3),H2(3)
      REAL*8    H(3,2)       
C
C ----------------------------------------------------------------------
C
C
C --- INITIALISATIONS
C
      CALL MATINI( 2, 3,0.D0,A     )
      CALL MATINI( 2, 3,0.D0,B     )
      CALL MATINI( 3, 2,0.D0,H     )
      CALL VECINI(3,0.D0,H1)
      CALL VECINI(3,0.D0,H2)
      NBCPF  = NBCPS - 1
C
C --- PARTIE ADHERENT METHODE LAGRANGIENNE
C       
      IF (PHASE.EQ.'ADHE') THEN   
C ---   MATRICE [A] = [T]t*[P]
        DO 4  I = 1,NDIM
          DO 5  K = 1,NDIM
            A(1,I) = TAU1(K)*MPROJT(K,I) + A(1,I)           
  5       CONTINUE
  4     CONTINUE
        DO 6  I = 1,NDIM
          DO 7  K = 1,NDIM
            A(2,I) = TAU2(K)*MPROJT(K,I) + A(2,I)          
  7       CONTINUE
  6     CONTINUE      
C
        DO 284 INOF = 1,NNL
          DO 283 INOE = 1,NNE
            DO 282 ICMP = 1,NBCPF
              DO 281 IDIM = 1,NDIM
                II = NBCPF*(INOF-1)+ICMP
                JJ = NDIM*(INOE-1)+IDIM
                MATRFE(II,JJ) = MATRFE(II,JJ)-
     &                          HPG*FFL(INOF)*FFE(INOE)*JACOBI*
     &                          LAMBDA*COEFFF*COEFFR*A(ICMP,IDIM)/COEFFS
     
 281          CONTINUE
 282        CONTINUE
 283      CONTINUE
 284    CONTINUE       
C
C --- PARTIE GLISSANT METHODE LAGRANGIENNE
C      
      ELSEIF (PHASE.EQ.'GLIS') THEN
C       --- VECTEUR PROJ. BOULE SUR TGT1: {H1} = [K].{T1}
        CALL MKKVEC(RESE  ,NRESE ,NDIM  ,TAU1  ,H1    )
C       --- VECTEUR PROJ. BOULE SUR TGT1: {H2} = [K].{T2}        
        CALL MKKVEC(RESE  ,NRESE ,NDIM  ,TAU2  ,H2    )
C       --- MATRICE [H] = [{H1}{H2}]        
        DO 16 IDIM = 1,3
          H(IDIM,1) = H1(IDIM)
          H(IDIM,2) = H2(IDIM)
 16     CONTINUE      
C       --- MATRICE [B] = [P]*[H]t
        DO 23 ICMP = 1,NBCPF
          DO 24 J = 1,NDIM
            DO 25  K = 1,NDIM
              B(ICMP,J) = H(K,ICMP)*MPROJT(K,J)+B(ICMP,J)
  25        CONTINUE
  24      CONTINUE
  23    CONTINUE        
C    
        DO 184 INOF = 1,NNL
          DO 183 INOE = 1,NNE
            DO 182 ICMP = 1,NBCPF
              DO 181 IDIM = 1,NDIM
                II = NBCPF*(INOF-1)+ICMP
                JJ = NDIM*(INOE-1)+IDIM
                MATRFE(II,JJ) = MATRFE(II,JJ)-
     &                          HPG*FFL(INOF)*FFE(INOE)*JACOBI*
     &                          LAMBDA*COEFFF*COEFFR*B(ICMP,IDIM)/COEFFS

 181          CONTINUE
 182        CONTINUE
 183      CONTINUE
 184    CONTINUE       

C
C --- PARTIE ADHERENT METHODE PENALISEE
C       
      ELSEIF (PHASE.EQ.'PADH') THEN   
C ---   MATRICE [A] = [T]t*[P]
        DO 304  I = 1,NDIM
          DO 305  K = 1,NDIM
            A(1,I) = TAU1(K)*MPROJT(K,I) + A(1,I)           
 305      CONTINUE
 304    CONTINUE
        DO 306  I = 1,NDIM
          DO 307  K = 1,NDIM
            A(2,I) = TAU2(K)*MPROJT(K,I) + A(2,I)          
 307      CONTINUE
 306    CONTINUE      
C
        DO 384 INOF = 1,NNL
          DO 383 INOE = 1,NNE
            DO 382 ICMP = 1,NBCPF
              DO 381 IDIM = 1,NDIM
                II = NBCPF*(INOF-1)+ICMP
                JJ = NDIM*(INOE-1)+IDIM
                MATRFE(II,JJ) = MATRFE(II,JJ)-
     &                          HPG*FFL(INOF)*FFE(INOE)*JACOBI*
     &                          LAMBDA*COEFFF*COEFFR*A(ICMP,IDIM)
     
 381          CONTINUE
 382        CONTINUE
 383      CONTINUE
 384    CONTINUE       
C
C --- PARTIE GLISSANT METHODE PENALISEE
C      
      ELSEIF (PHASE.EQ.'PGLIS') THEN
C       --- VECTEUR PROJ. BOULE SUR TGT1: {H1} = [K].{T1}
        CALL MKKVEC(RESE  ,NRESE ,NDIM  ,TAU1  ,H1    )
C       --- VECTEUR PROJ. BOULE SUR TGT1: {H2} = [K].{T2}        
        CALL MKKVEC(RESE  ,NRESE ,NDIM  ,TAU2  ,H2    )
C       --- MATRICE [H] = [{H1}{H2}]        
        DO 416 IDIM = 1,3
          H(IDIM,1) = H1(IDIM)
          H(IDIM,2) = H2(IDIM)
 416    CONTINUE      
C       --- MATRICE [B] = [P]*[H]t
        DO 423 ICMP = 1,NBCPF
          DO 424 J = 1,NDIM
            DO 425  K = 1,NDIM
              B(ICMP,J) = H(K,ICMP)*MPROJT(K,J)+B(ICMP,J)
 425        CONTINUE
 424      CONTINUE
 423    CONTINUE        
C    
        DO 484 INOF = 1,NNL
          DO 483 INOE = 1,NNE
            DO 482 ICMP = 1,NBCPF
              DO 481 IDIM = 1,NDIM
                II = NBCPF*(INOF-1)+ICMP
                JJ = NDIM*(INOE-1)+IDIM
                MATRFE(II,JJ) = MATRFE(II,JJ)-
     &                          HPG*FFL(INOF)*FFE(INOE)*JACOBI*
     &                          LAMBDA*COEFFF*COEFFR*B(ICMP,IDIM)

 481          CONTINUE
 482        CONTINUE
 483      CONTINUE
 484    CONTINUE 

      ELSEIF (PHASE.EQ.'EXFR') THEN
        CALL ISDECO(NDEXFR,NDEXCL,9)
        CMP = 0
        DO 9 IDIM = 1,NDIM
          IF (TAU1(IDIM).NE.0.D0) CMP = IDIM
 9      CONTINUE
        DO 122 INOF = 1,NNL
          IF (NDEXCL(INOF).EQ.1) THEN
            DO 121 INOE = 1,NNE
C               ON ANNULE QUE LE PREMIER LAGRANGE DE FROTTEMENT
C               DEVELOPPEMENT NON GENERIQUE ENCORE EN 3D
              II     = NBCPF*(INOF-1)+1
C               ON ANNULE QUE LA DIRECTION DE FROTTEMENT
C               // AUX AXES CAR DEVELOPPEMENT NON GENERIQUE
              JJ     = NDIM*(INOE-1)+CMP              
              MATRFE(II,JJ) = 0.D0
 121        CONTINUE
          ENDIF
 122    CONTINUE 

      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
