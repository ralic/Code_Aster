      SUBROUTINE MMMTMF(PHASE ,NDIM  ,NNM   ,NNL   ,NBCPS ,
     &                  HPG   ,JACOBI,FFM   ,FFL   ,TAU1  ,
     &                  TAU2  ,MPROJT,RESE  ,NRESE ,LAMBDA,
     &                  NDEXFR,COEFFF,MATRMF)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/01/2010   AUTEUR DESOZA T.DESOZA 
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
      INTEGER      NDIM,NNM,NNL,NDEXFR,NBCPS
      REAL*8       FFM(9),FFL(9)
      REAL*8       HPG,JACOBI
      REAL*8       TAU1(3),TAU2(3)
      REAL*8       RESE(3),NRESE           
      REAL*8       MPROJT(3,3)
      REAL*8       LAMBDA
      REAL*8       COEFFF       
      REAL*8       MATRMF(27,18)       
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE DEPL_MAIT/LAGR_F
C
C ----------------------------------------------------------------------
C
C
C IN  PHASE  : PHASE DE CALCUL
C              'GLIS' - CONTACT GLISSANT
C              'ADHE' - CONTACT ADHERENT
C              'EXFR' - EXCLUSION D'UNE DIRECTION DE FROTTEMENT
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NBCPS  : NB DE DDL DE LAGRANGE
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE 
C IN  TAU1   : PREMIER VECTEUR TANGENT
C IN  TAU2   : SECOND VECTEUR TANGENT
C IN  MPROJT : MATRICE DE PROJECTION TANGENTE
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
C IN  FFL    : FONCTIONS DE FORMES LAGR. 
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  NDEXFR : ENTIER CODE POUR EXCLUSION DIRECTION DE FROTTEMENT
C IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT 
C               GTK = LAMBDAF + COEFFR*VITESSE
C IN  NRESE  : RACINE DE LA NORME DE RESE
C IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C OUT MATRMF : MATRICE ELEMENTAIRE DEPL_M/LAGR_F
C
C ----------------------------------------------------------------------
C
      INTEGER   INOF,INOM,ICMP,IDIM,I,J,K,II,JJ,NBCPF
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
C --- PARTIE ADHERENT
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
          DO 283 INOM = 1,NNM
            DO 282 ICMP = 1,NBCPF
              DO 281 IDIM = 1,NDIM
                JJ = NBCPF*(INOF-1)+ICMP
                II = NDIM*(INOM-1)+IDIM
                MATRMF(II,JJ) = MATRMF(II,JJ)+
     &                          HPG*FFL(INOF)*FFM(INOM)*JACOBI*
     &                          LAMBDA*COEFFF*A(ICMP,IDIM)

 281          CONTINUE
 282        CONTINUE
 283      CONTINUE
 284    CONTINUE       
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
          DO 183 INOM = 1,NNM
            DO 182 ICMP = 1,NBCPF
              DO 181 IDIM = 1,NDIM
                JJ = NBCPF*(INOF-1)+ICMP
                II = NDIM*(INOM-1)+IDIM
                MATRMF(II,JJ) = MATRMF(II,JJ)+
     &                          HPG*FFL(INOF)*FFM(INOM)*JACOBI*
     &                          LAMBDA*COEFFF*B(ICMP,IDIM)

 181          CONTINUE
 182        CONTINUE
 183      CONTINUE
 184    CONTINUE
      ELSEIF (PHASE.EQ.'EXFR') THEN
        CALL ISDECO(NDEXFR,NDEXCL,9)
        CMP = 0
        DO 9 IDIM = 1,NDIM
          IF (TAU1(IDIM).NE.0.D0) CMP = IDIM
 9      CONTINUE
        DO 122 INOF = 1,NNL
          IF (NDEXCL(INOF).EQ.1) THEN
            DO 121 INOM = 1,NNM
C               ON ANNULE QUE LE PREMIER LAGRANGE DE FROTTEMENT
C               DEVELOPPEMENT NON GENERIQUE ENCORE EN 3D
              JJ     = NBCPF*(INOF-1)+ 1
C               ON ANNULE QUE LA DIRECTION DE FROTTEMENT
C               // AUX AXES CAR DEVELOPPEMENT NON GENERIQUE
              II     = NDIM*(INOM-1)+CMP            
              MATRMF(II,JJ) = 0.D0
 121        CONTINUE
          ENDIF
 122    CONTINUE     
 
 
      ELSE
        CALL ASSERT(.FALSE.)
      
      ENDIF



C
      END
