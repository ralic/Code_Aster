      SUBROUTINE MMMVFF(PHASE ,NDIM  ,NNL   ,NBCPS ,HPG   ,
     &                  FFL   ,TAU1  ,TAU2  ,JACOBI,NDEXFR,
     &                  COEFFS,DLAGRF,RESE  ,LAMBDA,COEFFF,
     &                  VECTFF)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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
      REAL*8       HPG,FFL(9),JACOBI,DLAGRF(2)
      REAL*8       TAU1(3),TAU2(3),RESE(3)
      REAL*8       COEFFS
      REAL*8       LAMBDA,COEFFF
      REAL*8       VECTFF(18)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DU VECTEUR LAGR_C
C
C ----------------------------------------------------------------------
C
C
C IN  PHASE  : PHASE DE CALCUL
C              'SANS' - PAS DE CONTACT
C              'ADHE' - CONTACT ADHERENT
C              'GLIS' - CONTACT GLISSANT
C              'EXFR' - EXCLUSION D'UNE DIRECTION DE FROTTEMENT
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNL    : NOMBRE DE NOEUDS LAGRANGE
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFL    : FONCTIONS DE FORMES LAGR. 
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  NBCPS  : NOMBRE DE COMPOSANTES/NOEUD DES LAGR_C+LAGR_F
C IN  TAU1   : PREMIER VECTEUR TANGENT
C IN  TAU2   : SECOND VECTEUR TANGENT
C IN  COEFFS : COEF_STAB_FROT
C IN  NDEXFR : ENTIER CODE POUR EXCLUSION DIRECTION DE FROTTEMENT
C IN  DLAGRF : INCREMENT DEPDEL DU LAGRANGIEN DE FROTTEMENT
C IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT 
C               GTK = LAMBDAF + COEFFR*VITESSE
C IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C OUT VECTFF : VECTEUR ELEMENTAIRE LAGR_F
C
C ----------------------------------------------------------------------
C
      INTEGER   I, K, L, II,NBCPF
      REAL*8    TT(2)
      REAL*8    NRESE,INTER(2)
      INTEGER   NDEXCL(9)
C
C ----------------------------------------------------------------------
C

C
C --- INITIALISATIONS
C
      DO 305 I = 1,2
        TT(I)    = 0.D0
        INTER(I) = 0.D0
 305  CONTINUE 
      NBCPF  = NBCPS-1
C
C --- MATRICE DE CHANGEMENT DE REPERE DES LAGR. DE FROTTEMENT
C
      IF (NDIM.EQ.2) THEN
        DO 301 K = 1,NDIM
          TT(1) = TAU1(K)*TAU1(K) +TT(1)
 301    CONTINUE
        TT(1) = DLAGRF(1)*TT(1)
        TT(2) = 0.D0
C
      ELSE IF (NDIM.EQ.3) THEN
        DO 31 K = 1,NDIM
          TT(1) = (DLAGRF(1)*TAU1(K)+DLAGRF(2)*TAU2(K))*TAU1(K)+TT(1)
 31     CONTINUE
        DO 32 K = 1,NDIM
          TT(2) = (DLAGRF(1)*TAU1(K)+DLAGRF(2)*TAU2(K))*TAU2(K)+TT(2)
 32     CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
C      
      IF (PHASE.EQ.'GLIS') THEN
        CALL NORMEV(RESE  ,NRESE )
      ENDIF  
      INTER(1) = 0.D0
      INTER(2) = 0.D0 
      IF (NDIM.EQ.2) THEN
        DO 228 I = 1,2
         INTER(1) = (DLAGRF(1)*TAU1(I)-RESE(I))*TAU1(I)+INTER(1)
  228   CONTINUE
      ELSE IF (NDIM.EQ.3)THEN
        DO 233 I = 1,3
          INTER(1)=(DLAGRF(1)*TAU1(I)+
     &              DLAGRF(2)*TAU2(I)-RESE(I))*TAU1(I)+INTER(1)
          INTER(2)=(DLAGRF(1)*TAU1(I)+
     &              DLAGRF(2)*TAU2(I)-RESE(I))*TAU2(I)+INTER(2)
  233   CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF    
C
C --- CALCUL DU VECTEUR  
C
      IF (PHASE.EQ.'SANS') THEN
        DO 101 I=1,NNL
          DO 102 L=1,NBCPF
            II = (I-1)*NBCPF+L
            VECTFF(II) = VECTFF(II)-
     &                   HPG*FFL(I)*JACOBI*
     &                   TT(L)/COEFFS
  102     CONTINUE
  101   CONTINUE 
      ELSEIF (PHASE.EQ.'EXFR') THEN
        CALL ISDECO(NDEXFR,NDEXCL,9)
        DO 111 I=1,9
          IF (NDEXCL(I).EQ.1) THEN
            II = (I-1)*NBCPF+1
            VECTFF(II) = 0.D0
          ENDIF  
  111   CONTINUE     
      ELSEIF ((PHASE.EQ.'ADHE').OR.(PHASE.EQ.'GLIS')) THEN
        DO 63 I = 1,NNL
          DO 64 L = 1,NBCPF   
            II = (I-1)*NBCPF+L
            VECTFF(II) = VECTFF(II)+
     &                   HPG*FFL(I)*JACOBI*
     &                   COEFFF*LAMBDA*INTER(L)/COEFFS
 64       CONTINUE                    
 63     CONTINUE    
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
