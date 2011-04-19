      SUBROUTINE MMMTE1(NDIM  ,NNL   ,NNE   ,NNM   ,WPG   ,
     &                  FFL   ,COEFCS,JACOBI,NDEXCL,MATRCC,
     &                  MATRCE,MATRCM,MATREC,MATRMC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/04/2011   AUTEUR ABBAS M.ABBAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER      NDIM,NNE,NNL,NNM
      REAL*8       FFL(9)
      INTEGER      NDEXCL(9)
      REAL*8       WPG,JACOBI
      REAL*8       COEFCS
      REAL*8       MATRCC(9,9)  
      REAL*8       MATREC(27,9),MATRMC(27,9)
      REAL*8       MATRCE(9,27),MATRCM(9,27)            
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
C
C CALCUL DES MATRICES - MODIFICATIONS BARSOUM OU RACCORD
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
C IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFL    : FONCTIONS DE FORMES LAGR.
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFCS : COEF_STAB_CONT
C IN  NDEXCL : TABLEAU DES NOEUDS CONCERNES
C OUT MATRCC : MATRICE ELEMENTAIRE LAGR_C/LAGR_C
C OUT MATRCE : MATRICE ELEMENTAIRE LAGR_C/DEPL_E
C OUT MATRCM : MATRICE ELEMENTAIRE LAGR_C/DEPL_M
C OUT MATREC : MATRICE ELEMENTAIRE DEPL_E/LAGR_C
C OUT MATRMC : MATRICE ELEMENTAIRE DEPL_M/LAGR_C
C
C ----------------------------------------------------------------------
C
      INTEGER   INOE,INOM,IDIM,JJ,INOC1,INOC2
      INTEGER   IEXCL
C
C ----------------------------------------------------------------------
C
      DO 10 INOC1 = 1,NNL
        DO 11 INOC2 = 1,NNL
          MATRCC(INOC1,INOC2) = MATRCC(INOC1,INOC2)-
     &                          WPG*JACOBI/COEFCS*
     &                          FFL(INOC2)*FFL(INOC1)
 11     CONTINUE
 10   CONTINUE
C
      DO 20 IEXCL = 1,9   
        IF (NDEXCL(IEXCL).EQ.1) THEN
          MATRCC(IEXCL,IEXCL) = - FFL(IEXCL)*FFL(IEXCL)
        ENDIF  
   20 CONTINUE
C
      DO 30 INOE = 1,NNE
        DO 31 IDIM = 1,NDIM
          JJ = NDIM*(INOE-1)+IDIM 
          DO 32 IEXCL = 1,9   
            IF (NDEXCL(IEXCL).EQ.1) THEN
              MATREC(JJ,IEXCL) = 0.D0
            ENDIF  
   32     CONTINUE           
   31   CONTINUE
   30 CONTINUE
C
      DO 40 INOM = 1,NNM
        DO 41 IDIM = 1,NDIM
          JJ = NDIM*(INOM-1)+IDIM 
          DO 42 IEXCL = 1,9   
            IF (NDEXCL(IEXCL).EQ.1) THEN
              MATRMC(JJ,IEXCL) = 0.D0
            ENDIF  
   42     CONTINUE
   41   CONTINUE
   40 CONTINUE
C
      DO 50 INOM = 1,NNM
        DO 51 IDIM = 1,NDIM
          JJ = NDIM*(INOM-1)+IDIM    
          DO 52 IEXCL = 1,9   
            IF (NDEXCL(IEXCL).EQ.1) THEN
              MATRCM(IEXCL,JJ) = 0.D0
            ENDIF  
   52     CONTINUE        
   51   CONTINUE
   50 CONTINUE
C 
      DO 60 INOE = 1,NNE
        DO 61 IDIM = 1,NDIM
          JJ = NDIM*(INOE-1)+IDIM              
          DO 62 IEXCL = 1,9   
            IF (NDEXCL(IEXCL).EQ.1) THEN
              MATRCE(IEXCL,JJ) = 0.D0
            ENDIF  
   62     CONTINUE        
   61   CONTINUE
   60 CONTINUE
C
      END
