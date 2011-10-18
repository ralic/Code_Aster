      SUBROUTINE MMMTCM(PHASEP,NDIM  ,NNL   ,NNM   ,NORM  ,
     &                  WPG   ,FFL   ,FFM   ,JACOBI,MATRCM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/10/2011   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*9  PHASEP
      INTEGER      NDIM,NNM,NNL
      REAL*8       FFM(9),FFL(9)
      REAL*8       WPG,JACOBI
      REAL*8       NORM(3)
      REAL*8       MATRCM(9,27)       
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE LAGR_C/DEPL_MAIT 
C
C ----------------------------------------------------------------------
C
C
C IN  PHASEP : PHASE DE CALCUL
C              'CONT'      - CONTACT
C              'CONT_PENA' - CONTACT PENALISE
C              'ADHE'      - ADHERENCE
C              'ADHE_PENA' - ADHERENCE PENALISE
C              'GLIS'      - GLISSEMENT
C              'GLIS_PENA' - GLISSEMENT PENALISE
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE 
C IN  NORM   : NORMALE AU POINT DE CONTACT
C IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
C IN  FFL    : FONCTIONS DE FORMES LAGR.
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C OUT MATRCM : MATRICE ELEMENTAIRE LAGR_C/DEPL_M
C
C ----------------------------------------------------------------------
C
      INTEGER   INOC,INOM,IDIM,JJ
C
C ----------------------------------------------------------------------
C
      IF (PHASEP(1:4).EQ.'CONT') THEN
        DO 200 INOC = 1,NNL
          DO 190 INOM = 1,NNM
            DO 180 IDIM = 1,NDIM
              JJ = NDIM*(INOM-1)+IDIM            
              MATRCM(INOC,JJ) = MATRCM(INOC,JJ) +
     &                       WPG*FFL(INOC)*FFM(INOM)*JACOBI*NORM(IDIM)
  180       CONTINUE
  190     CONTINUE
  200   CONTINUE    
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
