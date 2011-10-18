      SUBROUTINE MMMTCC(PHASEP,NNL   ,WPG   ,FFL   ,JACOBI,
     &                  COEFAC,MATRCC)
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
      INTEGER      NNL
      REAL*8       WPG,FFL(9),JACOBI
      REAL*8       COEFAC
      REAL*8       MATRCC(9,9)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE LAGR_C/LAGR_C
C
C ----------------------------------------------------------------------
C
C
C IN  PHASEP : PHASE DE CALCUL
C              'SANS' - PAS DE CONTACT
C              'CONT' - CONTACT
C              'SANS_PENA' - PENALISATION - PAS DE CONTACT
C              'CONT_PENA' - PENALISATION - CONTACT
C IN  NNL    : NOMBRE DE NOEUDS LAGRANGE 
C IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFL    : FONCTIONS DE FORMES LAGR.
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFAC : COEF_AUGM_CONT
C OUT MATRCC : MATRICE ELEMENTAIRE LAGR_C/LAGR_C
C
C ----------------------------------------------------------------------
C
      INTEGER   INOC1,INOC2
C
C ----------------------------------------------------------------------
C

      IF (PHASEP.EQ.'SANS') THEN
        DO 61 INOC1 = 1,NNL
          DO 51 INOC2 = 1,NNL
            MATRCC(INOC1,INOC2) = MATRCC(INOC1,INOC2)-
     &                    WPG*JACOBI/COEFAC*
     &                    FFL(INOC2)*FFL(INOC1)
 51       CONTINUE
 61     CONTINUE
      ELSEIF (PHASEP.EQ.'SANS_PENA') THEN
        DO 62 INOC1 = 1,NNL
          DO 52 INOC2 = 1,NNL
            MATRCC(INOC1,INOC2) = MATRCC(INOC1,INOC2)-
     &                    WPG*JACOBI/COEFAC*
     &                    FFL(INOC2)*FFL(INOC1)
 52       CONTINUE
 62     CONTINUE      
      ELSEIF (PHASEP.EQ.'CONT_PENA') THEN
        DO 63 INOC1 = 1,NNL
          DO 53 INOC2 = 1,NNL
            MATRCC(INOC1,INOC2) = MATRCC(INOC1,INOC2)-
     &                    WPG*JACOBI/COEFAC*
     &                    FFL(INOC2)*FFL(INOC1)
 53       CONTINUE
 63     CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
