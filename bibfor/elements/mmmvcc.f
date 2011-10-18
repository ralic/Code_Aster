      SUBROUTINE MMMVCC(PHASEP,NNL   ,WPG   ,FFL   ,JACOBI,
     &                  JEU   ,COEFAC,DLAGRC,VECTCC)
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
      REAL*8       WPG,FFL(9),JACOBI,DLAGRC
      REAL*8       COEFAC,JEU
      REAL*8       VECTCC(9)
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
C IN  PHASEP : PHASE DE CALCUL
C              'SANS' - PAS DE CONTACT
C              'CONT' - CONTACT
C              'SANS_PENA' - PENALISATION - PAS DE CONTACT
C              'CONT_PENA' - PENALISATION - CONTACT
C IN  NNL    : NOMBRE DE NOEUDS LAGRANGE
C IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFL    : FONCTIONS DE FORMES LAGRANGES
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  JEU    : VALEUR DU JEU
C IN  COEFAC : COEF_AUGM_CONT
C IN  DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
C OUT VECTCC : VECTEUR ELEMENTAIRE LAGR_C
C
C ----------------------------------------------------------------------
C
      INTEGER   INOC
C
C ----------------------------------------------------------------------
C
      IF (PHASEP.EQ.'SANS') THEN
        DO 61 INOC = 1,NNL
          VECTCC(INOC) = VECTCC(INOC) -
     &                   WPG*FFL(INOC)*DLAGRC*JACOBI/COEFAC
 61     CONTINUE
      ELSEIF (PHASEP.EQ.'SANS_PENA') THEN
        DO 64 INOC = 1,NNL
          VECTCC(INOC) = VECTCC(INOC) -
     &                   WPG*FFL(INOC)*DLAGRC*JACOBI/COEFAC
 64     CONTINUE
      ELSEIF (PHASEP.EQ.'CONT_PENA') THEN
        DO 63 INOC = 1,NNL
          VECTCC(INOC) = VECTCC(INOC) -
     &                   WPG*FFL(INOC)*DLAGRC*JACOBI/COEFAC -
     &                   WPG*FFL(INOC)*JEU*JACOBI 
 63     CONTINUE
      ELSEIF (PHASEP.EQ.'CONT') THEN
        DO 62 INOC = 1,NNL      
          VECTCC(INOC) = VECTCC(INOC)-
     &                   WPG*FFL(INOC)*JEU*JACOBI
 62     CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
