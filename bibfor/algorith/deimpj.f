      SUBROUTINE DEIMPJ(ITESTC,ITEST0,TETAJ,TESTC)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/05/2000   AUTEUR KXBADNG T.KESTENS 
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
C-----------------------------------------------------------------------
C DESCRIPTION : DETERMINATION DU POINT D'IMPLICITATION DU JACOBIEN
C -----------
C               APPELANT : MDCHOE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER  ITESTC, ITEST0
      REAL*8   TETAJ
      INTEGER  TESTC
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      IF ( (ITEST0.NE.0).OR.(ITESTC.NE.0) ) THEN
C
         IF ( (ITEST0.EQ.0).AND.(ITESTC.EQ.-1) ) THEN
            TETAJ = 0.0D0
            TESTC = 0
         ELSE IF ( (ITEST0.EQ.0).AND.(ITESTC.EQ.1) ) THEN
            TETAJ = 1.0D0
            TESTC = 1
         ELSE IF ( (ITEST0.EQ.-1).AND.(ITESTC.EQ.0) ) THEN
            TETAJ = 1.0D0
            TESTC = 0
         ELSE IF ( (ITEST0.EQ.-1).AND.(ITESTC.EQ.1) ) THEN
            TETAJ = 1.0D0
            TESTC = 1
         ELSE IF ( (ITEST0.EQ.1).AND.(ITESTC.EQ.0) ) THEN
            TETAJ = 0.0D0
            TESTC = 1
         ELSE IF ( (ITEST0.EQ.1).AND.(ITESTC.EQ.-1) ) THEN
            TETAJ = 0.0D0
            TESTC = 1
         ELSE IF ( (ITEST0.EQ.1).AND.(ITESTC.EQ.1) ) THEN
            TETAJ = 1.0D0
            TESTC = 1
         ENDIF
C
      ENDIF
C
C --- FIN DE DEIMPJ.
      END
