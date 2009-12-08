      SUBROUTINE XMELIN(TYPMA,TYPINT,NNINT)
C      
      IMPLICIT NONE
      CHARACTER*8 TYPMA
      INTEGER     TYPINT
      INTEGER     NNINT
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/05/2009   AUTEUR MAZET S.MAZET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT GRAND GLISSEMENTS (METHODE XFEM - UTILITAIRE)
C
C RETOURNE LE NOMBRE DE POINTS D'INTEGRATION POUR UN ELEMENT DE CONTACT
C SUIVANT LE TYPE DE SCHEMA D'INTEGRATION
C      
C ----------------------------------------------------------------------
C
C
C IN  TYPMA  : NOM DU TYPE DE MAILLE
C IN  TYPINT : TYPE SCHEMA INTEGRATION
C                 1 NOEUDS
C                 2 GAUSS
C                 3 SIMPSON
C                 4 SIMPSON_1
C                 5 SIMPSON_2
C                 6 NEWTON-COTES
C                 7 NEWTON-COTES_1
C                 8 NEWTON-COTES_2
C                 12 FPG2 (GAUSS 2 POINTS)
C                 13 FPG3
C                 14 FPG4
C                 16 FPG6
C                 17 FPG7
C OUT NNINT  : NOMBRE DE POINTS D'INTEGRATION DE CET ELEMENT
C
C ----------------------------------------------------------------------
C
C
C ----------------------------------------------------------------------
C
C
      IF (TYPINT .EQ. 1) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 2
        IF (TYPMA(1:3) .EQ. 'SE3') NNINT = 3
        IF (TYPMA(1:3) .EQ. 'TR3') NNINT = 3
        IF (TYPMA(1:3) .EQ. 'TR6') NNINT = 6
      ELSEIF (TYPINT .EQ. 2) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 2
        IF (TYPMA(1:3) .EQ. 'SE3') NNINT = 2
        IF (TYPMA(1:3) .EQ. 'TR3') NNINT = 3
        IF (TYPMA(1:3) .EQ. 'TR6') NNINT = 6
      ELSEIF (TYPINT .EQ. 3) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 3
        IF (TYPMA(1:3) .EQ. 'SE3') NNINT = 3
        IF (TYPMA(1:3) .EQ. 'TR3') NNINT = 6
        IF (TYPMA(1:3) .EQ. 'TR6') NNINT = 6
      ELSEIF (TYPINT .EQ. 4) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 5
        IF (TYPMA(1:3) .EQ. 'SE3') NNINT = 5
        IF (TYPMA(1:3) .EQ. 'TR3') NNINT = 15
        IF (TYPMA(1:3) .EQ. 'TR6') NNINT = 15
      ELSEIF (TYPINT .EQ. 5) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 9
        IF (TYPMA(1:3) .EQ. 'SE3') NNINT = 9
        IF (TYPMA(1:3) .EQ. 'TR3') NNINT = 42
        IF (TYPMA(1:3) .EQ. 'TR6') NNINT = 42
      ELSEIF (TYPINT .EQ. 6) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 4
        IF (TYPMA(1:3) .EQ. 'SE3') NNINT = 4
        IF (TYPMA(1:3) .EQ. 'TR3') NNINT = 4
        IF (TYPMA(1:3) .EQ. 'TR6') NNINT = 4
      ELSEIF (TYPINT .EQ. 7) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 5
        IF (TYPMA(1:3) .EQ. 'SE3') NNINT = 5
        IF (TYPMA(1:3) .EQ. 'TR3') NNINT = 6
        IF (TYPMA(1:3) .EQ. 'TR6') NNINT = 6
      ELSEIF (TYPINT .EQ. 8) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 10
        IF (TYPMA(1:3) .EQ. 'SE3') NNINT = 10
        IF (TYPMA(1:3) .EQ. 'TR3') NNINT = 42
        IF (TYPMA(1:3) .EQ. 'TR6') NNINT = 42
      ELSEIF (TYPINT .EQ. 12) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 2
      ELSEIF (TYPINT .EQ. 13) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 3
      ELSEIF (TYPINT .EQ. 14) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 4
      ELSEIF (TYPINT .EQ. 16) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 6
      ELSEIF (TYPINT .EQ. 17) THEN
        IF (TYPMA(1:3) .EQ. 'SE2') NNINT = 7
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
      END
