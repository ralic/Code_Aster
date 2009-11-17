      SUBROUTINE MMELIN(NOMA,NUMA,TYPINT,NNINT)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/11/2009   AUTEUR DESOZA T.DESOZA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8 NOMA
      INTEGER     NUMA
      INTEGER     TYPINT
      INTEGER     NNINT
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C RETOURNE LE NOMBRE DE POINTS D'INTEGRATION POUR UN ELEMENT DE CONTACT
C SUIVANT LE TYPE DE SCHEMA D'INTEGRATION
C      
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  NUMA   : NUMERO ABSOLU DE LA MAILLE
C IN  TYPINT : TYPE SCHEMA INTEGRATION
C                 1 NOEUDS
C                 2 GAUSS
C                 3 SIMPSON
C                 4 SIMPSON_1
C                 5 SIMPSON_2
C                 6 NEWTON-COTES
C                 7 NEWTON-COTES_1
C                 8 NEWTON-COTES_2
C OUT NNINT  : NOMBRE DE POINTS D'INTEGRATION DE CET ELEMENT
C
C ----------------------------------------------------------------------
C
      INTEGER      IBID
      CHARACTER*8  ALIAS
C
C ----------------------------------------------------------------------
C
      CALL MMELTY(NOMA,NUMA,ALIAS,IBID,IBID)
C
      IF (TYPINT .EQ. 1) THEN
        IF (ALIAS(1:3) .EQ. 'SE2') NNINT = 2
        IF (ALIAS(1:3) .EQ. 'SE3') NNINT = 3
        IF (ALIAS(1:3) .EQ. 'TR3') NNINT = 3
        IF (ALIAS(1:3) .EQ. 'TR6') NNINT = 6
        IF (ALIAS(1:3) .EQ. 'QU4') NNINT = 4
        IF (ALIAS(1:3) .EQ. 'QU8') NNINT = 9
        IF (ALIAS(1:3) .EQ. 'QU9') NNINT = 9
      ELSEIF (TYPINT .EQ. 2) THEN
        IF (ALIAS(1:3) .EQ. 'SE2') NNINT = 2
        IF (ALIAS(1:3) .EQ. 'SE3') NNINT = 3
        IF (ALIAS(1:3) .EQ. 'TR3') NNINT = 3
        IF (ALIAS(1:3) .EQ. 'TR6') NNINT = 6
        IF (ALIAS(1:3) .EQ. 'QU4') NNINT = 4
        IF (ALIAS(1:3) .EQ. 'QU8') NNINT = 9
        IF (ALIAS(1:3) .EQ. 'QU9') NNINT = 9
      ELSEIF (TYPINT .EQ. 3) THEN
        IF (ALIAS(1:3) .EQ. 'SE2') NNINT = 3
        IF (ALIAS(1:3) .EQ. 'SE3') NNINT = 3
        IF (ALIAS(1:3) .EQ. 'TR3') NNINT = 6
        IF (ALIAS(1:3) .EQ. 'TR6') NNINT = 6
        IF (ALIAS(1:3) .EQ. 'QU4') NNINT = 9
        IF (ALIAS(1:3) .EQ. 'QU8') NNINT = 9
        IF (ALIAS(1:3) .EQ. 'QU9') NNINT = 9
      ELSEIF (TYPINT .EQ. 4) THEN
        IF (ALIAS(1:3) .EQ. 'SE2') NNINT = 5
        IF (ALIAS(1:3) .EQ. 'SE3') NNINT = 5
        IF (ALIAS(1:3) .EQ. 'TR3') NNINT = 15
        IF (ALIAS(1:3) .EQ. 'TR6') NNINT = 15
        IF (ALIAS(1:3) .EQ. 'QU4') NNINT = 21
        IF (ALIAS(1:3) .EQ. 'QU8') NNINT = 21
        IF (ALIAS(1:3) .EQ. 'QU9') NNINT = 21
      ELSEIF (TYPINT .EQ. 5) THEN
        IF (ALIAS(1:3) .EQ. 'SE2') NNINT = 9
        IF (ALIAS(1:3) .EQ. 'SE3') NNINT = 9
        IF (ALIAS(1:3) .EQ. 'TR3') NNINT = 42
        IF (ALIAS(1:3) .EQ. 'TR6') NNINT = 42
        IF (ALIAS(1:3) .EQ. 'QU4') NNINT = 65
        IF (ALIAS(1:3) .EQ. 'QU8') NNINT = 65
        IF (ALIAS(1:3) .EQ. 'QU9') NNINT = 65
      ELSEIF (TYPINT .EQ. 6) THEN
        IF (ALIAS(1:3) .EQ. 'SE2') NNINT = 4
        IF (ALIAS(1:3) .EQ. 'SE3') NNINT = 4
        IF (ALIAS(1:3) .EQ. 'TR3') NNINT = 4
        IF (ALIAS(1:3) .EQ. 'TR6') NNINT = 4
        IF (ALIAS(1:3) .EQ. 'QU4') NNINT = 16
        IF (ALIAS(1:3) .EQ. 'QU8') NNINT = 16
        IF (ALIAS(1:3) .EQ. 'QU9') NNINT = 16
      ELSEIF (TYPINT .EQ. 7) THEN
        IF (ALIAS(1:3) .EQ. 'SE2') NNINT = 5
        IF (ALIAS(1:3) .EQ. 'SE3') NNINT = 5
        IF (ALIAS(1:3) .EQ. 'TR3') NNINT = 6
        IF (ALIAS(1:3) .EQ. 'TR6') NNINT = 6
        IF (ALIAS(1:3) .EQ. 'QU4') NNINT = 25
        IF (ALIAS(1:3) .EQ. 'QU8') NNINT = 25
        IF (ALIAS(1:3) .EQ. 'QU9') NNINT = 25
      ELSEIF (TYPINT .EQ. 8) THEN
        IF (ALIAS(1:3) .EQ. 'SE2') NNINT = 10
        IF (ALIAS(1:3) .EQ. 'SE3') NNINT = 10
        IF (ALIAS(1:3) .EQ. 'TR3') NNINT = 42
        IF (ALIAS(1:3) .EQ. 'TR6') NNINT = 42
        IF (ALIAS(1:3) .EQ. 'QU4') NNINT = 100
        IF (ALIAS(1:3) .EQ. 'QU8') NNINT = 100
        IF (ALIAS(1:3) .EQ. 'QU9') NNINT = 100
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
      END
