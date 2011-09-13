      SUBROUTINE MMELIN(NOMA,NUMA,TYPINT,NNINT)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/09/2011   AUTEUR MASSIN P.MASSIN 
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
C                X2 GAUSS (X est l'ordre de la quadrature)
C                Y3 SIMPSON (Y est nombre de partitions du domaine)
C		         Z4 NEWTON-COTES (Z est dégré du polynôme interpolateur)
C OUT NNINT  : NOMBRE DE POINTS D'INTEGRATION DE CET ELEMENT
C
C ----------------------------------------------------------------------
C
      INTEGER      IBID, PARAM
      CHARACTER*8  ALIAS
C
C ----------------------------------------------------------------------
C
      CALL MMELTY(NOMA,NUMA,ALIAS,IBID,IBID)
C
      IF (TYPINT .EQ. 1) THEN
        IF (ALIAS(1:3) .EQ. 'SE2') THEN
            NNINT = 2
        ELSE IF (ALIAS(1:3) .EQ. 'SE3') THEN
            NNINT = 3
        ELSE IF (ALIAS(1:3) .EQ. 'TR3') THEN
            NNINT = 3
        ELSE IF ((ALIAS(1:3).EQ.'TR6').OR.(ALIAS(1:3).EQ.'TR7')) THEN
            NNINT = 6
        ELSE IF (ALIAS(1:3) .EQ. 'QU4') THEN
            NNINT = 4
        ELSE IF ((ALIAS(1:3).EQ.'QU8')) THEN
            NNINT = 8
        ELSE IF ((ALIAS(1:3).EQ.'QU9')) THEN
            NNINT = 9
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
      ELSE IF (MOD(TYPINT,10) .EQ. 2) THEN
        PARAM = TYPINT/10
        IF (ALIAS(1:2) .EQ. 'SE') THEN
            IF ((ALIAS(3:3) .EQ. '3').AND.(PARAM .LE. 2)) THEN
                NNINT = 3
            ELSE
                NNINT = PARAM
            END IF
        ELSE IF (ALIAS(1:2) .EQ. 'TR') THEN
            IF (PARAM .EQ. 1) THEN
                NNINT = 1
            ELSE IF (PARAM .EQ. 2) THEN
                NNINT = 3
            ELSE IF (PARAM .EQ. 3) THEN
                NNINT = 4
            ELSE IF (PARAM .EQ. 4) THEN
                NNINT = 6
            ELSE IF (PARAM .EQ. 5) THEN
                NNINT = 7
            ELSE IF (PARAM .EQ. 6) THEN
                NNINT = 12
            ELSE IF (PARAM .EQ. 7) THEN
                NNINT = 13
            ELSE IF (PARAM .EQ. 8) THEN
                NNINT = 16
            ELSE IF (PARAM .EQ. 9) THEN
                NNINT = 19
            ELSE IF (PARAM .EQ. 10) THEN
                NNINT = 25
            ELSE
                CALL ASSERT(.FALSE.)
            END IF
        ELSE IF (ALIAS(1:2) .EQ. 'QU') THEN
            NNINT = PARAM**2
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
      ELSE IF (MOD(TYPINT,10) .EQ. 3) THEN
        PARAM = TYPINT/10
        IF (ALIAS(1:2) .EQ. 'SE') THEN
            NNINT = 2*PARAM+1
        ELSE IF (ALIAS(1:2) .EQ. 'TR') THEN
            NNINT = 2*(PARAM**2)+3*PARAM+1
        ELSE IF (ALIAS(1:2) .EQ. 'QU') THEN
            NNINT = (2*PARAM+1)**2
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
      ELSE IF (MOD(TYPINT,10) .EQ. 4) THEN
        PARAM = TYPINT/10
        IF (ALIAS(1:2) .EQ. 'SE') THEN
            NNINT = PARAM+1
        ELSE IF (ALIAS(1:2) .EQ. 'TR') THEN
            NNINT = (PARAM+1)*(PARAM+2)/2
        ELSE IF (ALIAS(1:2) .EQ. 'QU') THEN
            NNINT = (PARAM+1)**2
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
      END
