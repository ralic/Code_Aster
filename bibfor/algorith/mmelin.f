      SUBROUTINE MMELIN(NOMA  ,NUMA  ,TYPINT,NNINT )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8   NOMA
      INTEGER       NUMA
      INTEGER       TYPINT
      INTEGER       NNINT
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
C IN  TYPINT : TYPE D'INTEGRATION
C      / 1 'AUTO'    (ON CHOISIT LE SCHEMA LE PLUS ADAPTE)
C      /X2 'GAUSS'   (X EST LE DEGRE DES POLYNOMES DE LEGENDRE)
C      /Y3 'SIMPSON' (Y EST LE NOMBRE DE SUBDIVISIONS)
C      /Z4 'NCOTES'  (Z EST LE DEGRE DU POLYNOME INTERPOLATEUR)
C OUT NNINT  : NOMBRE DE POINTS D'INTEGRATION DE CET ELEMENT
C
C ----------------------------------------------------------------------
C
      CHARACTER*8  ALIAS
      INTEGER      IBID, PARAM
C
C ----------------------------------------------------------------------
C
      CALL MMELTY(NOMA,NUMA,ALIAS,IBID,IBID)
C
C     'AUTO'
      IF (TYPINT .EQ. 1) THEN
         IF (    ALIAS(1:3).EQ.'SE2') THEN
            NNINT = 2
         ELSEIF (ALIAS(1:3).EQ.'SE3') THEN
            NNINT = 3
         ELSEIF (ALIAS(1:3).EQ.'TR3') THEN
            NNINT = 3
         ELSEIF (ALIAS(1:3).EQ.'TR6') THEN
            NNINT = 6
         ELSEIF (ALIAS(1:3).EQ.'TR7') THEN
            NNINT = 6
         ELSEIF (ALIAS(1:3).EQ.'QU4') THEN
            NNINT = 4
         ELSEIF (ALIAS(1:3).EQ.'QU8') THEN
            NNINT = 9
         ELSEIF (ALIAS(1:3).EQ.'QU9') THEN
            NNINT = 9
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF
C
C     'GAUSS'
      ELSEIF (MOD(TYPINT,10) .EQ. 2) THEN
         PARAM = TYPINT/10
         IF (ALIAS(1:2) .EQ. 'SE') THEN
            NNINT = PARAM
         ELSEIF (ALIAS(1:2) .EQ. 'TR') THEN
            IF (PARAM .EQ. 1) THEN
               NNINT = 1
            ELSEIF (PARAM .EQ. 2) THEN
               NNINT = 3
            ELSEIF (PARAM .EQ. 3) THEN
               NNINT = 4
            ELSEIF (PARAM .EQ. 4) THEN
               NNINT = 6
            ELSEIF (PARAM .EQ. 5) THEN
               NNINT = 7
            ELSEIF (PARAM .EQ. 6) THEN
               NNINT = 12
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF
         ELSE IF (ALIAS(1:2) .EQ. 'QU') THEN
            NNINT = PARAM**2
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF
C
C     'SIMPSON'
      ELSE IF (MOD(TYPINT,10) .EQ. 3) THEN
         PARAM = TYPINT/10
         IF (ALIAS(1:2) .EQ. 'SE') THEN
            NNINT = 2*PARAM+1
         ELSEIF (ALIAS(1:2) .EQ. 'TR') THEN
            NNINT = 2*(PARAM**2)+3*PARAM+1
         ELSEIF (ALIAS(1:2) .EQ. 'QU') THEN
            NNINT = (2*PARAM+1)**2
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF
C
C     'NCOTES'
      ELSEIF (MOD(TYPINT,10) .EQ. 4) THEN
         PARAM = TYPINT/10
         IF (ALIAS(1:2) .EQ. 'SE') THEN
            NNINT = PARAM+1
         ELSEIF (ALIAS(1:2) .EQ. 'TR') THEN
            NNINT = (PARAM+1)*(PARAM+2)/2
         ELSEIF (ALIAS(1:2) .EQ. 'QU') THEN
            NNINT = (PARAM+1)**2
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF
C
      END
