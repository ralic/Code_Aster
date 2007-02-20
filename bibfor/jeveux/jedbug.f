      SUBROUTINE JEDBUG ( IDB )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 19/02/2007   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IDB
C     ==================================================================
      LOGICAL          LTESJE , LBUGJE
      COMMON /LPARJE/  LTESJE , LBUGJE
      CHARACTER *6     PGMA
      COMMON /KAPPJE/  PGMA
C     ==================================================================
      PGMA = 'JEDBUG'
C     ------------------------------------------------------------------
      IF ( IDB .EQ. 0 ) THEN
         LBUGJE = .FALSE.
         LTESJE = .FALSE.
      ELSE IF ( IDB .EQ. 1 ) THEN
         LBUGJE = .FALSE.
         LTESJE = .TRUE.
      ELSE IF ( IDB .EQ. 2 ) THEN
         LBUGJE = .TRUE.
         LTESJE = .TRUE.
      ENDIF
C     ------------------------------------------------------------------
      IF ( LTESJE ) THEN
        CALL U2MESI ( 'A', 'JEVEUX_34', 1, IDB )
      END IF
C     ------------------------------------------------------------------
      END
