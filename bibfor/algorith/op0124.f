      SUBROUTINE OP0124()
      IMPLICIT NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
C
C     COMMANDE:  CREA_RESU
C
C ----------------------------------------------------------------------
      INTEGER        NBFAC
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()

      CALL VE0124 ( )
C
C ----------------------------------------------------------------------
C                   TRAITEMENT DU MOT CLE "ECLA_PG"
C ----------------------------------------------------------------------
C
      CALL GETFAC ( 'ECLA_PG' , NBFAC )
      IF ( NBFAC .GT. 0 ) THEN
         CALL ECLPGR()
         GO TO 9999
      END IF
C
C ----------------------------------------------------------------------
C                   TRAITEMENT DU MOT CLE "PERM_CHAM"
C ----------------------------------------------------------------------
C
      CALL GETFAC ( 'PERM_CHAM' , NBFAC )
      IF ( NBFAC .GT. 0 ) THEN
         CALL CRPERM()
         GO TO 9999
      END IF
C
C ----------------------------------------------------------------------
C               TRAITEMENT DU MOT CLE "PROL_RTZ"
C ----------------------------------------------------------------------
C
      CALL GETFAC ( 'PROL_RTZ' , NBFAC )
      IF ( NBFAC .GT. 0 ) THEN
         CALL CRPROL()
         GO TO 9999
      END IF
C
C ----------------------------------------------------------------------
C               TRAITEMENT DU MOT CLE "AFFE"
C ----------------------------------------------------------------------
C
      CALL GETFAC ( 'AFFE', NBFAC )
      IF ( NBFAC .GT. 0 ) THEN
         CALL CRTYPE()
         GO TO 9999
      END IF
C
C ----------------------------------------------------------------------
C               TRAITEMENT DU MOT CLE "ASSE"
C ----------------------------------------------------------------------
C
      CALL GETFAC ( 'ASSE', NBFAC )
      IF ( NBFAC .GT. 0 ) THEN
         CALL CRASSE()
         GO TO 9999
      END IF
C
C ----------------------------------------------------------------------
C               TRAITEMENT DU MOT CLE "PREP_VRC1"
C ----------------------------------------------------------------------
C
      CALL GETFAC ( 'PREP_VRC1', NBFAC )
      IF ( NBFAC .GT. 0 ) THEN
         CALL CRVRC1()
         GO TO 9999
      END IF
C
C ----------------------------------------------------------------------
C               TRAITEMENT DU MOT CLE "PREP_VRC2"
C ----------------------------------------------------------------------
C
      CALL GETFAC ( 'PREP_VRC2', NBFAC )
      IF ( NBFAC .GT. 0 ) THEN
         CALL CRVRC2()
         GO TO 9999
      END IF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
