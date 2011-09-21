      SUBROUTINE OP0160()
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     OPERATEUR   IMPR_MACR_ELEM
C     ------------------------------------------------------------------
      INTEGER       VERSIO, N1, IFIC
      LOGICAL       ULEXIS
      CHARACTER*8   FORMAT, MACREL
      CHARACTER*16  FICHIE
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL INFMAJ()
C
      IFIC   = 0
      FICHIE = ' '
C
      CALL GETVID ( ' ', 'MACR_ELEM_DYNA', 1,IARG,1, MACREL, N1 )
C
      CALL GETVTX ( ' ', 'FORMAT', 1,IARG,1, FORMAT, N1 )
C
C     ------------------------------------------------------------------
      IF ( FORMAT .EQ. 'IDEAS' ) THEN

         CALL GETVIS ( ' ', 'VERSION', 1,IARG,1, VERSIO, N1 )

         CALL GETVIS ( ' ', 'UNITE'  , 1,IARG,1, IFIC , N1 )
         IF ( .NOT. ULEXIS( IFIC ) ) THEN
            CALL ULOPEN ( IFIC, ' ', FICHIE, 'NEW', 'O' )
         ENDIF

         CALL IREDSU ( MACREL, FORMAT, IFIC , VERSIO )
C
C     ------------------------------------------------------------------
      ELSEIF ( FORMAT .EQ. 'MISS_3D' ) THEN
         CALL IREDMI ( MACREL)
C
C     ------------------------------------------------------------------
C      ELSEIF ( FORMAT .EQ. 'CADYRO' ) THEN
C         CALL IREDCA ( MACREL )
C
C     ------------------------------------------------------------------
C      ELSEIF ( FORMAT .EQ. 'PLEXUS' ) THEN
C
C         CALL GETVIS ( ' ', 'VERSION', 1,IARG,1, VERSIO, N1 )
C
C         CALL GETVIS ( ' ', 'UNITE'  , 1,1,1, IFIC , N1 )
C         IF ( .NOT. ULEXIS( IFIC ) ) THEN
C            CALL ULOPEN ( IFIC, ' ', FICHIE, 'NEW', 'O' )
C         ENDIF
C
C         CALL IREDPL ( MACREL, IFIC, VERSIO )
C
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF
C
      END
