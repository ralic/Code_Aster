      SUBROUTINE JJVERN ( NOML32 , ICRE , IRET )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_6
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER *32       NOML32
      INTEGER                      ICRE , IRET
C     ------------------------------------------------------------------
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
      INTEGER          ILLICI , JCLASS(0:255)
      COMMON /JCHAJE/  ILLICI , JCLASS
C     ------------------------------------------------------------------
      CHARACTER *75    CMESS
      CHARACTER *24    NOML24
      CHARACTER *32    NOM32
      CHARACTER *8     NUME     , NOME     , NOMATR   ,  NOML8
      DATA             NUME     , NOME     , NOMATR
     &               /'$$XNUM  ','$$XNOM  ','$$XATR  '/
C DEB ------------------------------------------------------------------
      IRET   = 0
      NOMUTI = NOML32
      NOML8  = NOML32(25:32)
      NOML24 = NOML32(1:24)
      IF ( NOML8 .NE. '        ' ) THEN
        IF ( NOML8 .NE. NOME .AND.
     &       NOML8 .NE. NUME .AND.
     &       NOML8 .NE. NOMATR     ) THEN
          CMESS  = 'LONGUEUR OU STRUCTURE DU NOM INCORRECTE'
          CALL U2MESK('S','JEVEUX_01',1,CMESS)
        ENDIF
      END IF
      IF (NOML24       .EQ. NOMOS(1:24) .AND.
     &    NOMOS(25:32) .EQ. '        '       ) THEN
        IRET = 1
      ELSE IF ( NOML24 .EQ. NOMCO(1:24) ) THEN
        IRET = 2
      ELSE
        NOM32 = NOML24
C ----- RECHERCHE DU NOM DANS LES BASES PAR JJCREN
C
        CALL JJCREN ( NOM32 , ICRE , IRET )
C ----- VALIDITE DES CARACTERES COMPOSANT LE NOM
C
        IF(IRET.NE.0 .AND. ICRE .NE. 0 ) THEN
          IF ( INDEX ( NOML24 , '$' ) .NE. 0 ) THEN
            CMESS  = 'LONGUEUR OU STRUCTURE DU NOM INCORRECTE'
            CALL U2MESK('S','JEVEUX_01',1,CMESS)
          ENDIF
          DO 10 K = 1,32
            IF( JCLASS(ICHAR( NOM32(K:K) )).EQ. ILLICI ) THEN
              CMESS = 'LE CARACTERE "'//NOM32(K:K)//'" EST ILLICITE'
              CALL U2MESK('S','JEVEUX_01',1,CMESS)
            END IF
 10       CONTINUE
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END
