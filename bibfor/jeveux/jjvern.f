      SUBROUTINE JJVERN ( NOML32 , ICRE , IRET )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 13/11/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C TOLE CRP_6
      IMPLICIT NONE
      INCLUDE 'jeveux_private.h'
      CHARACTER *32       NOML32
      INTEGER                      ICRE , IRET
C     ------------------------------------------------------------------
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
      INTEGER          ILLICI , JCLASS(0:255)
      COMMON /JCHAJE/  ILLICI , JCLASS
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C-----------------------------------------------------------------------
      INTEGER JDOCU ,JGENR ,JORIG ,JRNOM ,JTYPE ,K ,N 

C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C     ------------------------------------------------------------------
      CHARACTER *24    NOML24
      CHARACTER *32    NOM32,BLAN32
      CHARACTER *8     NUME     , NOME     , NOMATR   ,  NOML8
      DATA             NUME     , NOME     , NOMATR
     &               /'$$XNUM  ','$$XNOM  ','$$XATR  '/
C DEB ------------------------------------------------------------------
      IRET   = 0
      BLAN32='                                '
      NOM32=BLAN32
      NOMUTI = NOML32
      NOML8  = NOML32(25:32)
      NOML24 = NOML32(1:24)
      IF ( NOML8 .NE. '        ' ) THEN
        IF ( NOML8 .NE. NOME .AND.
     &       NOML8 .NE. NUME .AND.
     &       NOML8 .NE. NOMATR     ) THEN
          CALL U2MESK('F','JEVEUX1_56',1,NOML8)
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
            CALL U2MESK('F','JEVEUX1_57',1,NOML24)
          ENDIF
          DO 10 K = 1,32
            IF( JCLASS(ICHAR( NOM32(K:K) )).EQ. ILLICI ) THEN
              CALL U2MESK('F','JEVEUX1_58',1,NOM32(K:K))
            END IF
 10       CONTINUE
        ENDIF
        IF (NOML8 .NE. '        ' ) THEN
          IF (IRET .EQ. 1 ) THEN
            IF (GENR(JGENR(ICLAOS)+IDATOS) .NE. 'N' ) THEN
              CALL U2MESK('F','JEVEUX1_68',1,NOML24)           
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END
