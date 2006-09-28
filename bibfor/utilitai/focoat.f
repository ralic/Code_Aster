      SUBROUTINE FOCOAT (NOMFON, NBFON, NOPARA, NORESU, INTERP, PROLGD)
      IMPLICIT NONE
      INTEGER                    NBFON
      CHARACTER*(*)      NOMFON(*)
      CHARACTER*16                      NOPARA, NORESU, INTERP, PROLGD
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     POUR LA COMBINAISON LINEAIRE DE FONCTION,
C     DETERMINE LES ATTRIBUTS DE LA FONCTION EN SORTIE
C     ----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      IOCC, LPRO
      CHARACTER*1  PROLG, PROLD
      CHARACTER*24 PROL
      LOGICAL      PROLGE, PROLDE, PROLGC, PROLDC, PROLGL, PROLDL
      LOGICAL      INTNON, INTLIN, INTLIL, INTLOG, INTLOL
C     ----------------------------------------------------------------
C
      CALL JEMARQ()
      PROL(20:24) = '.PROL'
C
      PROL(1:19) = NOMFON(1)
      CALL JEVEUO(PROL,'L',LPRO)
      NOPARA = ZK16(LPRO+2)
      NORESU = ZK16(LPRO+3)
C
      INTNON = .FALSE.
      INTLIN = .FALSE.
      INTLIL = .FALSE.
      INTLOG = .FALSE.
      INTLOL = .FALSE.
      DO 10 IOCC = 1, NBFON
         PROL(1:19) = NOMFON(IOCC)
         CALL JEVEUO(PROL,'L',LPRO)
         IF ( ZK16(LPRO+1)(1:3) .EQ. 'NON' ) THEN
            INTNON = .TRUE.
         ELSEIF ( ZK16(LPRO+1)(1:3) .EQ. 'INT' ) THEN
            INTNON = .TRUE.
         ELSEIF ( ZK16(LPRO+1) .EQ. 'LIN LIN ' ) THEN
            INTLIN = .TRUE.
         ELSEIF ( ZK16(LPRO+1) .EQ. 'LIN LOG ' ) THEN
            INTLIL = .TRUE.
         ELSEIF ( ZK16(LPRO+1) .EQ. 'LOG LOG ' ) THEN
            INTLOG = .TRUE.
         ELSEIF ( ZK16(LPRO+1) .EQ. 'LOG LIN ' ) THEN
            INTLOL = .TRUE.
         ENDIF
 10   CONTINUE
C
      IF ( INTNON ) THEN
         INTERP = 'NON NON '
      ELSEIF ( INTLIN .AND. ( .NOT.INTLIL .AND. .NOT.INTLOG
     &                                    .AND. .NOT.INTLOL )  ) THEN
         INTERP = 'LIN LIN '
      ELSEIF ( INTLIL .AND. ( .NOT.INTLIN .AND. .NOT.INTLOG
     &                                    .AND. .NOT.INTLOL )  ) THEN
         INTERP = 'LIN LOG '
      ELSEIF ( INTLOG .AND. ( .NOT.INTLIN .AND. .NOT.INTLIL
     &                                    .AND. .NOT.INTLOL )  ) THEN
         INTERP = 'LOG LOG '
      ELSEIF ( INTLOL .AND. ( .NOT.INTLIN .AND. .NOT.INTLIL
     &                                    .AND. .NOT.INTLOG )  ) THEN
         INTERP = 'LOG LIN '
      ELSE
         CALL U2MESS('A','UTILITAI_83')
         INTERP = 'LIN LIN '
      ENDIF
C
      PROLGE = .FALSE.
      PROLDE = .FALSE.
      PROLGC = .FALSE.
      PROLDC = .FALSE.
      PROLGL = .FALSE.
      PROLDL = .FALSE.
      DO 20 IOCC = 1, NBFON
         PROL(1:19) = NOMFON(IOCC)
         CALL JEVEUO(PROL,'L',LPRO)
         IF ( ZK16(LPRO+4)(1:1) .EQ. 'E' ) THEN
            PROLGE = .TRUE.
         ELSEIF ( ZK16(LPRO+4)(1:1) .EQ. 'C' ) THEN
            PROLGC = .TRUE.
         ELSEIF ( ZK16(LPRO+4)(1:1) .EQ. 'L' ) THEN
            PROLGL = .TRUE.
         ENDIF
         IF ( ZK16(LPRO+4)(2:2) .EQ. 'E' ) THEN
            PROLDE = .TRUE.
         ELSEIF ( ZK16(LPRO+4)(2:2) .EQ. 'C' ) THEN
            PROLDC = .TRUE.
         ELSEIF ( ZK16(LPRO+4)(2:2) .EQ. 'L' ) THEN
            PROLDL = .TRUE.
         ENDIF
 20   CONTINUE
C
      IF ( PROLGE ) THEN
         PROLG = 'E'
      ELSEIF ( PROLGC ) THEN
         PROLG = 'C'
      ELSEIF ( PROLGL ) THEN
         PROLG = 'L'
      ELSE
         PROLG = 'E'
      ENDIF
C
      IF ( PROLDE ) THEN
         PROLD = 'E'
      ELSEIF ( PROLDC ) THEN
         PROLD = 'C'
      ELSEIF ( PROLDL ) THEN
         PROLD = 'L'
      ELSE
         PROLD = 'E'
      ENDIF
C
      PROLGD = PROLG//PROLD//'      '
C
      CALL JEDEMA()
      END
