      SUBROUTINE CALCFO ( COMPL, NOMFIN, NOMFON, NBVAL, VALE, NOPARA )
      IMPLICIT   NONE
      INTEGER             NBVAL
      REAL*8              VALE(*)
      LOGICAL             COMPL
      CHARACTER*16        NOPARA
      CHARACTER*19        NOMFIN, NOMFON
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/05/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CREATION DU SD FONCTION A PARTIR D'UNE FORMULE (FONCTION )
C     ------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IER, NBVAL2, LVALE, LFON, IVAL, LPROL
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
C     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.VALE ---
C
      IF ( COMPL ) THEN
         NBVAL2 = 3*NBVAL
      ELSE
         NBVAL2 = 2*NBVAL
      ENDIF
C
      CALL WKVECT ( NOMFON//'.VALE', 'G V R', NBVAL2, LVALE )
      LFON = LVALE + NBVAL
      DO 10 IVAL = 0, NBVAL-1
         ZR(LVALE+IVAL) = VALE(IVAL+1)
         IF ( COMPL ) THEN
            CALL FOINTC( NOMFIN, 1, NOPARA, ZR(LVALE+IVAL),
     &                   ZR(LFON+2*IVAL), ZR(LFON+2*IVAL+1), IER )
            IF (IER.NE.0) THEN
                CALL U2MESS('F','UTILITAI_8')
            ENDIF
         ELSE
            CALL FOINTE ( 'F ', NOMFIN, 1, NOPARA,
     &                          ZR(LVALE+IVAL), ZR(LFON+IVAL), IER )
         ENDIF
 10   CONTINUE
C
C     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL ---
C
      CALL WKVECT ( NOMFON//'.PROL', 'G V K16', 5, LPROL )
      IF ( COMPL ) THEN
         ZK16(LPROL)   = 'FONCT_C         '
      ELSE
         ZK16(LPROL)   = 'FONCTION        '
      ENDIF
      ZK16(LPROL+1) = 'LIN LIN         '
      ZK16(LPROL+2) = NOPARA
      ZK16(LPROL+3) = 'TOUTRESU        '
      ZK16(LPROL+4) = 'EE              '
C
      CALL JEDEMA()
      END
