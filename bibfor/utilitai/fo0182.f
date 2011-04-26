      SUBROUTINE FO0182 ( OBSTAC, NBVAL, VALE )
      IMPLICIT   NONE
      INTEGER             NBVAL
      REAL*8              VALE(*)
      CHARACTER*(*)       OBSTAC
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C
C     OPERATEUR "RECU_FONCTION"  MOT CLE "OBSTACLE"
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER        I, LPRO, LVAL, LXLGUT
      CHARACTER*19   NOMFON
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMFON = OBSTAC
C
C     --- REMPLISSAGE DU .PROL ---
C
      CALL ASSERT(LXLGUT(NOMFON).LE.24)
      CALL WKVECT ( NOMFON//'.PROL', 'G V K24', 6, LPRO )
      ZK24(LPRO)   = 'FONCTION'
      ZK24(LPRO+1) = 'LIN LIN '
      ZK24(LPRO+2) = 'THETA   '
      ZK24(LPRO+3) = 'R       '
      ZK24(LPRO+4) = 'EE      '
      ZK24(LPRO+5) = NOMFON
C
C     --- REMPLISSAGE DU .VALE ---
C
      CALL WKVECT ( NOMFON//'.VALE', 'G V R8', 2*NBVAL, LVAL )
C
      DO 10 I = 1 , NBVAL
         ZR(LVAL+I-1)       = VALE(2*I-1)
         ZR(LVAL+NBVAL+I-1) = VALE(2*I)
 10   CONTINUE
C
      CALL JEDEMA()
      END
