      SUBROUTINE FOCAIN ( METHOD, NOMFON, CSTE, SORTIE ,BASE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       METHOD, NOMFON,       SORTIE
      CHARACTER*1                                       BASE
      REAL*8                              CSTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C     ------------------------------------------------------------------
C     INTEGRATION D'UNE FONCTION
C IN  METHOD : K : METHODE D'INTEGRATION
C           TRAPEZE   : DISPONIBLE
C           SIMPSON    : DISPONIBLE
C           VILLARCEAU : NON DISPONIBLE
C           HARDY      : NON DISPONIBLE
C     ------------------------------------------------------------------
C     RAPPEL DES FORMULES PARTICULIERES DE NEWTON-COTES (PAS CONSTANT)
C
C     NOM DE LA FORMULE: N :    A   A0   A1   A2   A3   A4   A5   A6
C     -----------------------------------------------------------
C     TRAPEZES         : 1 :    2    1    1   --   --   --   --   --
C     SIMPSON          : 2 :    6    1    4    1   --   --   --   --
C     VILLARCEAU       : 4 :   45   14   64   24   64   14   --   --
C     HARDY            : 6 :  140   41  216   27  272   27  216   41
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*16 NOMRES, TYPRES
      CHARACTER*19 NOMFI, NOMFS
      CHARACTER*24 VALE, PROL
      CHARACTER*1 K1BID
C     ----------------------------------------------------------------
C
      CALL JEMARQ()
      NOMFI = NOMFON
      NOMFS = SORTIE
C
C     ---  NOMBRE DE POINTS ----
      CALL GETTCO(NOMFI,TYPRES)
      IF (TYPRES.EQ.'FORMULE') THEN
          CALL U2MESK('F','MODELISA2_5',1,NOMFI)
      ENDIF
      VALE = NOMFI//'.VALE'
      CALL JELIRA(VALE,'LONUTI',NBVAL,K1BID)
      CALL JEVEUO(VALE,'L',LVAR)
      NBPTS = NBVAL/2
      LFON  = LVAR + NBPTS
C
      IF (NBPTS.GE.2) THEN
C
C       --- CREATION DU TABLEAU DES VALEURS ---
        CALL WKVECT(NOMFS//'.VALE',BASE//' V R',NBVAL,LRES)
C
C       --- RECOPIE DES VARIABLES ---
        DO 410 I = 0 , NBPTS-1
           ZR(LRES+I) = ZR(LVAR+I)
  410   CONTINUE
        LRES = LRES + NBPTS
C
C       --- INTEGRATION ---
        IF (METHOD.EQ.'SIMPSON') THEN
           CALL FOC2IN(METHOD,NBPTS,ZR(LVAR),ZR(LFON),CSTE,ZR(LRES))
        ELSE IF ( METHOD .EQ. 'TRAPEZE' .OR. METHOD .EQ. '  ' ) THEN
           CALL FOC2IN(METHOD,NBPTS,ZR(LVAR),ZR(LFON),CSTE,ZR(LRES))
        ELSE
           CALL U2MESS('F','UTILITAI_82')
        ENDIF
      ELSE IF (NBPTS.EQ.1) THEN
C
C       --- CREATION DU TABLEAU DES VALEURS ---
        CALL WKVECT(NOMFS//'.VALE',BASE//' V R',4,LRES)
C
C       --- RECOPIE DES VARIABLES ---
        ZR(LRES)   = ZR(LVAR)
        ZR(LRES+1) = ZR(LVAR)+1.D0
        ZR(LRES+2) = 0.D0
        ZR(LRES+3) = ZR(LVAR+1)
      ENDIF
C
C     --- AFFECTATION DU .PROL ---
      PROL = NOMFI//'.PROL'
      CALL JEVEUO(PROL,'L',LPRO)
      NOMRES = ZK24(LPRO+3)
      IF ( NOMRES(1:4) .EQ. 'ACCE' ) THEN
         NOMRES = 'VITE'
      ELSEIF ( NOMRES(1:4) .EQ. 'VITE' ) THEN
         NOMRES = 'DEPL'
      ELSE
         NOMRES      = 'TOUTRESU'
      ENDIF
      PROL = NOMFS//'.PROL'
      CALL ASSERT(LXLGUT(NOMFS).LE.24)
      CALL WKVECT(PROL,'G V K24',6,LPROS)
      ZK24(LPROS  ) = 'FONCTION'
      IF (ZK24(LPRO+1)(1:3).EQ.'INT') THEN
         ZK24(LPROS+1) = 'LIN LIN '
      ELSE
         ZK24(LPROS+1) = ZK24(LPRO+1)
      ENDIF
      ZK24(LPROS+2) = ZK24(LPRO+2)
      ZK24(LPROS+3) = NOMRES
      IF (ZK24(LPRO+4)(1:1).EQ.'I' .OR. ZK24(LPRO+4)(2:2).EQ.'I') THEN
         ZK24(LPROS+4) = 'EE      '
      ELSE
         ZK24(LPROS+4) = ZK24(LPRO+4)
      ENDIF
C
      ZK24(LPROS+5) = NOMFS
      CALL JEDEMA()
      END
