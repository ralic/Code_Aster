      SUBROUTINE RFTABL ( TABRES )
      IMPLICIT   NONE
      CHARACTER*(*)       TABRES
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
C
C     OPERATEUR "RECU_FONCTION"   MOT CLE "TABLE"
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
      INTEGER       IBID, N2,N3,N4, NPARFI, IRET
      INTEGER       IFM, NIV
      REAL*8        R8B
      COMPLEX*16    C16B
      CHARACTER*8   K8B, INTERP, PROLGD
      CHARACTER*16  NOMCMD, TYPCON, PARAX, PARAY
      CHARACTER*19  NOMFON, NEWTAB, NEWTA1
      CHARACTER*24  NOPARA, NOMF
      CHARACTER*24  VALK(2)
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
      CALL GETRES ( NOMFON, TYPCON, NOMCMD )
C
      CALL GETVTX ( ' ', 'PARA_X'       , 0,IARG,1, PARAX , N2 )
      CALL GETVTX ( ' ', 'PARA_Y'       , 0,IARG,1, PARAY , N3 )
      CALL GETVTX ( ' ', 'NOM_PARA_TABL', 0,IARG,1, NOPARA, N4 )
C
      INTERP = 'NON NON '
      PROLGD = 'EE      '
C
      NEWTAB = TABRES
C
C     ------------------------------------------------------------------
C
C                 --- TRAITEMENT DU MOT CLE "FILTRE" ---
C
C     ------------------------------------------------------------------
      CALL GETFAC ( 'FILTRE' , NPARFI )
      IF ( NPARFI .NE. 0 ) THEN
         NEWTA1 = '&&OP0177.FILTRE '
         CALL TBIMFI ( NPARFI, NEWTAB, NEWTA1, IRET )
         IF ( IRET .NE. 0 ) CALL U2MESS('F', 'UTILITAI7_11')
         NEWTAB = NEWTA1
      ENDIF
C     ------------------------------------------------------------------
C
      IF ( N2+N3 .NE. 0 ) THEN
C
        CALL TBEXFO ( NEWTAB, PARAX, PARAY, NOMFON, INTERP, PROLGD,'G')
C
      ELSEIF ( N4 .NE. 0 ) THEN
C
          CALL TBLIVA ( NEWTAB,0,K8B,IBID,R8B,C16B,K8B,K8B,R8B,
     &                  NOPARA,K8B,IBID,R8B,C16B,NOMF,IRET)
          IF (IRET.NE.0) THEN
            VALK(1) = NOPARA
            VALK(2)(1:19) = NEWTAB
            CALL U2MESK('F','MODELISA2_91', 2, VALK)
          ENDIF
          CALL COPISD ( 'FONCTION', 'G', NOMF, NOMFON )
C
      ELSE
         CALL U2MESS('F','UTILITAI4_27')
      ENDIF
C
C
      IF ( NPARFI .NE. 0 )  CALL DETRSD ( 'TABLE' , NEWTA1 )
C
      CALL FOATTR(' ',1,NOMFON)
C
C     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
C         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
      CALL ORDONN(NOMFON,NOMCMD,0)

      CALL TITRE
      IF (NIV.GT.1) CALL FOIMPR(NOMFON,NIV,IFM,0,K8B)

      CALL JEDEMA()
      END
