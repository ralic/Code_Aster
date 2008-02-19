      SUBROUTINE RFMGE1 ( MODGEN )
      IMPLICIT NONE
      CHARACTER*(*)       MODGEN
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/02/2008   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     OPERATEUR "RECU_FONCTION"  MOT CLE "RESU_GENE"
C                                CONCEPT MODE_GENE
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      N1, NCMP, IRET, JORDR, LPRO, LVAR, LFON, NBORDR, IM,
     &             IORD, IAD, JVALE, JREFE, JDEEQ, JNUME, NBMODE, I,
     &             ISTRU, IBID, LXLGUT
      REAL*8       EPSI
      CHARACTER*4  INTERP(2)
      CHARACTER*8  K8B, CRIT, MODE
      CHARACTER*14 NUGENE
      CHARACTER*16 K16B, NOMCMD, TYPCON, NOMCHA, NPARA
      CHARACTER*19 NOCH19, NOMFON, KNUME
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETRES ( NOMFON , TYPCON , NOMCMD )
C
      INTERP(1) = 'NON '
      INTERP(2) = 'NON '
C
      CALL GETVTX ( ' ', 'CRITERE'  , 1,1,1, CRIT  , N1 )
      CALL GETVR8 ( ' ', 'PRECISION', 1,1,1, EPSI  , N1 )
      CALL GETVTX ( ' ', 'INTERPOL' , 1,1,2, INTERP, N1 )
      IF ( N1 .EQ. 1 ) INTERP(2) = INTERP(1)
C
      KNUME = '&&RFMGE1.NUME_ORDR'
      CALL RSUTNU ( MODGEN, ' ', 1, KNUME, NBORDR, EPSI, CRIT, IRET )
      IF (IRET.NE.0) THEN
         CALL U2MESS('F','UTILITAI4_11')
      ENDIF
      CALL JEVEUO ( KNUME, 'L', JORDR )
C
C     --- CREATION DE LA FONCTION ---
C
      CALL ASSERT(LXLGUT(NOMFON).LE.24)
      CALL WKVECT ( NOMFON//'.PROL', 'G V K24', 6, LPRO )
      ZK24(LPRO)   = 'FONCTION        '
      ZK24(LPRO+1) = INTERP(1)//INTERP(2)
      ZK24(LPRO+2) = 'FREQ            '
      ZK24(LPRO+4) = 'EE              '
      ZK24(LPRO+5) = NOMFON
C
      CALL WKVECT ( NOMFON//'.VALE', 'G V R', 2*NBORDR, LVAR )
      LFON = LVAR + NBORDR - 1
C
      CALL GETVTX ( ' ', 'NOM_PARA_RESU', 1,1,1, NPARA , N1 )
      IF ( N1 .NE. 0 ) THEN
         ZK24(LPRO+3) = NPARA
         DO 200 IORD = 1 , NBORDR
            CALL RSADPA(MODGEN,'L',1,'FREQ',ZI(JORDR+IORD-1),0,IAD,K8B)
            ZR(LVAR-1+IORD) = ZR(IAD)
            CALL RSADPA(MODGEN,'L',1,NPARA,ZI(JORDR+IORD-1),0,IAD,K8B)
            ZR(LFON-1+IORD) = ZR(IAD)
 200     CONTINUE
         GOTO 9999
      ENDIF
C
      CALL GETVTX ( ' ', 'NOM_CHAM'     , 1,1,1, NOMCHA, N1 )
      CALL GETVIS ( ' ', 'NUME_CMP_GENE', 1,1,1, NCMP  , N1 )
C
      ZK24(LPRO+3) = NOMCHA
C
      DO 100 IORD = 1 , NBORDR
C
         CALL RSEXCH(MODGEN,NOMCHA,ZI(JORDR+IORD-1),NOCH19,IRET)
         IF(IRET.NE.0) THEN
          CALL U2MESS('F','UTILITAI4_12')
         ENDIF
         CALL RSADPA(MODGEN,'L',1,'FREQ',ZI(JORDR+IORD-1),0,IAD,K8B)
         ZR(LVAR+IORD) = ZR(IAD)
C
         CALL JEVEUO(NOCH19//'.VALE','L',JVALE)
         CALL JELIRA(NOCH19//'.VALE','TYPE',IBID,K16B)
         IF ( K16B(1:1) .NE. 'R') THEN
            CALL U2MESS('F','UTILITAI4_17')
         ENDIF
C
         CALL JEVEUO(NOCH19//'.REFE','L',JREFE)
         MODE = ZK24(JREFE)(1:8)
         IF ( MODE .EQ. '        ' ) THEN
            NUGENE = ZK24(JREFE+1)(1:14)
            CALL JEVEUO(NUGENE//'.NUME.DEEQ','L',JDEEQ)
            CALL JEVEUO(NUGENE//'.NUME.NEQU','L',JNUME)
            NBMODE = ZI(JNUME)
            IM = 0
            DO 110 I = 1 , NBMODE
               ISTRU = ZI(JDEEQ+2*(I-1)+2-1)
               IF ( ISTRU .LT. 0 ) GOTO 110
               IM = IM + 1
               IF ( IM .EQ. NCMP ) GOTO 114
 110        CONTINUE
          CALL U2MESS('F','UTILITAI4_14')
 114        CONTINUE
            IM = I
         ELSE
            IM = NCMP
         ENDIF
C
         ZR(LFON+IORD) = ZR(JVALE+IM-1)
C
 100  CONTINUE
C
 9999 CONTINUE
C
      CALL JEDETR( KNUME )
C
      CALL JEDEMA()
      END
