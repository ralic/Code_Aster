      SUBROUTINE TRMULT(MODSTA,NUMEXI,MAILLA,NEQ,IDDEEQ,PSIDE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8   MODSTA,MAILLA
      INTEGER       NUMEXI,NEQ,IDDEEQ
      REAL*8        PSIDE(NEQ)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 19/10/2010   AUTEUR DELMAS J.DELMAS 
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
C     OPERATEUR :   DYNA_TRAN_MODAL
C
C     CREE ET CALCULE LE VECTEUR PSI*DIRECTION DANS LE CAS D'UN CALCUL
C     SISMIQUE D UNE STRUCTURE MULTI-SUPPORTEE
C     ------------------------------------------------------------------
C IN  : MODSTA : NOM DU CONCEPT MODES STATIQUES
C IN  : NUMEXI : NUMERO D'OCCURENCE DU MOT CLE EXCIT
C IN  : MAILLA : NOM DU MAILLAGE
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : IDDEEQ : INDICE DE L'EQUATION
C OUT : PSIDE  : VALEURS DU VECTEUR PSI*DELTA
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
      CHARACTER*32 JEXNUM, JEXNOM, JEXR8, JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      REAL*8       XNORM, DEPL(6)
      CHARACTER*8  NOMGR, NOMNOE
      CHARACTER*24 MAGRNO, MANONO, DEEQ
      CHARACTER*24 VALK(3)
      CHARACTER*8  KBID
      INTEGER      LMAT, IBID, IORDR, IER
      REAL*8       R8B, EPSI
      CHARACTER*8  K8B, CMP(6), CRIT
      CHARACTER*16 ACCES
      CHARACTER*19 CHAMNO
      COMPLEX*16   C16B
C     ------------------------------------------------------------------
      DATA CMP / 'DX' , 'DY' , 'DZ' , 'DRX' , 'DRY' , 'DRZ' /
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C     --- RECUPERATION DES ARGUMENTS DE LA COMMANDE ---
C
      CALL JEMARQ()
      EPSI = 1.D-4
      MAGRNO = ' '
      MANONO = ' '
      IER=0
C
C     --- RECUPERATION DE LA DIRECTION SISMIQUE  ---
C
      CALL GETVR8('EXCIT','DIRECTION',NUMEXI,1,0,DEPL,NBD)
      NBDIR = -NBD
      CALL GETVR8('EXCIT','DIRECTION',NUMEXI,1,NBDIR,DEPL,NBD)
C     --- ON NORMALISE LE VECTEUR ---
      XNORM = 0.D0
      DO 10 I = 1,NBDIR
         XNORM = XNORM + DEPL(I) * DEPL(I)
 10   CONTINUE
      XNORM = SQRT(XNORM)
      IF (XNORM.LT.0.D0) THEN
         CALL U2MESS('F','ALGORITH9_81')
      ENDIF
      DO 12 I = 1,NBDIR
         DEPL(I) = DEPL(I) / XNORM
 12   CONTINUE
C
C     --- RECUPERATION DES POINTS D'ANCRAGE ---
C
      CALL GETVEM(MAILLA,'NOEUD','EXCIT','NOEUD',
     &     NUMEXI,1,0,KBID,NBNO)
      IF (NBNO.NE.0) THEN
C        --- ON RECUPERE UNE LISTE DE NOEUD ---
         NBNO = - NBNO
         CALL WKVECT('&&TRMULT.NOEUD','V V K8',NBNO,IDNO)
         CALL GETVEM(MAILLA,'NOEUD','EXCIT','NOEUD',
     &        NUMEXI,1,NBNO,ZK8(IDNO),NBV)
      ELSE
C        --- ON RECUPERE UNE LISTE DE GROUP_NO ---
         CALL GETVEM(MAILLA,'GROUP_NO','EXCIT','GROUP_NO',
     &           NUMEXI,1,0,KBID,NBGR)
         NBGR = - NBGR
         IF (NBGR.NE.0) THEN
            CALL WKVECT('&&TRMULT.GROUP_NO','V V K8',NBGR,IDGN)
            CALL GETVEM(MAILLA,'GROUP_NO','EXCIT','GROUP_NO',
     &              NUMEXI,1,NBGR,ZK8(IDGN),NBV)
C           --- ECLATE LE GROUP_NO EN NOEUD ---
            CALL COMPNO(MAILLA,NBGR,ZK8(IDGN),NBNO)
            CALL WKVECT('&&TRMULT.NOEUD','V V K8',NBNO,IDNO)
            MAGRNO = MAILLA//'.GROUPENO'
            MANONO = MAILLA//'.NOMNOE'
            II = -1
            DO 20 I = 1,NBGR
               CALL JELIRA(JEXNOM(MAGRNO,ZK8(IDGN+I-1)),'LONUTI',
     &          NB,KBID)
               CALL JEVEUO(JEXNOM(MAGRNO,ZK8(IDGN+I-1)),'L',LDGN)
               DO 22 IN = 0,NB-1
                  CALL JENUNO(JEXNUM(MANONO,ZI(LDGN+IN)),NOMNOE)
                  II = II + 1
                  ZK8(IDNO+II) = NOMNOE
 22            CONTINUE
 20         CONTINUE
         ENDIF
      ENDIF
C
C     --- CALCUL DU VECTEUR PSI*DELTA ---
C
      CALL R8INIR(NEQ,0.D0,PSIDE,1)
      DO 30 ID = 1,NBDIR
         XD = DEPL(ID)
         IF (ABS(XD).GT.EPSI) THEN
            DO 40 IN = 1,NBNO
               ACCES(1:8 ) = ZK8(IDNO+IN-1)
               ACCES(9:16) = CMP(ID)
C
C              --- ON RECUPERE LE MODE STATIQUE ASSOCIE AU NOEUD ---
               CALL RSORAC(MODSTA,'NOEUD_CMP',IBID,R8B,ACCES,C16B,EPSI,
     &                     CRIT,IORDR,1,NBTROU)
               IF (NBTROU.NE.1) THEN
                  IER = IER + 1
                  VALK (1) = ACCES(1:8)
                  VALK (2) = ACCES(9:16)
                  CALL U2MESG('F', 'ALGELINE4_61',2,VALK,0,0,0,0.D0)
                  GOTO 40
               ENDIF
               CALL RSVPAR(MODSTA,IORDR,'TYPE_DEFO',IBID,R8B,
     &                                  'DEPL_IMPO',IRET)
               IF (IRET.NE.100) THEN
                  IER = IER + 1
                  VALK (1) = 'MODE_MECA'
                  VALK (2) = ACCES(1:8)
                  VALK (3) = ACCES(9:16)
                  CALL U2MESG('F', 'ALGELINE4_62',3,VALK,0,0,0,0.D0)
                  GOTO 40
               ENDIF
               CALL RSEXCH(MODSTA,'DEPL',IORDR,CHAMNO,IRET)
               IF (IRET.NE.0) THEN
                  IER = IER + 1
                  VALK (1) = CHAMNO
                  VALK (2) = ACCES(1:8)
                  VALK (3) = ACCES(9:16)
                  CALL U2MESG('E', 'ALGELINE4_63',3,VALK,0,0,0,0.D0)
                  GOTO 40
               ELSE
                  CALL JEVEUO(CHAMNO//'.VALE','L',IDMST)
C
C                 --- ON EFFECTUE LE PRODUIT  MODE_STAT * DIR ---
                  DO 42 I = 1,NEQ
                     PSIDE(I) = PSIDE(I)+ XD * ZR(IDMST+I-1)
 42               CONTINUE
                  CALL JELIBE(CHAMNO//'.VALE')
               ENDIF
 40         CONTINUE
         ENDIF
 30   CONTINUE
C
C     --- MISE A ZERO DES DDL DE LAGRANGE
      CALL ZERLAG(PSIDE(1),NEQ,ZI(IDDEEQ))
C
      CALL JEDETR('&&TRMULT.NOEUD')
      CALL JEDETR('&&TRMULT.GROUP_NO')
C
      CALL JEDEMA()
      END
