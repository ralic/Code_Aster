      SUBROUTINE TRMULT(MODSTA,NUMEXI,MAILLA,NEQ,IDDEEQ,PSIDE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      CHARACTER*8   MODSTA,MAILLA
      INTEGER       NUMEXI,NEQ,IDDEEQ
      REAL*8        PSIDE(NEQ)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 22/01/2013   AUTEUR BERRO H.BERRO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8       XNORM, DEPL(6)
      CHARACTER*8   NOMNOE
      CHARACTER*24 MAGRNO, MANONO
      CHARACTER*24 VALK(3)
      CHARACTER*8  KBID
      INTEGER       IBID, IORDR, IER
      REAL*8       R8B, EPSI
      CHARACTER*8   CMP(6), CRIT
      CHARACTER*16 ACCES
      CHARACTER*19 CHAMNO
      COMPLEX*16   C16B
      INTEGER      IARG
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,ID ,IDGN ,IDMST ,IDNO ,II ,IN
      INTEGER IRET ,LDGN ,NB ,NBD ,NBDIR ,NBGR ,NBNO
      INTEGER NBTROU ,NBV
      REAL*8 XD
C-----------------------------------------------------------------------
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
      CALL GETVR8('EXCIT','DIRECTION',NUMEXI,IARG,0,DEPL,NBD)
      NBDIR = -NBD
      CALL GETVR8('EXCIT','DIRECTION',NUMEXI,IARG,NBDIR,DEPL,NBD)
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
     &     NUMEXI,IARG,0,KBID,NBNO)
      IF (NBNO.NE.0) THEN
C        --- ON RECUPERE UNE LISTE DE NOEUD ---
         NBNO = - NBNO
         CALL WKVECT('&&TRMULT.NOEUD','V V K8',NBNO,IDNO)
         CALL GETVEM(MAILLA,'NOEUD','EXCIT','NOEUD',
     &        NUMEXI,IARG,NBNO,ZK8(IDNO),NBV)
      ELSE
C        --- ON RECUPERE UNE LISTE DE GROUP_NO ---
         CALL GETVEM(MAILLA,'GROUP_NO','EXCIT','GROUP_NO',
     &           NUMEXI,IARG,0,KBID,NBGR)
         NBGR = - NBGR
         IF (NBGR.NE.0) THEN
            CALL WKVECT('&&TRMULT.GROUP_NO','V V K24',NBGR,IDGN)
            CALL GETVEM(MAILLA,'GROUP_NO','EXCIT','GROUP_NO',
     &              NUMEXI,IARG,NBGR,ZK24(IDGN),NBV)
C           --- ECLATE LE GROUP_NO EN NOEUD ---
            CALL COMPNO(MAILLA,NBGR,ZK24(IDGN),NBNO)
            CALL WKVECT('&&TRMULT.NOEUD','V V K8',NBNO,IDNO)
            MAGRNO = MAILLA//'.GROUPENO'
            MANONO = MAILLA//'.NOMNOE'
            II = -1
            DO 20 I = 1,NBGR
               CALL JELIRA(JEXNOM(MAGRNO,ZK24(IDGN+I-1)),'LONUTI',
     &          NB,KBID)
               CALL JEVEUO(JEXNOM(MAGRNO,ZK24(IDGN+I-1)),'L',LDGN)
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
               CALL RSEXCH('F',MODSTA,'DEPL',IORDR,CHAMNO,IRET)
               CALL JEVEUO(CHAMNO//'.VALE','L',IDMST)
C
C              --- ON EFFECTUE LE PRODUIT  MODE_STAT * DIR ---
               DO 42 I = 1,NEQ
                  PSIDE(I) = PSIDE(I)+ XD * ZR(IDMST+I-1)
 42            CONTINUE
               CALL JELIBE(CHAMNO//'.VALE')
 40         CONTINUE
         ENDIF
 30   CONTINUE
C
C     --- MISE A ZERO DES DDL DE LAGRANGE
      CALL ZERLAG('R',PSIDE(1),C16B,NEQ,ZI(IDDEEQ))
C
      CALL JEDETR('&&TRMULT.NOEUD')
      CALL JEDETR('&&TRMULT.GROUP_NO')
C
      CALL JEDEMA()
      END
