      SUBROUTINE OP0162()
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     INTERFACE ASTER - MISS3D : PROCEDURE  IMPR_MISS_3D
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
C
      INTEGER          IERD, GD
      INTEGER      ULISOP
      CHARACTER*8  K8B, RESU, MASSE, NOMA, VECT, TYPI
      CHARACTER*14 NUME
      CHARACTER*16 CONCEP, NOMCMD, K16NOM
      CHARACTER*19 FONC
      CHARACTER*24  REFE, VALE, DEEQ, TYPE
      CHARACTER*24   KBID, NOMCH0
      CHARACTER*8  MAEL, BASEMO, NOMFON, INTERF
      CHARACTER*80 TITRE
      REAL*8        PETIR8, DI(6)
      REAL*8       TINI, TFIN, FINI, FFIN, PAS
C     ------------------------------------------------------------------
      DATA  REFE  /'                  _REFE'/
      DATA  VALE  /'                   .VALE'/
      DATA  NOMCH0 /'&&OP0162.CHAMNO'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETRES( RESU , CONCEP , NOMCMD )
C
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
      PETIR8 = 1.D-40
C
C     ----- RECUPERATION UNITE DE MISS ---
      CALL GETVIS(' ','UNITE',1,1,1,IFMIS,NU)
      K16NOM = ' '
      IF ( ULISOP ( IFMIS, K16NOM ) .EQ. 0 )  THEN
        CALL ULOPEN ( IFMIS,' ',' ','NEW','O')
      ENDIF
C
C     ----- RECUPERATION DES MODES -----
      CALL GETVID(' ','MACR_ELEM_DYNA',1,1,1,MAEL,NMM)
      REFE(1:18) = MAEL//'.MAEL_MASS'
      CALL JEVEUO(REFE,'L',JREFE)
      BASEMO = ZK24(JREFE)
      MASSE = ZK24(JREFE+1)
C
C     --- ON RECUPERE LE TYPE D'INTERFACE ---
C
      CALL JEVEUO(BASEMO(1:8)//'           .REFD','L',JVAL)
      INTERF = ZK24(JVAL+4) (1:8)
      IF (INTERF.NE.' ') THEN
C       CALL BMNBMD(BASEMO,'MODE',NBMODE)
C       CALL BMNBMD(BASEMO,'DEFORMEE',NBMODS)
       CALL DISMOI('F','NB_MODES_STA',BASEMO,'RESULTAT',NBMODS,K8B,IERD)
       CALL DISMOI('F','NB_MODES_DYN',BASEMO,'RESULTAT',NBMODE,K8B,IERD)
       TYPE = INTERF//'.IDC_TYPE'
       CALL JEVEUO(TYPE,'L',JTYP)
       TYPI = ZK8(JTYP)
      ELSE
C       CALL JEVEUO(BASEMO//'           .UTIL','L',JVAL)
C       NBMODE = ZI(JVAL+2)
C       NBMODS = ZI(JVAL+3)
       CALL DISMOI('F','NB_MODES_STA',BASEMO,'RESULTAT',NBMODS,K8B,IERD)
       CALL DISMOI('F','NB_MODES_DYN',BASEMO,'RESULTAT',NBMODE,K8B,IERD)
       TYPI = 'CRAIGB'
      ENDIF
      NBMODT = NBMODE + NBMODS
C
      WRITE(IFM,'(1X,I6,1X,''MODES DYNAMIQUES'',1X,A8)') NBMODE,TYPI
      WRITE(IFM,'(1X,I6,1X,''MODES STATIQUES'',2X,A8)') NBMODS,TYPI
      WRITE(IFMIS,'(''DYNA'',1X,I6,1X,A8)') NBMODE,TYPI
      WRITE(IFMIS,'(''STAT'',1X,I6,1X,A8)') NBMODS,TYPI
C
      CALL GETVTX(' ','TITRE',1,1,1,TITRE,NTI)
      IF (NTI.NE.0) WRITE(IFMIS,'(''TITRE'',/A80)') TITRE
      IF (NTI.NE.0) WRITE(IFM,'(A80)') TITRE
C
C
C--------RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
C
      CALL DISMOI('F','NOM_NUME_DDL',MASSE,'MATR_ASSE',IBID ,NUME,IERD)
      CALL DISMOI('F','NB_EQUA',MASSE,'MATR_ASSE',NEQ,KBID,IRET)
      DEEQ = NUME//'.NUME.DEEQ'
      CALL JEVEUO(DEEQ,'L',IDDEEQ)
      CALL DISMOI('F','NOM_MAILLA',MASSE,'MATR_ASSE',IBID ,NOMA,IERD)
      CALL DISMOI('F','NB_NO_MAILLA',NOMA ,'MAILLAGE' ,NBNOEU,K8B,IERD)
      CALL DISMOI('F','NUM_GD_SI'   ,NUME ,'NUME_DDL' ,GD    ,K8B,IERD)
      IF (INTERF.EQ.' ') CALL VTCREB(NOMCH0, NUME, 'V', 'R', NEQ)
C
      CALL WKVECT('&&OP0162.VECTASS1','V V R',NEQ,IDVEC1)
      CALL WKVECT('&&OP0162.VECTASS2','V V R',NEQ,IDVEC2)
C
C     ----- RECUPERATION INSTANTS OU FREQUENCES ---
      CALL GETVR8(' ','INST_INIT',1,1,1,TINI,N1)
      CALL GETVR8(' ','INST_FIN',1,1,1,TFIN,N2)
      CALL GETVR8(' ','FREQ_INIT',1,1,1,FINI,N3)
      CALL GETVR8(' ','FREQ_FIN',1,1,1,FFIN,N4)
      CALL GETVR8(' ','PAS',1,1,1,PAS,N)
      IF (N1.NE.0) THEN
        WRITE(IFMIS,'(''TEMPS DE'',1X,1PE12.5,1X,''A'',1X,1PE12.5,
     &  1X,''PAS'',1X,1PE12.5)') TINI,TFIN,PAS
        WRITE(IFM,'(''TEMPS DE'',1X,1PE12.5,1X,''A'',1X,1PE12.5,
     &  1X,''PAS'',1X,1PE12.5)') TINI,TFIN,PAS
      ELSE
        WRITE(IFMIS,'(''FREQ DE'',1X,1PE12.5,1X,''A'',1X,1PE12.5,
     &  1X,''PAS'',1X,1PE12.5)') FINI,FFIN,PAS
        WRITE(IFM,'(''FREQ DE'',1X,1PE12.5,1X,''A'',1X,1PE12.5,
     &  1X,''PAS'',1X,1PE12.5)') FINI,FFIN,PAS
      ENDIF
      IC=0
      CALL GETVR8(' ','DIRE_ONDE',IC,1,3,DI(1),N)
      IF (N.NE.0) THEN
         WRITE(IFMIS,'(''DIRE ONDE'')')
         WRITE(IFMIS,'(3(1X,1PE12.5))') (DI(I),I=1,3)
      ENDIF
C     ----- RECUPERATION NIVEAU IMPRESSION ---
C
      CALL GETFAC('EXCIT',NCHARB)
      WRITE(IFMIS,'(''CHARGE'',1X,I6)') NCHARB
      WRITE(IFM,'(''CHARGE'',1X,I6)') NCHARB
      IF (NCHARB.EQ.0) GOTO 9998
      IF (NBMODE.EQ.0) THEN
       CALL WKVECT('&&OP0162.DVECT','V V R',1,JVECT)
      ELSE
       CALL WKVECT('&&OP0162.DVECT','V V R',NBMODE,JVECT)
      ENDIF
      IF (NBMODS.EQ.0) THEN
       CALL WKVECT('&&OP0162.SVECT','V V R',1,ISVECT)
      ELSE
       CALL WKVECT('&&OP0162.SVECT','V V R',NBMODS,ISVECT)
      ENDIF
      CALL WKVECT('&&OP0162.BASEMO','V V R',NBMODT*NEQ,IDBASE)
      IF (INTERF.NE.' ') THEN
        CALL COPMOD(BASEMO,'DEPL',NEQ,NUME,NBMODT,ZR(IDBASE))
      ELSE
        CALL COPMO2(BASEMO,NEQ,NUME,NBMODT,ZR(IDBASE))
      ENDIF
C
C --- ALLOCATION VECTEUR DE TRAVAIL
C
      CALL WKVECT('&&OP0162.VECT1','V V R',NEQ,IADMO1)
      CALL WKVECT('&&OP0162.VECT2','V V R',NEQ,IADMO2)
      DO 70 IC = 1,NCHARB
         CALL GETVID('EXCIT','VECT_ASSE',IC,1,1,VECT,N)
         IF(N.NE.0)THEN
            CALL CHPVER('F',VECT,'NOEU','DEPL_R',IERD)
         ENDIF
         VALE(1:8) = VECT
         CALL JEVEUO(VALE,'L',IDVEC1)
         DO 71 J = 1,NBMODE
C
C-----      RECOPIE DU JEME MODE
C
            CALL DCOPY(NEQ,ZR(IDBASE+(J-1)*NEQ),1,ZR(IDVEC2),1)
C
C-----       MISE A ZERO DES DDLS DE LAGRANGE
C
            CALL ZERLAG(ZR(IDVEC2),NEQ,ZI(IDDEEQ))
C
C-----       PRODUIT SCALAIRE VECTASS * MODE
C
            PIJ = DDOT(NEQ,ZR(IDVEC1),1,ZR(IDVEC2),1)
            ZR(JVECT+J-1) = PIJ + PETIR8
  71     CONTINUE
         DO 72 J = 1,NBMODS
C
C-----      RECOPIE DU JEME MODE
C
            J2 = J + NBMODE
            CALL DCOPY(NEQ,ZR(IDBASE+(J2-1)*NEQ),1,ZR(IDVEC2),1)
C
C-----       MISE A ZERO DES DDLS DE LAGRANGE
C
            CALL ZERLAG(ZR(IDVEC2),NEQ,ZI(IDDEEQ))
C
C-----       PRODUIT SCALAIRE VECTASS * MODE
C
            PIJ = DDOT(NEQ,ZR(IDVEC1),1,ZR(IDVEC2),1)
            ZR(ISVECT+J-1) = PIJ + PETIR8
  72     CONTINUE
         IF (NIV.GT.1) WRITE(IFM,'(''DYNA CHAR'',1X,I6)') IC
         IF (NIV.GT.1) WRITE(IFM,'(6(1X,1PE12.5))')
     +    (ZR(JVECT+K-1),K=1,NBMODE)
         IF (NIV.GT.1) WRITE(IFM,'(''STAT CHAR'',1X,I6)') IC
         IF (NIV.GT.1) WRITE(IFM,'(6(1X,1PE12.5))')
     +    (ZR(ISVECT+K-1),K=1,NBMODS)
         WRITE(IFMIS,'(''DYNA CHAR'',1X,I6)') IC
         WRITE(IFMIS,'(6(1X,1PE12.5))')
     +    (ZR(JVECT+K-1),K=1,NBMODE)
         WRITE(IFMIS,'(''STAT CHAR'',1X,I6)') IC
         WRITE(IFMIS,'(6(1X,1PE12.5))')
     +    (ZR(ISVECT+K-1),K=1,NBMODS)
         CALL GETVID('EXCIT','FONC_MULT',IC,1,1,NOMFON,NFO)
         IF (NFO.NE.0) GOTO 80
         COEF = 1.D0
         T = 0.D0
         CALL GETVR8('EXCIT','COEF_MULT',IC,1,1,COEF,NC)
         NBVAL = 1
         IF (NIV.GT.1) WRITE(IFM,'(''FONC CHAR'',1X,I6,
     +    ''VALE'',1X,I6)') IC,NBVAL
         IF (NIV.GT.1) WRITE(IFM,'(6(1X,1PE12.5))')
     +    T, COEF
         WRITE(IFMIS,'(''FONC CHAR'',2(1X,I6))') IC,NBVAL
         WRITE(IFMIS,'(6(1X,1PE12.5))')
     +    T, COEF
         GOTO 81
  80     CONTINUE
         FONC = NOMFON
         CALL JELIRA(FONC//'.VALE','LONMAX',NBVAL,KBID)
         CALL JEVEUO(FONC//'.VALE','L',JFONC)
         NBVAL = NBVAL/2
         IF (NIV.GT.1) WRITE(IFM,'(''FONC CHAR'',1X,I6,
     +    ''VALE'',1X,I6)') IC,NBVAL
         IF (NIV.GT.1) WRITE(IFM,'(6(1X,1PE12.5))')
     +    (ZR(JFONC+K-1),ZR(JFONC+K+NBVAL-1),K=1,NBVAL)
         WRITE(IFMIS,'(''FONC CHAR'',2(1X,I6))') IC,NBVAL
         WRITE(IFMIS,'(6(1X,1PE12.5))')
     +    (ZR(JFONC+K-1),ZR(JFONC+K+NBVAL-1),K=1,NBVAL)
  81  CONTINUE
  70  CONTINUE
9998  CONTINUE
      CALL GETFAC('EXCIT_SOL',NCHARS)
      WRITE(IFMIS,'(''SOLS'',1X,I6)') NCHARS
      WRITE(IFM,'(''SOLS'',1X,I6)') NCHARS
      IF (NCHARS.EQ.0) GOTO 9999
      DO 73 IC = 1,NCHARS
         CALL GETVR8('EXCIT_SOL','DIRECTION',IC,1,3,DI(1),N)
         CALL GETVTX('EXCIT_SOL','NOM_CHAM',IC,1,1,TYPI,N)
         IF (NIV.GT.1) WRITE(IFM,'(''DIRE SOLS'',1X,I6)') IC
         IF (NIV.GT.1) WRITE(IFM,'(3(1X,1PE12.5))') (DI(I),I=1,3)
         IF (NIV.GT.1) WRITE(IFM,'(''SOLS'',1X,I6,1X,
     +    ''TYPE'',1X,A8)') IC,TYPI
         WRITE(IFMIS,'(''DIRE SOLS'',1X,I6)') IC
         WRITE(IFMIS,'(3(1X,1PE12.5))') (DI(I),I=1,3)
         WRITE(IFMIS,'(''TYPE SOLS'',1X,I6,1X,A8)') IC,TYPI
         CALL GETVID('EXCIT_SOL','FONC_SIGNAL',IC,1,1,NOMFON,N)
         FONC = NOMFON
         CALL JELIRA(FONC//'.VALE','LONMAX',NBVAL,KBID)
         CALL JEVEUO(FONC//'.VALE','L',JFONC)
         NBVAL = NBVAL/2
         IF (NIV.GT.1) WRITE(IFM,'(''FONC SOLS'',1X,I6,1X,
     +    ''VALE'',1X,I6)') IC,NBVAL
         IF (NIV.GT.1) WRITE(IFM,'(6(1X,1PE12.5))')
     +    (ZR(JFONC+K-1),ZR(JFONC+K+NBVAL-1),K=1,NBVAL)
         WRITE(IFMIS,'(''FONC SOLS'',2(1X,I6))') IC,NBVAL
         WRITE(IFMIS,'(6(1X,1PE12.5))')
     +    (ZR(JFONC+K-1),ZR(JFONC+K+NBVAL-1),K=1,NBVAL)
   73 CONTINUE
 9999 CONTINUE
C
      CALL GETFAC('SOURCE_SOL',NSOURS)
      IF (NSOURS.EQ.0) GOTO 9995
      WRITE(IFMIS,'(''SOUS'',1X,I6)') NSOURS
      WRITE(IFM,'(''SOUS'',1X,I6)') NSOURS
      DO 74 IC = 1,NSOURS
C         IC = 1
         CALL GETVR8('SOURCE_SOL','POINT',IC,1,3,DI(1),N)
         CALL GETVR8('SOURCE_SOL','DIRECTION',IC,1,3,DI(4),N)
         CALL GETVTX('SOURCE_SOL','NOM_CHAM',IC,1,1,TYPI,N)
         IF (NIV.GT.1) WRITE(IFM,'(''DIRE SOUS'',1X,I6)') IC
         IF (NIV.GT.1) WRITE(IFM,'(3(1X,1PE12.5))') (DI(I),I=1,6)
         IF (NIV.GT.1) WRITE(IFM,'(''SOLS'',1X,I6,1X,
     +    ''TYPE'',1X,A8)') IC,TYPI
         WRITE(IFMIS,'(''DIRE SOUS'',1X,I6)') IC
         WRITE(IFMIS,'(3(1X,1PE12.5))') (DI(I),I=1,6)
         WRITE(IFMIS,'(''TYPE SOUS'',1X,I6,1X,A8)') IC,TYPI
         CALL GETVID('SOURCE_SOL','FONC_SIGNAL',IC,1,1,NOMFON,N)
         FONC = NOMFON
         CALL JELIRA(FONC//'.VALE','LONMAX',NBVAL,KBID)
         CALL JEVEUO(FONC//'.VALE','L',JFONC)
         NBVAL = NBVAL/2
         IF (NIV.GT.1) WRITE(IFM,'(''FONC SOUS'',1X,I6,1X,
     +    ''VALE'',1X,I6)') IC,NBVAL
         IF (NIV.GT.1) WRITE(IFM,'(6(1X,1PE12.5))')
     +    (ZR(JFONC+K-1),ZR(JFONC+K+NBVAL-1),K=1,NBVAL)
         WRITE(IFMIS,'(''FONC SOUS'',2(1X,I6))') IC,NBVAL
         WRITE(IFMIS,'(6(1X,1PE12.5))')
     +    (ZR(JFONC+K-1),ZR(JFONC+K+NBVAL-1),K=1,NBVAL)
   74 CONTINUE
 9995 CONTINUE
C
      CALL GETFAC('SOURCE_FLUIDE',NSOURF)
      IF (NSOURF.EQ.0) GOTO 9996
      WRITE(IFMIS,'(''SOUF'',1X,I6)') NSOURF
      WRITE(IFM,'(''SOUF'',1X,I6)') NSOURF
      DO 75 IC = 1,NSOURF
C         IC = 1
         CALL GETVR8('SOURCE_FLUIDE','POINT',IC,1,3,DI(1),N)
         CALL GETVTX('SOURCE_FLUIDE','NOM_CHAM',IC,1,1,TYPI,N)
         IF (NIV.GT.1) WRITE(IFM,'(''DIRE SOUF'',1X,I6)') IC
         IF (NIV.GT.1) WRITE(IFM,'(3(1X,1PE12.5))') (DI(I),I=1,3)
         IF (NIV.GT.1) WRITE(IFM,'(''SOLS'',1X,I6,1X,
     +    ''TYPE'',1X,A8)') IC,TYPI
         WRITE(IFMIS,'(''DIRE SOUF'',1X,I6)') IC
         WRITE(IFMIS,'(3(1X,1PE12.5))') (DI(I),I=1,3)
         WRITE(IFMIS,'(''TYPE SOUF'',1X,I6,1X,A8)') IC,TYPI
         CALL GETVID('SOURCE_FLUIDE','FONC_SIGNAL',IC,1,1,NOMFON,N)
         FONC = NOMFON
         CALL JELIRA(FONC//'.VALE','LONMAX',NBVAL,KBID)
         CALL JEVEUO(FONC//'.VALE','L',JFONC)
         NBVAL = NBVAL/2
         IF (NIV.GT.1) WRITE(IFM,'(''FONC SOUF'',1X,I6,1X,
     +    ''VALE'',1X,I6)') IC,NBVAL
         IF (NIV.GT.1) WRITE(IFM,'(6(1X,1PE12.5))')
     +    (ZR(JFONC+K-1),ZR(JFONC+K+NBVAL-1),K=1,NBVAL)
         WRITE(IFMIS,'(''FONC SOUF'',2(1X,I6))') IC,NBVAL
         WRITE(IFMIS,'(6(1X,1PE12.5))')
     +    (ZR(JFONC+K-1),ZR(JFONC+K+NBVAL-1),K=1,NBVAL)
   75 CONTINUE
 9996 CONTINUE
C
      CALL JEDEMA()
      END
