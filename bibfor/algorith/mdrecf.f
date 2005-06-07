      SUBROUTINE MDRECF (NBEXCI,IDESCF,NOMFON,COEFM,
     &                   IADVEC,INUMOR,FONDEP,FONVIT,FONACC,PSDEL,
     &                   NEQ,TYPBAS,BASEMO,NBMODE,RIGGEN,NOMMOT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            NBEXCI,NBPAS,NEQ, NBMODE
      INTEGER            IDESCF(*),INUMOR(*),IADVEC(*)
      REAL*8             COEFM(*),RIGGEN(NBMODE)
      REAL*8             PSDEL(NEQ,NBEXCI)
      CHARACTER*8        NOMFON(2*NBEXCI)
      CHARACTER*8        FONDEP(2*NBEXCI),FONVIT(2*NBEXCI)
      CHARACTER*8        FONACC(2*NBEXCI)
      CHARACTER*8        BASEMO, NOMMOT
      CHARACTER*16       TYPBAS
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/01/2004   AUTEUR NICOLAS O.NICOLAS 
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
C
C     CALCULE LES FORCES EXTERIEURES A CHAQUE PAS DE TEMPS
C     ------------------------------------------------------------------
C IN  : NBEXCI  : NOMBRE DE SUPPORTS INDEPENDANTS
C IN  : IDESCF  :
C IN  : NOMFON  : NOM DE LA FONCTION EXCITATION
C IN  : COEFM   :
C IN  : IADVEC  :
C IN  : INUMOR  :
C OUT : FONDEP  : NOM DE LA FONCTION DEPLACEMENT
C OUT : FONVIT  : NOM DE LA FONCTION VITESSE
C OUT : FONACC  : NOM DE LA FONCTION ACCELERATION 
C OUT : PSDEL  : VECTEUR DES PSI*DELTA OU CORRECTIONS MODALES
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : BASEMO : NOM DE LA BASE MODALE DE PROJECTION
C OUT : NOMMOT :OUI SI MULTI APPUIS OU CORRECTION MODALE
C ----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
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
C
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      PARAMETER     ( MXPARA = 10 )
      INTEGER       I, J, K, IPT, NBPF, NBPT,IER
      REAL*8        UN, T, ALPHA
      REAL*8        COEF
      CHARACTER*8   K8BID, INTERP, PROLGD, MODSTA, MODCOR
      CHARACTER*8   MATASS, MAILLA, MONMOT(2),BLANC
      CHARACTER*14  NUMDDL
      CHARACTER*16  NOMCMD, NOMP(MXPARA)
      CHARACTER*19  CHANNO, FONCT, FACCE
      CHARACTER*19  CHAMNO, CHAMN2
      CHARACTER*24  DEEQ
      LOGICAL       LFORC
C     ------------------------------------------------------------------
      DATA BLANC    /'        '/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IER = 0
C ---    CALCUL TRANSITOIRE CLASSIQUE
      IF ((TYPBAS(1:9).EQ.'MODE_MECA').OR.
     +    (TYPBAS(1:9).EQ.'MODE_STAT')) THEN
         CALL JEVEUO(BASEMO//'           .REFE','L',JDRIF)
         MATASS =  ZK24(JDRIF)(1:8)
         IF (MATASS.EQ.BLANC) MATASS =  ZK24(JDRIF+2)(1:8)
         CALL DISMOI('F','NOM_MAILLA'  ,MATASS,'MATR_ASSE',
     +          IB,MAILLA,IER)
         CALL DISMOI('F','NOM_NUME_DDL',MATASS,'MATR_ASSE',
     +         IBID,NUMDDL,IER)
         DEEQ = NUMDDL//'.NUME.DEEQ'
         CALL JEVEUO(DEEQ,'L',IDDEEQ)
      ELSEIF (TYPBAS(1:9).EQ.'BASE_MODA') THEN
         CALL JEVEUO(BASEMO//'           .REFE','L',JDRIF)
         NUMDDL = ZK24(JDRIF+1)(1:14)
         CALL DISMOI('F','NOM_MAILLA',NUMDDL,'NUME_DDL',
     +          IB,MAILLA,IER)
         DEEQ = NUMDDL//'.NUME.DEEQ'
         CALL JEVEUO(DEEQ,'L',IDDEEQ)
      ENDIF
      NOMMOT = 'NON'
CC
      DO 10 I=1,NBEXCI
        CALL GETVIS('EXCIT','NUME_MODE',I,1,1,INUM,NF)
        CALL GETVID('EXCIT','VECT_GENE',I,1,1,CHANNO,L1)
        CALL GETVID('EXCIT','FONC_MULT',I,1,1,FONCT,N1)
        CALL GETVR8('EXCIT','COEF_MULT',I,1,1,ALPHA,M1)
        CALL GETVID('EXCIT','ACCE',I,1,1,FACCE,NA)
        CALL GETVTX('EXCIT','MULT_APPUI',I,1,1,MONMOT(1),N2)
        CALL GETVTX('EXCIT','CORR_STAT',I,1,1,MONMOT(2),N3)
        IF (MONMOT(1).EQ.'OUI'.OR.MONMOT(2).EQ.'OUI') NOMMOT = 'OUI'
        IF (N1.NE.0) THEN
C         CAS D'UNE FONC_MULT
          NOMFON(I) = FONCT
          CALL JEVEUO(FONCT//'.PROL','L',LPROL)
          NOMFON(I+NBEXCI) = ZK16(LPROL)
          IF (L1.NE.0) THEN
C           CAS D'UN VECT_GENE
            CALL JEVEUT(CHANNO//'.VALE','L',JVALE)
            IADVEC(I)=JVALE
            IDESCF(I)=1
          ELSE
C           CAS D'UN NUME_ORDR
            INUMOR(I)=INUM
            IDESCF(I)=2
          ENDIF
        ELSE IF (M1.NE.0) THEN
C         CAS D'UN COEF MULT
          COEFM(I)=ALPHA
          IF (L1.NE.0) THEN
C           CAS D'UN VECT_GENE
            CALL JEVEUT(CHANNO//'.VALE','L',JVALE)
            IADVEC(I)=JVALE
            IDESCF(I)=3
          ELSE
C           CAS D'UN NUME_ORDR
            INUMOR(I)=INUM
            IDESCF(I)=4
          ENDIF
        ELSE IF (NA.NE.0) THEN
C         CAS D'UN ACCELEROGRAMME
          NOMFON(I) = FACCE
          FONACC(I) = FACCE
          CALL JEVEUO(FACCE//'.PROL','L',LPROL)
          NOMFON(I+NBEXCI) = ZK16(LPROL)
          FONACC(I+NBEXCI) = ZK16(LPROL)
          IF (L1.NE.0) THEN
C           CAS D'UN VECT_GENE
            CALL JEVEUT(CHANNO//'.VALE','L',JVALE)
            IADVEC(I)=JVALE
            IDESCF(I)=1
          ELSE
C           CAS D'UN NUME_ORDR
            INUMOR(I)=INUM
            IDESCF(I)=2
          ENDIF
        ENDIF
        IF (MONMOT(1)(1:3).EQ.'OUI') THEN
           CALL GETVID(' ','MODE_STAT',1,1,1,MODSTA,NBV)
           IF(NBV.EQ.0) THEN
              IER =IER+1
              CALL UTDEBM('E','MDRECF','IL MANQUE LES MODES STATIQUES')
              CALL UTFINM( )
              GOTO 10
           ENDIF
           CALL TRMULT(MODSTA,I,MAILLA,NEQ,IDDEEQ,PSDEL(1,I))
           CALL GETVID('EXCIT','VITE',I,1,1,FONVIT(I),N3)
           FONCT = FONVIT(I)
           CALL JEVEUO(FONCT//'.PROL','L',LPROL)
           FONVIT(I+NBEXCI) = ZK16(LPROL)
           CALL GETVID('EXCIT','DEPL',I,1,1,FONDEP(I),N4)
           FONCT = FONDEP(I)
           CALL JEVEUO(FONCT//'.PROL','L',LPROL)
           FONDEP(I+NBEXCI) = ZK16(LPROL)
        ENDIF
        IF (MONMOT(2)(1:3).EQ.'OUI') THEN
           CALL GETVID(' ','MODE_CORR',1,1,1,MODCOR,NBV)
           IF(NBV.EQ.0) THEN
              IER =IER+1
              CALL UTDEBM('E','MDRECF','IL MANQUE LES MODES CORRIGES')
              CALL UTFINM( )
              GOTO 10
           ENDIF
           CALL GETVID('EXCIT','D_FONC_DT',I,1,1,FONVIT(I),N3)
           FONCT = FONVIT(I)
           CALL JEVEUO(FONCT//'.PROL','L',LPROL)
           FONVIT(I+NBEXCI) = ZK16(LPROL)
           CALL GETVID('EXCIT','D_FONC_DT2',I,1,1,FONACC(I),N4)
           FONCT = FONACC(I)
           CALL JEVEUO(FONCT//'.PROL','L',LPROL)
           FONACC(I+NBEXCI) = ZK16(LPROL)
           FONDEP(I) = NOMFON(I)
           FONDEP(I+NBEXCI) = NOMFON(I+NBEXCI) 
C
           CALL RSEXCH(MODCOR,'DEPL',I,CHAMNO,IRET)
           CALL JEVEUO(CHAMNO//'.VALE','L',IMODCO)
           DO 30 IEQ = 1,NEQ
              PSDEL(IEQ,I) = ZR(IMODCO+IEQ-1)
 30        CONTINUE
           DO 40 NM = 1,NBMODE
              COEF = ZR(IADVEC(I)+NM-1)/RIGGEN(NM)
              CALL RSEXCH(BASEMO,'DEPL',NM,CHAMN2,IRET)
              CALL JEVEUO(CHAMN2//'.VALE','L',IMOD)
              DO 50 IEQ = 1,NEQ
                 PSDEL(IEQ,I) = PSDEL(IEQ,I) - COEF*ZR(IMOD+IEQ-1)
 50           CONTINUE
              CALL JELIBE(CHAMN2//'.VALE')
 40        CONTINUE
           CALL JELIBE(CHAMNO//'.VALE')
C
C     --- MISE A ZERO DES DDL DE LAGRANGE
           CALL ZERLAG(PSDEL(1,I),NEQ,ZI(IDDEEQ))
C
        ENDIF
 10   CONTINUE
C
      CALL JEDEMA()
      END
