      SUBROUTINE WPFOPR( LMASSE,LAMOR,LRAIDE,APPR,FMIN,
     &                   SIGMA,MATOPA,MATPSC,RAIDE,LQZ,SOLVEU)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       APPR,MATOPA,MATPSC,RAIDE
      CHARACTER*19        SOLVEU
      INTEGER             LMASSE,LAMOR,LRAIDE
      REAL*8              FMIN
      COMPLEX*16          SIGMA
      LOGICAL             LQZ
C     -----------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================
C     DETERMINATION D'UN SHIFT ET CALCUL DE LA MATRICE SHIFTEE
C     DANS LE CAS QUADRATIQUE REEL
C     ------------------------------------------------------------------
C OUT LDYNAM  : IS : POINTEUR SUR LA FACTORISEE DE LA MATRICE DYNAMIQUE
C                    INDUITE PAR L'OPTION
C OUT SIGMA   : C16: SHIFT
C IN  LQZ     : METHODE QZ OU NON
C IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
C     ------------------------------------------------------------------
C
C     ------ DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      LMAT(3),LMATRA,LMTPSC,JPOMR,JREFE,IBID,JBID
      REAL*8       ASHIFT,CONSTR(3),R8DEPI,CONSTC(6),VALR(2)
      CHARACTER*1  TYPCST(3),BASE
      CHARACTER*8  NAMDDL
      CHARACTER*19 NOMI,NOMT,MATPRE,MATASS
      CHARACTER*24 NMAT(3),NMATRA,NMTPSC

C     ------------------------------------------------------------------
      DATA NAMDDL/'        '/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()

      LMAT(1) = LMASSE
      NMAT(1) = ZK24(ZI(LMAT(1)+1))
      LMAT(2) = LAMOR
      NMAT(2) = ZK24(ZI(LMAT(2)+1))
      LMAT(3) = LRAIDE
      NMAT(3) = ZK24(ZI(LMAT(3)+1))
C
      FSHIFT = R8DEPI()*FMIN
      ASHIFT = 0.D0
C
      CALL GETVR8('CALC_FREQ','AMOR_REDUIT',1,1,1,ASHIFT,IBID)
C
      IF (ABS(ASHIFT).GE.1.D0) THEN
         ASHIFT = 0.95D0
         VALR (1) = 1.D0
         VALR (2) = 0.95D0
         CALL U2MESG('I', 'ALGELINE4_95',0,' ',0,0,2,VALR)
      ENDIF

      ASHIFT = - (ASHIFT*FSHIFT)/SQRT(1.D0-ASHIFT*ASHIFT)
      SIGMA = DCMPLX(ASHIFT,FSHIFT)

C --- POUR QZ CALCUL DE LA MATRICE SHIFTEE ET DE SA FACTORISEE INUTILE
      IF (LQZ) GOTO 999
C
      IF (FMIN.EQ.0.D0) THEN
         JPOMR=0
         DO 15 ICOMB = 1,3
C           ON RECHERCHE UNE EVENTUELLE MATRICE NON SYMETRIQUE
            NOMI=NMAT(ICOMB)
            CALL JEVEUO(NOMI//'.REFA','L',JREFE)
            IF ( ZK24(JREFE-1+9).EQ.'MR' ) THEN
               JPOMR=ICOMB
            ENDIF
   15    CONTINUE
C
C        --- DECALAGE REEL ---
         IF ( JPOMR.EQ.0 ) THEN
            CALL MTDEFS(MATOPA,RAIDE,'V','R')
         ELSE
            NOMT = NMAT(JPOMR)
            CALL MTDEFS(MATOPA,NOMT,'V','C')
         ENDIF
         CALL MTDSCR(MATOPA)
         NMATRA=MATOPA(1:19)//'.&INT'
         CALL JEVEUO(MATOPA(1:19)//'.&INT','E',LMATRA)
         DO 10 ICOMB = 1,3
            TYPCST(ICOMB) = 'R'
  10     CONTINUE
         CONSTR(1) = ASHIFT*ASHIFT
         CONSTR(2) = ASHIFT
         CONSTR(3) = 1.D0
         CALL MTCMBL(3,TYPCST,CONSTR,NMAT,NMATRA,NAMDDL,' ','ELIM=')
         LMTPSC = 0
C
      ELSE
         JPOMR=0
         DO 25 ICOMB = 1,3
C           ON RECHERCHE UNE EVENTUELLE MATRICE NON SYMETRIQUE
            NOMI=NMAT(ICOMB)
            CALL JEVEUO(NOMI//'.REFA','L',JREFE)
            IF ( ZK24(JREFE-1+9).EQ.'MR' ) THEN
               JPOMR=ICOMB
            ENDIF
   25    CONTINUE
C
C        --- DECALAGE COMPLEXE ---
         IF ( JPOMR.EQ.0 ) THEN
            CALL MTDEFS(MATOPA,RAIDE,'V','C')
         ELSE
            NOMT = NMAT(JPOMR)
            CALL MTDEFS(MATOPA,NOMT,'V','C')
         ENDIF
         CALL MTDSCR(MATOPA)
         NMATRA=MATOPA(1:19)//'.&INT'
         CALL JEVEUO(MATOPA(1:19)//'.&INT','E',LMATRA)
         DO 20 ICOMB = 1,3
            TYPCST(ICOMB) = 'C'
  20     CONTINUE
         CONSTC(1) = DBLE(SIGMA*SIGMA)
         CONSTC(2) = DIMAG(SIGMA*SIGMA)
         CONSTC(3) = DBLE(SIGMA)
         CONSTC(4) = DIMAG(SIGMA)
         CONSTC(5) = 1.D0
         CONSTC(6) = 0.D0
         CALL MTCMBL(3,TYPCST,CONSTC,NMAT,NMATRA,NAMDDL,' ','ELIM=')
         IF (APPR.EQ.'R') THEN
            IF ( JPOMR.EQ.0 ) THEN
               CALL MTDEFS(MATPSC,RAIDE,'V','R')
            ELSE
               NOMT = NMAT(JPOMR)
               CALL MTDEFS(MATPSC,NOMT,'V','R')
            ENDIF
            CALL MTDSCR(MATPSC)
            NMTPSC=MATPSC(1:19)//'.&INT'
            CALL JEVEUO(MATPSC(1:19)//'.&INT','E',LMTPSC)
            DO 30 ICOMB = 1,3
               TYPCST(ICOMB) = 'R'
  30        CONTINUE
            CONSTR(1) = ASHIFT*ASHIFT
            CONSTR(2) = ASHIFT
            CONSTR(3) = 1.D0
            CALL MTCMBL(3,TYPCST,CONSTR,NMAT,NMTPSC,NAMDDL,' ','ELIM=')
C
         ELSE
C
            LMTPSC = 0
C
         ENDIF
C
      ENDIF
C
C     --- FACTORISATION DES MATRICES ---
C
      BASE='V'
      MATPRE=' '
      MATASS=ZK24(ZI(LMATRA+1))
      CALL PRERES(SOLVEU,BASE,IBID,MATPRE,MATASS,JBID,1)
      IF (LMTPSC.NE.0) THEN
        MATASS=ZK24(ZI(LMTPSC+1))
        CALL PRERES(SOLVEU,BASE,IBID,MATPRE,MATASS,JBID,1)
      ENDIF
C
  999 CONTINUE
      CALL JEDEMA()
      END
