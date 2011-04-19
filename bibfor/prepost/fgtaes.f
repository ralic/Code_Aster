      SUBROUTINE FGTAES(NOMMAT,NOMNAP,NBCYCL,EPSMIN,EPSMAX,DOM)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOMMAT,NOMNAP
      REAL*8                          EPSMIN(*),EPSMAX(*)
      REAL*8                   DOM(*)
      INTEGER                  NBCYCL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     -----------------------------------------------------------------
C     CALCUL DU DOMMAGE ELEMENTAIRE POUR TAHERI_MIXTE
C     ------------------------------------------------------------------
C IN  NOMMAT : K   : NOM DU MATERIAU
C IN  NOMNAP : K   : NOM DE LA NAPPE
C IN  NBCYCL : I   : NOMBRE DE CYCLES
C IN  EPSMIN : R   : DEFORMATIONS MINIMALES DES CYCLES
C IN  EPSMAX : R   : DEFORMATIONS MAXIMALES DES CYCLES
C OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
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
      INTEGER ICODRE(10),ICODWO
      INTEGER  ICODBA,ICODHS
      CHARACTER*4  MODE
      CHARACTER*8  NOMRE1,NOMRE2,NOMPA1,NOMP(2),NOMRES(10)
      CHARACTER*8  CARA,NOMPAR
      CHARACTER*10 PHENO
      REAL*8       NRUPT,DELTA,DNAP,EPMAX,VALP(2),VAL(10)
      REAL*8       SALT,X,RE,SLMODI,Y
      LOGICAL ENDUR
      DATA ZERO /1.D-13/
C
      CALL JEMARQ()
C
      EPMAX = 0.D0
      NOMRE1 = 'MANSON_C'
      NOMRE2 = 'WOHLER  '
      NBPAR     = 1
      PHENO     = 'FATIGUE '
      NOMPA1    = 'EPSI    '
C
      CARA = 'WOHLER'
      CALL RCPARE(NOMMAT,PHENO,CARA,ICODWO)
      IF(ICODWO.EQ.0) MODE = 'FONC'
      CARA = 'A_BASQUI'
      CALL RCPARE(NOMMAT,PHENO,CARA,ICODBA)
      IF(ICODBA.EQ.0) THEN
        NOMPAR = ' '
        NBPAR  = 0
        NOMRES(2) = 'A_BASQUI'
        NOMRES(3) = 'BETA_BAS'
        CALL RCVALE(NOMMAT,'FATIGUE',NBPAR,NOMPAR,RBID,2,NOMRES(2),
     &                                     VAL(2),ICODRE(2),2)
        MODE = 'BASQ'
      ENDIF
      CARA = 'A0'
      CALL RCPARE(NOMMAT,PHENO,CARA,ICODHS)
      IF(ICODHS.EQ.0) THEN
        NOMRES(4) = 'E_REFE'
        NOMRES(5) = 'A0'
        NOMRES(6) = 'A1'
        NOMRES(7) = 'A2'
        NOMRES(8) = 'A3'
        NOMRES(9) = 'SL'
        NBPAR     = 0
        NOMPAR    = ' '
        CALL RCVALE(NOMMAT,'FATIGUE',NBPAR,NOMPAR,RBID,6,NOMRES(4),
     &                                     VAL(4),ICODRE(4),2)
        NOMRES(10) = 'E'
        CALL RCVALE(NOMMAT,'ELAS',NBPAR,NOMPAR,RBID,1,NOMRES(10),RE,
     &                                         ICODRE(10),2)
        MODE = 'ZONE'
      ENDIF
C
      DO 10 I=1,NBCYCL
        DELTA = (ABS(EPSMAX(I)-EPSMIN(I)))/2.D0
        IF(DELTA.GT.EPMAX-ZERO) THEN
          EPMAX = DELTA
C
C --- INTERPOLATION SUR MANSON_COFFIN ---
C
          CALL RCVALE(NOMMAT,PHENO,NBPAR,NOMPA1,DELTA,1,NOMRE1,
     &                                          NRUPT,ICODRE(1),2)
          DOM(I) = 1.D0/NRUPT
        ELSE
          NOMP(1) = 'X'
          NOMP(2) = 'EPSI'
          VALP(1) = EPMAX
          VALP(2) = DELTA
          CALL FOINTE('F ',NOMNAP,2,NOMP,VALP,DNAP,IER)
C
C --- INTERPOLATION SUR WOHLER ---
C
          IF(MODE.EQ.'FONC') THEN
            NBPAR = 1
            NOMPAR = 'SIGM'
            CALL LIMEND( NOMMAT,DNAP,'WOHLER',ENDUR)
            IF (ENDUR) THEN
                DOM(I) = 0.D0
            ELSE
                CALL RCVALE(NOMMAT,PHENO,NBPAR,NOMPAR,DNAP,1,NOMRE2,
     &                                            NRUPT,ICODRE(1),2)
                DOM(I) = 1.D0/NRUPT
            ENDIF
          ELSEIF(MODE.EQ.'BASQ') THEN
            DOM(I) = VAL(2)* DNAP**VAL(3)
          ELSEIF(MODE.EQ.'ZONE') THEN
            SLMODI  = VAL(9)
            SALT  = (VAL(4)/RE)*DNAP
            X  = LOG10 (SALT)
            IF (SALT.GE.SLMODI) THEN
              Y = VAL(5) + VAL(6)*X + VAL(7)*X**2 + VAL(8)*X**3
              NRUPT = 10**Y
              DOM(I) = 1.D0 / NRUPT
            ELSE
              DOM(I) = 0.D0
            ENDIF
          ENDIF
        ENDIF
 10   CONTINUE
C
      CALL JEDEMA()
      END
