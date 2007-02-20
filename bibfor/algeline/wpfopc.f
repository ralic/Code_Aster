      SUBROUTINE WPFOPC( LMASSE, LAMOR, LRAIDE, FMIN,
     &                   SIGMA, MATOPA, RAIDE, NPREC)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       MATOPA, RAIDE
      INTEGER             LMASSE, LAMOR, LRAIDE
      REAL*8              FMIN
      COMPLEX*16          SIGMA
C     -----------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     DANS LE CAS QUADRATIQUE COMPLEXE
C     ------------------------------------------------------------------
C OUT LDYNAM  : IS : POINTEUR SUR LA FACTORISEE DE LA MATRICE DYNAMIQUE
C                    INDUITE PAR L'OPTION
C     ------------------------------------------------------------------
C
C     ------ DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      INTEGER      LMAT(3), LMATRA
      CHARACTER*24 NMAT(3),NMATRA
      REAL*8       ASHIFT, R8DEPI, CONSTC(6)
      REAL*8 VALR(2)
      CHARACTER*1  TYPCST(3)

      CHARACTER*8  NAMDDL
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
         CALL U2MESG('I', 'ALGELINE4_94',0,' ',0,0,2,VALR)
      ENDIF

      ASHIFT = - (ASHIFT*FSHIFT)/SQRT(1.D0-ASHIFT*ASHIFT)
      SIGMA = DCMPLX(ASHIFT,FSHIFT)
C
C     --- DECALAGE COMPLEXE ---
      CALL MTDEFS(MATOPA,RAIDE,'V','C')
      CALL MTDSCR(MATOPA)
      NMATRA=MATOPA(1:19)//'.&INT'
      CALL JEVEUO(MATOPA(1:19)//'.&INT','E',LMATRA)
      CONSTC(1) = DBLE(SIGMA*SIGMA)
      CONSTC(2) = DIMAG(SIGMA*SIGMA)
      CONSTC(3) = DBLE(SIGMA)
      CONSTC(4) = DIMAG(SIGMA)
      CONSTC(5) = 1.D0
      CONSTC(6) = 0.D0
      TYPCST(1) = 'C'
      TYPCST(2) = 'C'
      TYPCST(3) = 'C'
      CALL MTCMBL(3,TYPCST,CONSTC,NMAT,NMATRA,NAMDDL,' ')
C     --- FACTORISATION DES MATRICES ---
C
      CALL TLDLGG(1,LMATRA,1,0,NPREC,NDECI,ISINGU,NPVNEG,IERX)
C
      CALL JEDEMA()
      END
