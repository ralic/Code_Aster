      SUBROUTINE MTCONL(NBCOMB,TYPCST,CONST,LMAT,TYPRES,LRES)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBCOMB,                    LMAT(*),    LRES
      CHARACTER*(*)            TYPCST(*)
      CHARACTER*(*)                                     TYPRES
      REAL*8                          CONST(*)
      CHARACTER*1 TYPREZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 24/03/2009   AUTEUR REZETTE C.REZETTE 
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
C     COMBINAISON LINEAIRE DU CONDITIONNEMENT DES LAGRANGES DES MATRICES
C       *  LES MATRICES SONT SONT A COEFFICIENTS REELS OU COMPLEXES
C       *  LES SCALAIRES SONT REELS OU COMPLEXES
C     -----------------------------------------------------------------
C IN  NBCOMB : I : NOMBRE DE MATRICES A COMBINER
C IN  TYPCST : K1: TYPE DES CONSTANTES (R OU C)
C IN  CONST  : R : TABLEAU DE R*8    DES COEFICIENTS
C IN  LMAT   : I : TABLEAU DES POINTEURS DES MATRICES
C IN  TYPRES : K1: TYPE DES MATRICES   (R OU C)
C IN  LRES   : I : POINTEUR DE MATRICE RESULTAT
C     -----------------------------------------------------------------
C
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*6      PGC, PGCANC
      COMMON  /NOMAJE/ PGC
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      REAL*8        UN
      CHARACTER*1   TYPE
      CHARACTER*4   CLAS,CUMUL
      CHARACTER*19  CBID(2)
      REAL*8        TCST
      CHARACTER*8   TPCST
C     ------------------------------------------------------------------

      CALL JEMARQ()
      PGCANC = PGC
      PGC    = 'MTCONL'
      TYPREZ = TYPRES
C
C     --- DUPLICATION EVENTUELLE DU .CONL ---
      CBID(1) = ZK24(ZI(LRES+1))
      NEQ     = ZI(LRES+2)
      JCOMB   = 0
      ICONST  = 1
      DO 10 ICOMB = 1, NBCOMB
         CBID(2) = ZK24(ZI(LMAT(ICOMB)+1))
         CALL JEEXIN(CBID(2)//'.CONL',IER2)
         IF (IER2.NE.0) THEN
C           --- CREATION (EVENTUELLE)  D'UN .CONL AU TYPE "TYPRES" ---
            CALL JEEXIN(CBID(1)//'.CONL',IER1)
            IF (IER1.NE.0) THEN
               CALL JELIRA(CBID(1)//'.CONL','TYPE',IBID,TYPE)
               IF ( TYPE .NE. TYPREZ ) THEN
                  CALL JEDETR(CBID(1)//'.CONL')
                  IER1 = 0
               ENDIF
            ENDIF
            IF (IER1.EQ.0) THEN
               CALL JELIRA(CBID(1)//'.VALM','CLAS',IBID,CLAS)
               CALL WKVECT(CBID(1)//'.CONL',CLAS(1:1)//' V '//TYPREZ,
     +                                                      NEQ,LCONL1)
            ELSE
               CALL JEVEUO(CBID(1)//'.CONL','E',LCONL1)
            ENDIF
C
C           --- REMPLISSAGE ---
            CUMUL = 'ZERO'
            DO 30 JCOMB = ICOMB , NBCOMB
               CBID(2) = ZK24(ZI(LMAT(JCOMB)+1))
               CALL JEEXIN(CBID(2)//'.CONL',IER2)
               IF (IER2.NE.0) THEN
C                 --- MOULINEX ----
                  CALL JEVEUO(CBID(2)//'.CONL','L',LCONL2)
                  CALL JELIRA(CBID(2)//'.CONL','TYPE',IBID,TYPE)

C                 SI LE COEFFICIENT EST COMPLEXE : ATTENTION !
C                 CAR LES MATRICES INITIALES ET RESULTATS .CONL SONT
C                 FORCEMENT REELLES DU COUP ON DOIT FOURNIR UN
C                 CONST REEL !
                  IF (TYPCST(JCOMB).EQ.'C') THEN
                     TCST = ABS(DCMPLX(CONST(ICONST),CONST(ICONST+1)))
                     TPCST = 'R'
                     CALL MTXCNL(CUMUL,TPCST,TCST,
     +                              TYPE,LCONL2,TYPREZ,LCONL1,NEQ)
                  ELSE
                     CALL MTXCNL(CUMUL,TYPCST(JCOMB),CONST(ICONST),
     +                              TYPE,LCONL2,TYPREZ,LCONL1,NEQ)
                  ENDIF
                  CUMUL = 'CUMU'
               ENDIF
               ICONST = ICONST + 1
               IF ( TYPCST(JCOMB) .EQ. 'C' ) ICONST = ICONST + 1
   30       CONTINUE
            GO TO 20
         ELSE
            ICONST = ICONST + 1
            IF ( TYPCST(ICOMB) .EQ. 'C' ) ICONST = ICONST + 1
         ENDIF
   10 CONTINUE
C
C
   20 CONTINUE
      IF ( JCOMB .EQ. 0 ) THEN
C        --- PAS DE .CONL ---
         CALL JEEXIN(CBID(1)//'.CONL',IER1)
         IF ( IER1 .NE. 0 ) CALL JEDETR(CBID(1)//'.CONL')
      ENDIF
      PGC = PGCANC
      CALL JEDEMA()
      END
