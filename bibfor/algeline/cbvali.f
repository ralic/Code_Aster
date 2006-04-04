      SUBROUTINE CBVALI(NBCOMB,TYPCST,CONST,TYPMAT,LMAT,TYPRES,LRES)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NBCOMB,LMAT(*),LRES
      CHARACTER*(*) TYPCST(*),TYPMAT(*),TYPRES
      REAL*8 CONST(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 04/04/2006   AUTEUR VABHHTS J.PELLET 
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
C     COMBINAISON LINEAIRE DES .CCVA DES MATRICES
C     CETTE OPERATION N'EST FAITE QUE LORSQUE LES MATRICES
C     ONT DES DDLS ELIMINES
C     LES DDLS ELIMINES DOIVENT ETRE LES MEMES POUR
C     TOUTES LES MATRICES A COMBINER
C       *  LES MATRICES SONT SUPPOSEES ETRE DE MEME STOCKAGE
C          MAIS PEUVENT ETRE A ELEMENTS REELS OU COMPLEXES
C       *  LES SCALAIRES SONT REELS OU COMPLEXES
C     -----------------------------------------------------------------
C IN  I  NBCOMB = NOMBRE DE MATRICES A COMBINER
C IN  K* TYPCST = TYPE DES CONSTANTES (R OU C OU I)
C IN  R  CONST  = TABLEAU DE R*8    DES COEFICIENTS
C IN  K* TYPMAT = TYPE DES MATRICES   (R OU C)
C IN  I  LMAT = TABLEAU DES POINTEURS DES MATRICES
C IN  K* TYPRES = TYPE DES MATRICES   (R OU C)
C IN  I  LRES = POINTEUR DE MATRICE RESULTAT
C
C     -----------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     -----------------------------------------------------------------
C     NCCVA = LONGUEUR DES BLOCS
      INTEGER NCCVA
C     -----------------------------------------------------------------
      CHARACTER*1 CH1,KBID
      CHARACTER*19 MATRES,MATI
C     -----------------------------------------------------------------
      CALL JEMARQ()
C
      MATRES = ZK24(ZI(LRES+1))
      CALL JELIRA(MATRES//'.CCVA','LONMAX',NCCVA,KBID)


      CALL JEVEUO(MATRES//'.CCVA','E',JCCVA)
      ICONST = 1

      DO 80 IMAT = 1,NBCOMB
        MATI = ZK24(ZI(LMAT(IMAT)+1))
        CALL JEVEUO(MATI//'.CCVA','L',JCCVA2)
        CALL JELIRA(MATI//'.CCVA','LONMAX',NCCVA2,KBID)
        CALL ASSERT(NCCVA2.EQ.NCCVA)


        IF (TYPRES(1:1).EQ.'R') THEN
C       ---------------------------------
          IF (TYPMAT(IMAT) (1:1).EQ.'R') THEN
            DO 10 I = 1,NCCVA
              ZR(JCCVA+I-1) = ZR(JCCVA+I-1) +
     &                        CONST(ICONST)*ZR(JCCVA2-1+I)
   10       CONTINUE
          ELSE IF (TYPMAT(IMAT) (1:1).EQ.'C') THEN
            IF (TYPCST(IMAT) (1:1).EQ.'R' .OR.
     &          TYPCST(IMAT) (1:1).EQ.'C') THEN
              DO 20 I = 1,NCCVA
                ZR(JCCVA+I-1) = ZR(JCCVA+I-1) +
     &                          CONST(ICONST)*ZC(JCCVA2-1+I)
   20         CONTINUE
            ELSE IF (TYPCST(IMAT) (1:1).EQ.'I') THEN
              DO 30 I = 1,NCCVA
                ZR(JCCVA+I-1) = ZR(JCCVA+I-1) +
     &                          CONST(ICONST)*DIMAG(ZC(JCCVA2-1+I))
   30         CONTINUE
            ELSE
              CH1 = TYPCST(IMAT) (1:1)
              CALL UTMESS('F','CBVALI_1','TYPE INCONNU: '//CH1)
            END IF
          ELSE
            CH1 = TYPMAT(IMAT) (1:1)
            CALL UTMESS('F','CBVALI_2','TYPE INCONNU: '//CH1)
          END IF


        ELSE IF (TYPRES(1:1).EQ.'C') THEN
C       ---------------------------------
          IF (TYPMAT(IMAT) (1:1).EQ.'R') THEN
            IF (TYPCST(IMAT) (1:1).EQ.'R') THEN
              DO 40 I = 1,NCCVA
                ZC(JCCVA+I-1) = ZC(JCCVA+I-1) +
     &                          CONST(ICONST)*ZR(JCCVA2-1+I)
   40         CONTINUE
            ELSE IF (TYPCST(IMAT) (1:1).EQ.'C') THEN
              C8CST = DCMPLX(CONST(ICONST),CONST(ICONST+1))
              DO 50 I = 1,NCCVA
                ZC(JCCVA+I-1) = ZC(JCCVA+I-1) + C8CST*ZR(JCCVA2-1+I)
   50         CONTINUE
            END IF
          ELSE IF (TYPMAT(IMAT) (1:1).EQ.'C') THEN
            IF (TYPCST(IMAT) (1:1).EQ.'R') THEN
              DO 60 I = 1,NCCVA
                ZC(JCCVA+I-1) = ZC(JCCVA+I-1) +
     &                          CONST(ICONST)*ZC(JCCVA2-1+I)
   60         CONTINUE
            ELSE IF (TYPCST(IMAT) (1:1).EQ.'C') THEN
              C8CST = DCMPLX(CONST(ICONST),CONST(ICONST+1))
              DO 70 I = 1,NCCVA
                ZC(JCCVA+I-1) = ZC(JCCVA+I-1) + C8CST*ZC(JCCVA2-1+I)
   70         CONTINUE
            ELSE
              CH1 = TYPCST(IMAT) (1:1)
              CALL UTMESS('F','CBVALI_3','TYPE INCONNU: '//CH1)
            END IF


          ELSE
C       ---------------------------------
            CH1 = TYPMAT(IMAT) (1:1)
            CALL UTMESS('F','CBVALI_4','TYPE INCONNU: '//CH1)
          END IF

        END IF

        ICONST = ICONST + 1
        IF (TYPCST(IMAT) (1:1).EQ.'C') ICONST = ICONST + 1
        CALL JELIBE(MATI//'.CCVA')
   80 CONTINUE


      CALL JEDEMA()
      END
