      SUBROUTINE MRCONL(OPER,LMAT,NEQ2,TYPEV,RVECT,NVECT)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER LMAT,NEQ2,NVECT
      CHARACTER*1 TYPEV
      CHARACTER*4 OPER
      REAL*8 RVECT(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     TENIR COMPTE DU CONDITIONNEMENT DES LAGRANGE SUR DES VECTEURS
C     ------------------------------------------------------------------
C IN  : OPER    : 'MULT' : ON MULTIPLIE RVECT PAR RCOEF
C                 'DIVI' : ON DIVISE RVECT PAR RCOEF
C IN  : LMAT    : ADRESSE DU DESCRIPTEUR DE LA MATRICE
C IN  : NEQ2    : NOMBRE D'EQUATIONS DU VECTEUR RVECT(NEQ2,NVECT)
C                 SI NEQ2.LE.0 ALORS NEQ2 = LMAT(2)
C IN  : NVECT   : NOMBRE DE VECTEURS DU VECTEUR RVECT(NEQ2,NVECT)
C IN  : TYPEV   : TYPE DES COEFFICIENTS DU VECTEUR
C               = 'R'  : A COEFFICIENTS REELS
C               = 'C'  : A COEFFICIENTS COMPLEXES
C               = ' '  : LES COEFFICIENTS DU VECTEURS SONT DU MEME TYPE
C                        QUE CEUX DE LA MATRICE.
C VAR :  RVECT  : VECTEUR A MODIFIER
C               REMARQUE : RVECT DOIT ETRE DU MEME TYPE QUE LA MATRICE
C                          SOIT REAL*8
C                          SOIT COMPLEX*16
C     ------------------------------------------------------------------


      CHARACTER*1 FTYPE(2),TYPE,TYPECN
      CHARACTER*24 CONL
      COMPLEX*16 C8CST
      INTEGER IBID,IEQ,II,IND,IRET,IVE,JCONL,NEQ
C     ------------------------------------------------------------------
      DATA FTYPE/'R','C'/
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CALL ASSERT(OPER.EQ.'MULT' .OR. OPER.EQ.'DIVI')
      IF (TYPEV.EQ.' ') THEN
        TYPE=FTYPE(ZI(LMAT+3))
      ELSE
        TYPE=TYPEV
      ENDIF
      NEQ=NEQ2
      IF (NEQ2.LE.0)NEQ=ZI(LMAT+2)


      CONL=ZK24(ZI(LMAT+1))(1:19)//'.CONL'
      CALL JEEXIN(CONL,IRET)
      IF (IRET.NE.0) THEN
        CALL JELIRA(CONL,'TYPE',IBID,TYPECN)
        CALL JEVEUO(CONL,'L',JCONL)
        JCONL=JCONL-1


        IF (TYPE.EQ.'R' .AND. TYPECN.EQ.'R') THEN
          DO 30 IVE=1,NVECT
            IND=NEQ*(IVE-1)
            IF (OPER.EQ.'MULT') THEN
              DO 10 IEQ=1,NEQ
                RVECT(IND+IEQ)=RVECT(IND+IEQ)*ZR(JCONL+IEQ)
   10         CONTINUE
            ELSE
              DO 20 IEQ=1,NEQ
                RVECT(IND+IEQ)=RVECT(IND+IEQ)/ZR(JCONL+IEQ)
   20         CONTINUE
            ENDIF
   30     CONTINUE


        ELSEIF (TYPE.EQ.'C') THEN
          IF (TYPECN.EQ.'R') THEN
            DO 60 IVE=1,NVECT
              IND=NEQ*(IVE-1)
              IF (OPER.EQ.'MULT') THEN
                DO 40 IEQ=1,NEQ
                  II=IND+2*IEQ
                  RVECT(II-1)=RVECT(II-1)*ZR(JCONL+IEQ)
                  RVECT(II)=RVECT(II)*ZR(JCONL+IEQ)
   40           CONTINUE
              ELSE
                DO 50 IEQ=1,NEQ
                  II=IND+2*IEQ
                  RVECT(II-1)=RVECT(II-1)/ZR(JCONL+IEQ)
                  RVECT(II)=RVECT(II)/ZR(JCONL+IEQ)
   50           CONTINUE
              ENDIF
   60       CONTINUE

          ELSEIF (TYPECN.EQ.'C') THEN
            DO 90 IVE=1,NVECT
              IND=NEQ*(IVE-1)
              IF (OPER.EQ.'MULT') THEN
                DO 70 IEQ=1,NEQ
                  II=IND+2*IEQ
                  C8CST=DCMPLX(RVECT(II-1),RVECT(II))
                  C8CST=C8CST*ZC(JCONL+IEQ)
                  RVECT(II-1)=DBLE(C8CST)
                  RVECT(II)=DIMAG(C8CST)
   70           CONTINUE
              ELSE
                DO 80 IEQ=1,NEQ
                  II=IND+2*IEQ
                  C8CST=DCMPLX(RVECT(II-1),RVECT(II))
                  C8CST=C8CST/ZC(JCONL+IEQ)
                  RVECT(II-1)=DBLE(C8CST)
                  RVECT(II)=DIMAG(C8CST)
   80           CONTINUE
              ENDIF
   90       CONTINUE
          ENDIF
        ENDIF
      ENDIF
      CALL JEDEMA()
      END
