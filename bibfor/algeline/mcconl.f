      SUBROUTINE MCCONL(OPER,LMAT,NEQ2,TYPEV,CVECT,NVECT)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER LMAT,NEQ2,NVECT
      CHARACTER*1 TYPEV
      CHARACTER*4 OPER
      COMPLEX*16 CVECT(*)
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
C     TENIR COMPTE DU CONDITIONNEMENT DES LAGRANGE SUR LE SECOND MEMBRE
C     ------------------------------------------------------------------
C IN  : OPER    : 'MULT' : ON MULTIPLIE RVECT PAR RCOEF
C                 'DIVI' : ON DIVISE RVECT PAR RCOEF
C IN  : LMAT    : ADRESSE DU DESCRIPTEUR DE LA MATRICE
C IN  : NEQ2    : NOMBRE D'EQUATIONS DU VECTEUR CVECT(NEQ2,NVECT)
C                 SI NEQ2.LE.0 ALORS NEQ2 = LMAT(2)
C IN  : NVECT   : NOMBRE DE VECTEURS DU VECTEUR CVECT(NEQ2,NVECT)
C IN  : TYPEV   : TYPE DES COEFFICIENTS DU VECTEUR
C               = 'R'  : A COEFFICIENTS REELS
C               = 'C'  : A COEFFICIENTS COMPLEXES
C               = ' '  : LES COEFFICIENTS DU VECTEURS SONT DU MEME TYPE
C                        QUE CEUX DE LA MATRICE.
C VAR :  CVECT  : VECTEUR A MODIFIER
C               REMARQUE : CVECT DOIT ETRE DU MEME TYPE QUE LA MATRICE
C                          SOIT REAL*8
C                          SOIT COMPLEX*16
C     ------------------------------------------------------------------


      CHARACTER*1 FTYPE(2),TYPE,TYPECN
      CHARACTER*24 CONL
      INTEGER IBID,IEQ,II,IND,IRET,IVE,JCONL,NEQ,NEQC
      REAL*8 REALVE,RIMAGV
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
        NEQC=NEQ/2
        IF (TYPE.EQ.'R' .AND. TYPECN.EQ.'R') THEN
          DO 30 IVE=1,NVECT
            IND=NEQC*(IVE-1)
            II=1
            IF (OPER.EQ.'MULT') THEN
              DO 10 IEQ=1,NEQC
                REALVE=DBLE(CVECT(IND+IEQ))*ZR(JCONL+II)
                RIMAGV=DIMAG(CVECT(IND+IEQ))*ZR(JCONL+II+1)
                CVECT(IND+IEQ)=DCMPLX(REALVE,RIMAGV)
                II=II+2
   10         CONTINUE
            ELSE
              DO 20 IEQ=1,NEQC
                REALVE=DBLE(CVECT(IND+IEQ))/ZR(JCONL+II)
                RIMAGV=DIMAG(CVECT(IND+IEQ))/ZR(JCONL+II+1)
                CVECT(IND+IEQ)=DCMPLX(REALVE,RIMAGV)
                II=II+2
   20         CONTINUE
            ENDIF
   30     CONTINUE
        ELSEIF (TYPE.EQ.'C') THEN
          IF (TYPECN.EQ.'R') THEN
            DO 60 IVE=1,NVECT
              IND=NEQ*(IVE-1)
              IF (OPER.EQ.'MULT') THEN
                DO 40 IEQ=1,NEQ
                  CVECT(IND+IEQ)=CVECT(IND+IEQ)*ZR(JCONL+IEQ)
   40           CONTINUE
              ELSE
                DO 50 IEQ=1,NEQ
                  CVECT(IND+IEQ)=CVECT(IND+IEQ)/ZR(JCONL+IEQ)
   50           CONTINUE
              ENDIF
   60       CONTINUE
          ELSEIF (TYPECN.EQ.'C') THEN
            DO 90 IVE=1,NVECT
              IND=NEQ*(IVE-1)
              IF (OPER.EQ.'MULT') THEN
                DO 70 IEQ=1,NEQ
                  CVECT(IND+IEQ)=CVECT(IND+IEQ)*ZC(JCONL+IEQ)
   70           CONTINUE
              ELSE
                DO 80 IEQ=1,NEQ
                  CVECT(IND+IEQ)=CVECT(IND+IEQ)/ZC(JCONL+IEQ)
   80           CONTINUE
              ENDIF
   90       CONTINUE
          ENDIF
        ENDIF
      ENDIF
      CALL JEDEMA()
      END
