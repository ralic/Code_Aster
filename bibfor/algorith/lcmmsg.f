      SUBROUTINE LCMMSG(NOMFAM,NBSYS,NUSYS,PGL2,MUS,NG,MG,IR,Q)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
C RESPONSABLE JMBHH01 J.M.PROIX
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       IN  FAMSYS  :  NOM FAMILLE SYS GLIS
C           NUSYS   :  NUMERO SYS GLIS (FACULTATIF)
C           PGL2     :  MATRICE DE PASSAGE REPERE GLOBAL REPERE LOCAL
C           IR      :  =0 pas de rotation de reseau ; =1 : rotation
C           Q       :  matrice de rotation de reseau
C     OUT:
C           NBSYS    : NOMBRE DE SYS GLIS
C           MUS       : TENSEUR MUS POUR LE SYS GLIS NUMERO NUSYS
C
      CHARACTER*16 NOMFAM
      REAL*8 MUS(6),N(30,3),M(30,3),PGL2(3,3),NG(3),NL(3),MG(3),ML(3)
      REAL*8 SQRT2,SQRT3,Q(3,3),NGR(3),MGR(3),TBSYS(30,6),NORN,NORM
      INTEGER NBSYS,NUSYS,K,I,J,IR
C     ----------------------------------------------------------------
C TOLE CRP_20

      IF (NOMFAM(1:4).EQ.'UTIL') THEN
        CALL LCMMJS(NOMFAM,NBSYS,TBSYS)
      ENDIF

      IF (NUSYS.EQ.0) THEN
        IF (NOMFAM.EQ.'BCC24') THEN
          NBSYS=24
          GOTO 150

        ELSEIF (NOMFAM.EQ.'OCTAEDRIQUE') THEN
          NBSYS=12
          GOTO 150

        ELSEIF (NOMFAM.EQ.'CUBIQUE1') THEN
          NBSYS=12
          GOTO 150

        ELSEIF (NOMFAM.EQ.'CUBIQUE2') THEN
          NBSYS=12
          GOTO 150

        ELSEIF (NOMFAM.EQ.'ZIRCONIUM') THEN
          NBSYS=30
          GOTO 150

        ELSEIF (NOMFAM.EQ.'UNIAXIAL') THEN
          NBSYS=1
          GOTO 150

        ELSEIF (NOMFAM(1:4).EQ.'UTIL') THEN
          GOTO 150

        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ENDIF

      SQRT2=SQRT(2.D0)
      SQRT3=SQRT(3.D0)
      CALL VECINI(6,0.D0,MUS)
      IF (NOMFAM.EQ.'ZIRCONIUM') THEN
C  prism1
        N(1,1)=1.0D0
        N(1,2)=0.0D0
        N(1,3)=0.0D0
        N(2,1)=-0.5D0
        N(2,2)=0.866025403784D0
        N(2,3)=0.0D0
        N(3,1)=-0.5D0
        N(3,2)=-0.866025403784D0
        N(3,3)=0.0D0
        M(1,1)=0.0D0
        M(1,2)=-1.0D0
        M(1,3)=0.0D0
        M(2,1)=0.866025403784D0
        M(2,2)=0.5D0
        M(2,3)=0.0D0
        M(3,1)=-0.866025403784D0
        M(3,2)=0.5D0
        M(3,3)=0.0D0

C  basal1
        N(4,1)=1.0D0
        N(4,2)=0.0D0
        N(4,3)=0.0D0
        N(5,1)=-0.5D0
        N(5,2)=0.866025403784D0
        N(5,3)=0.0D0
        N(6,1)=-0.5D0
        N(6,2)=-0.866025403784D0
        N(6,3)=0.0D0
        M(4,1)=0.0D0
        M(4,2)=0.0D0
        M(4,3)=1.0D0
        M(5,1)=0.0D0
        M(5,2)=0.0D0
        M(5,3)=1.0D0
        M(6,1)=0.0D0
        M(6,2)=0.0D0
        M(6,3)=1.0D0

C  pyr_a1
        N(7,1)=1.0D0
        N(7,2)=0.0D0
        N(7,3)=0.0D0
        N(8,1)=1.0D0
        N(8,2)=0.0D0
        N(8,3)=0.0D0
        N(9,1)=-0.5D0
        N(9,2)=0.866025403784D0
        N(9,3)=0.0D0
        N(10,1)=-0.5D0
        N(10,2)=0.866025403784D0
        N(10,3)=0.0D0
        N(11,1)=-0.5D0
        N(11,2)=-0.866025403784D0
        N(11,3)=0.0D0
        N(12,1)=-0.5D0
        N(12,2)=-0.866025403784D0
        N(12,3)=0.0D0
        M(7,1)=0.0D0
        M(7,2)=-0.87856329157D0
        M(7,3)=0.477625944339D0
        M(8,1)=0.0D0
        M(8,2)=0.87856329157D0
        M(8,3)=0.477625944339D0
        M(9,1)=-0.760858129332D0
        M(9,2)=-0.439281645785D0
        M(9,3)=0.477625944339D0
        M(10,1)=0.760858129332D0
        M(10,2)=0.439281645785D0
        M(10,3)=0.477625944339D0
        M(11,1)=-0.760858129332D0
        M(11,2)=0.439281645785D0
        M(11,3)=0.477625944339D0
        M(12,1)=0.760858129332D0
        M(12,2)=-0.439281645785D0
        M(12,3)=0.477625944339D0

C  pyr_c_a1
        N(13,1)=0.531670580449D0
        N(13,2)=0.0D0
        N(13,3)=-0.846951234656D0
        N(14,1)=0.265835290225D0
        N(14,2)=0.460440229114D0
        N(14,3)=-0.846951234656D0
        N(15,1)=0.265835290225D0
        N(15,2)=0.460440229114D0
        N(15,3)=-0.846951234656D0
        N(16,1)=-0.265835290225D0
        N(16,2)=0.460440229114D0
        N(16,3)=-0.846951234656D0
        N(17,1)=-0.265835290225D0
        N(17,2)=0.460440229114D0
        N(17,3)=-0.846951234656D0
        N(18,1)=-0.531670580449D0
        N(18,2)=0.0D0
        N(18,3)=-0.846951234656D0
        N(19,1)=-0.531670580449D0
        N(19,2)=0.0D0
        N(19,3)=-0.846951234656D0
        N(20,1)=-0.265835290225D0
        N(20,2)=-0.460440229114D0
        N(20,3)=-0.846951234656D0
        N(21,1)=-0.265835290225D0
        N(21,2)=-0.460440229114D0
        N(21,3)=-0.846951234656D0
        N(22,1)=0.265835290225D0
        N(22,2)=-0.460440229114D0
        N(22,3)=-0.846951234656D0
        N(23,1)=0.265835290225D0
        N(23,2)=-0.460440229114D0
        N(23,3)=-0.846951234656D0
        N(24,1)=0.531670580449D0
        N(24,2)=0.0D0
        N(24,3)=-0.846951234656D0
        M(13,1)=0.760858129332D0
        M(13,2)=0.439281645785D0
        M(13,3)=0.477625944339D0
        M(14,1)=0.760858129332D0
        M(14,2)=0.439281645785D0
        M(14,3)=0.477625944339D0
        M(15,1)=0.0D0
        M(15,2)=0.87856329157D0
        M(15,3)=0.477625944339D0
        M(16,1)=0.0D0
        M(16,2)=0.87856329157D0
        M(16,3)=0.477625944339D0
        M(17,1)=-0.760858129332D0
        M(17,2)=0.439281645785D0
        M(17,3)=0.477625944339D0
        M(18,1)=-0.760858129332D0
        M(18,2)=0.439281645785D0
        M(18,3)=0.477625944339D0
        M(19,1)=-0.760858129332D0
        M(19,2)=-0.439281645785D0
        M(19,3)=0.477625944339D0
        M(20,1)=-0.760858129332D0
        M(20,2)=-0.439281645785D0
        M(20,3)=0.477625944339D0
        M(21,1)=0.0D0
        M(21,2)=-0.87856329157D0
        M(21,3)=0.477625944339D0
        M(22,1)=0.0D0
        M(22,2)=-0.87856329157D0
        M(22,3)=0.477625944339D0
        M(23,1)=0.760858129332D0
        M(23,2)=-0.439281645785D0
        M(23,3)=0.477625944339D0
        M(24,1)=0.760858129332D0
        M(24,2)=-0.439281645785D0
        M(24,3)=0.477625944339D0

C  pyr2_c_a1
        N(25,1)=-0.265835290225D0
        N(25,2)=-0.460440229114D0
        N(25,3)=0.846951234656D0
        N(26,1)=0.531670580449D0
        N(26,2)=0.0D0
        N(26,3)=0.846951234656D0
        N(27,1)=-0.265835290225D0
        N(27,2)=0.460440229114D0
        N(27,3)=0.846951234656D0
        N(28,1)=0.265835290225D0
        N(28,2)=0.460440229114D0
        N(28,3)=0.846951234656D0
        N(29,1)=-0.531670580449D0
        N(29,2)=0.0D0
        N(29,3)=0.846951234656D0
        N(30,1)=0.265835290225D0
        N(30,2)=-0.460440229114D0
        N(30,3)=0.846951234656D0
        M(25,1)=0.423475617328D0
        M(25,2)=0.733481284978D0
        M(25,3)=0.531670580449D0
        M(26,1)=-0.846951234656D0
        M(26,2)=0.0D0
        M(26,3)=0.531670580449D0
        M(27,1)=0.423475617328D0
        M(27,2)=-0.733481284978D0
        M(27,3)=0.531670580449D0
        M(28,1)=-0.423475617328D0
        M(28,2)=-0.733481284978D0
        M(28,3)=0.531670580449D0
        M(29,1)=0.846951234656D0
        M(29,2)=0.0D0
        M(29,3)=0.531670580449D0
        M(30,1)=-0.423475617328D0
        M(30,2)=0.733481284978D0
        M(30,3)=0.531670580449D0

C        N ET L SONT UNITAIRES
      ELSEIF (NOMFAM.EQ.'OCTAEDRIQUE') THEN
C FCC LATTICE
        N(1,1)=1.D0
        N(1,2)=1.D0
        N(1,3)=1.D0
        N(2,1)=1.D0
        N(2,2)=1.D0
        N(2,3)=1.D0
        N(3,1)=1.D0
        N(3,2)=1.D0
        N(3,3)=1.D0
        N(4,1)=1.D0
        N(4,2)=-1.D0
        N(4,3)=1.D0
        N(5,1)=1.D0
        N(5,2)=-1.D0
        N(5,3)=1.D0
        N(6,1)=1.D0
        N(6,2)=-1.D0
        N(6,3)=1.D0
        N(7,1)=-1.D0
        N(7,2)=1.D0
        N(7,3)=1.D0
        N(8,1)=-1.D0
        N(8,2)=1.D0
        N(8,3)=1.D0
        N(9,1)=-1.D0
        N(9,2)=1.D0
        N(9,3)=1.D0
        N(10,1)=-1.D0
        N(10,2)=-1.D0
        N(10,3)=1.D0
        N(11,1)=-1.D0
        N(11,2)=-1.D0
        N(11,3)=1.D0
        N(12,1)=-1.D0
        N(12,2)=-1.D0
        N(12,3)=1.D0
        M(1,1)=-1.D0
        M(1,2)=0.D0
        M(1,3)=1.D0
        M(2,1)=0.D0
        M(2,2)=-1.D0
        M(2,3)=1.D0
        M(3,1)=-1.D0
        M(3,2)=1.D0
        M(3,3)=0.D0
        M(4,1)=-1.D0
        M(4,2)=0.D0
        M(4,3)=1.D0
        M(5,1)=0.D0
        M(5,2)=1.D0
        M(5,3)=1.D0
        M(6,1)=1.D0
        M(6,2)=1.D0
        M(6,3)=0.D0
        M(7,1)=0.D0
        M(7,2)=-1.D0
        M(7,3)=1.D0
        M(8,1)=1.D0
        M(8,2)=1.D0
        M(8,3)=0.D0
        M(9,1)=1.D0
        M(9,2)=0.D0
        M(9,3)=1.D0
        M(10,1)=-1.D0
        M(10,2)=1.D0
        M(10,3)=0.D0
        M(11,1)=1.D0
        M(11,2)=0.D0
        M(11,3)=1.D0
        M(12,1)=0.D0
        M(12,2)=1.D0
        M(12,3)=1.D0
C        N ET L DOIVENT ETRE UNITAIRES
        DO 20 J=1,12
          DO 10 K=1,3
            M(J,K)=M(J,K)/SQRT2
            N(J,K)=N(J,K)/SQRT3
   10     CONTINUE
   20   CONTINUE
      ELSEIF (NOMFAM.EQ.'CUBIQUE1') THEN
C        BCC LATTICE, {110} SLIP
        N(1,1)=1.D0
        N(1,2)=1.D0
        N(1,3)=0.D0
        N(2,1)=-1.D0
        N(2,2)=0.D0
        N(2,3)=1.D0
        N(3,1)=0.D0
        N(3,2)=-1.D0
        N(3,3)=-1.D0
        N(4,1)=0.D0
        N(4,2)=-1.D0
        N(4,3)=1.D0
        N(5,1)=1.D0
        N(5,2)=0.D0
        N(5,3)=-1.D0
        N(6,1)=-1.D0
        N(6,2)=1.D0
        N(6,3)=0.D0
        N(7,1)=-1.D0
        N(7,2)=-1.D0
        N(7,3)=0.D0
        N(8,1)=1.D0
        N(8,2)=0.D0
        N(8,3)=1.D0
        N(9,1)=0.D0
        N(9,2)=1.D0
        N(9,3)=-1.D0
        N(10,1)=1.D0
        N(10,2)=-1.D0
        N(10,3)=0.D0
        N(11,1)=-1.D0
        N(11,2)=0.D0
        N(11,3)=-1.D0
        N(12,1)=0.D0
        N(12,2)=1.D0
        N(12,3)=1.D0

        M(1,1)=1.D0
        M(1,2)=-1.D0
        M(1,3)=1.D0
        M(2,1)=1.D0
        M(2,2)=-1.D0
        M(2,3)=1.D0
        M(3,1)=1.D0
        M(3,2)=-1.D0
        M(3,3)=1.D0
        M(4,1)=1.D0
        M(4,2)=1.D0
        M(4,3)=1.D0
        M(5,1)=1.D0
        M(5,2)=1.D0
        M(5,3)=1.D0
        M(6,1)=1.D0
        M(6,2)=1.D0
        M(6,3)=1.D0
        M(7,1)=-1.D0
        M(7,2)=1.D0
        M(7,3)=1.D0
        M(8,1)=-1.D0
        M(8,2)=1.D0
        M(8,3)=1.D0
        M(9,1)=-1.D0
        M(9,2)=1.D0
        M(9,3)=1.D0
        M(10,1)=1.D0
        M(10,2)=1.D0
        M(10,3)=-1.D0
        M(11,1)=1.D0
        M(11,2)=1.D0
        M(11,3)=-1.D0
        M(12,1)=1.D0
        M(12,2)=1.D0
        M(12,3)=-1.D0

C        N ET L DOIVENT ETRE UNITAIRES
        DO 40 J=1,12
          DO 30 K=1,3
            M(J,K)=M(J,K)/SQRT3
            N(J,K)=N(J,K)/SQRT2
   30     CONTINUE
   40   CONTINUE
      ELSEIF (NOMFAM.EQ.'CUBIQUE2') THEN
C        BCC LATTICE, {211} SLIP
        N(1,1)=2.D0
        N(1,2)=-1.D0
        N(1,3)=1.D0
        N(2,1)=1.D0
        N(2,2)=-2.D0
        N(2,3)=-1.D0
        N(3,1)=1.D0
        N(3,2)=1.D0
        N(3,3)=2.D0
        N(4,1)=2.D0
        N(4,2)=1.D0
        N(4,3)=1.D0
        N(5,1)=1.D0
        N(5,2)=2.D0
        N(5,3)=-1.D0
        N(6,1)=1.D0
        N(6,2)=-1.D0
        N(6,3)=2.D0
        N(7,1)=2.D0
        N(7,2)=1.D0
        N(7,3)=-1.D0
        N(8,1)=1.D0
        N(8,2)=2.D0
        N(8,3)=1.D0
        N(9,1)=1.D0
        N(9,2)=-1.D0
        N(9,3)=-2.D0
        N(10,1)=2.D0
        N(10,2)=-1.D0
        N(10,3)=-1.D0
        N(11,1)=1.D0
        N(11,2)=-2.D0
        N(11,3)=1.D0
        N(12,1)=1.D0
        N(12,2)=1.D0
        N(12,3)=-2.D0
        M(1,1)=1.D0
        M(1,2)=1.D0
        M(1,3)=-1.D0
        M(2,1)=1.D0
        M(2,2)=1.D0
        M(2,3)=-1.D0
        M(3,1)=1.D0
        M(3,2)=1.D0
        M(3,3)=-1.D0
        M(4,1)=1.D0
        M(4,2)=-1.D0
        M(4,3)=-1.D0
        M(5,1)=1.D0
        M(5,2)=-1.D0
        M(5,3)=-1.D0
        M(6,1)=1.D0
        M(6,2)=-1.D0
        M(6,3)=-1.D0
        M(7,1)=1.D0
        M(7,2)=-1.D0
        M(7,3)=1.D0
        M(8,1)=1.D0
        M(8,2)=-1.D0
        M(8,3)=1.D0
        M(9,1)=1.D0
        M(9,2)=-1.D0
        M(9,3)=1.D0
        M(10,1)=1.D0
        M(10,2)=1.D0
        M(10,3)=1.D0
        M(11,1)=1.D0
        M(11,2)=1.D0
        M(11,3)=1.D0
        M(12,1)=1.D0
        M(12,2)=1.D0
        M(12,3)=1.D0
C        N ET L DOIVENT ETRE UNITAIRES
        DO 60 J=1,12
          DO 50 K=1,3
            M(J,K)=M(J,K)/SQRT3
            N(J,K)=N(J,K)/SQRT(6.D0)
   50     CONTINUE
   60   CONTINUE
      ELSEIF (NOMFAM.EQ.'BCC24') THEN
C        BCC LATTICE, {110} SLIP
        N(1,1)=0.D0
        N(1,2)=1.D0
        N(1,3)=1.D0
        N(2,1)=1.D0
        N(2,2)=0.D0
        N(2,3)=1.D0
        N(3,1)=1.D0
        N(3,2)=-1.D0
        N(3,3)=0.D0
        N(4,1)=0.D0
        N(4,2)=1.D0
        N(4,3)=-1.D0
        N(5,1)=1.D0
        N(5,2)=0.D0
        N(5,3)=1.D0
        N(6,1)=1.D0
        N(6,2)=1.D0
        N(6,3)=0.D0
        N(7,1)=0.D0
        N(7,2)=1.D0
        N(7,3)=1.D0
        N(8,1)=1.D0
        N(8,2)=0.D0
        N(8,3)=-1.D0
        N(9,1)=1.D0
        N(9,2)=1.D0
        N(9,3)=0.D0
        N(10,1)=0.D0
        N(10,2)=1.D0
        N(10,3)=-1.D0
        N(11,1)=1.D0
        N(11,2)=0.D0
        N(11,3)=-1.D0
        N(12,1)=1.D0
        N(12,2)=-1.D0
        N(12,3)=0.D0
        M(1,1)=1.D0
        M(1,2)=1.D0
        M(1,3)=-1.D0
        M(2,1)=1.D0
        M(2,2)=1.D0
        M(2,3)=-1.D0
        M(3,1)=1.D0
        M(3,2)=1.D0
        M(3,3)=-1.D0
        M(4,1)=1.D0
        M(4,2)=-1.D0
        M(4,3)=-1.D0
        M(5,1)=1.D0
        M(5,2)=-1.D0
        M(5,3)=-1.D0
        M(6,1)=1.D0
        M(6,2)=-1.D0
        M(6,3)=-1.D0
        M(7,1)=1.D0
        M(7,2)=-1.D0
        M(7,3)=1.D0
        M(8,1)=1.D0
        M(8,2)=-1.D0
        M(8,3)=1.D0
        M(9,1)=1.D0
        M(9,2)=-1.D0
        M(9,3)=1.D0
        M(10,1)=1.D0
        M(10,2)=1.D0
        M(10,3)=1.D0
        M(11,1)=1.D0
        M(11,2)=1.D0
        M(11,3)=1.D0
        M(12,1)=1.D0
        M(12,2)=1.D0
        M(12,3)=1.D0
        DO 80 J=1,12
          DO 70 K=1,3
            M(J,K)=M(J,K)/SQRT3
            N(J,K)=N(J,K)/SQRT2
   70     CONTINUE
   80   CONTINUE
C        BCC LATTICE, {211} SLIP
        N(13,1)=2.D0
        N(13,2)=-1.D0
        N(13,3)=1.D0
        N(14,1)=1.D0
        N(14,2)=-2.D0
        N(14,3)=-1.D0
        N(15,1)=1.D0
        N(15,2)=1.D0
        N(15,3)=2.D0
        N(16,1)=2.D0
        N(16,2)=1.D0
        N(16,3)=1.D0
        N(17,1)=1.D0
        N(17,2)=2.D0
        N(17,3)=-1.D0
        N(18,1)=1.D0
        N(18,2)=-1.D0
        N(18,3)=2.D0
        N(19,1)=2.D0
        N(19,2)=1.D0
        N(19,3)=-1.D0
        N(20,1)=1.D0
        N(20,2)=2.D0
        N(20,3)=1.D0
        N(21,1)=1.D0
        N(21,2)=-1.D0
        N(21,3)=-2.D0
        N(22,1)=2.D0
        N(22,2)=-1.D0
        N(22,3)=-1.D0
        N(23,1)=1.D0
        N(23,2)=-2.D0
        N(23,3)=1.D0
        N(24,1)=1.D0
        N(24,2)=1.D0
        N(24,3)=-2.D0

        M(13,1)=1.D0
        M(13,2)=1.D0
        M(13,3)=-1.D0
        M(14,1)=1.D0
        M(14,2)=1.D0
        M(14,3)=-1.D0
        M(15,1)=1.D0
        M(15,2)=1.D0
        M(15,3)=-1.D0
        M(16,1)=1.D0
        M(16,2)=-1.D0
        M(16,3)=-1.D0
        M(17,1)=1.D0
        M(17,2)=-1.D0
        M(17,3)=-1.D0
        M(18,1)=1.D0
        M(18,2)=-1.D0
        M(18,3)=-1.D0
        M(19,1)=1.D0
        M(19,2)=-1.D0
        M(19,3)=1.D0
        M(20,1)=1.D0
        M(20,2)=-1.D0
        M(20,3)=1.D0
        M(21,1)=1.D0
        M(21,2)=-1.D0
        M(21,3)=1.D0
        M(22,1)=1.D0
        M(22,2)=1.D0
        M(22,3)=1.D0
        M(23,1)=1.D0
        M(23,2)=1.D0
        M(23,3)=1.D0
        M(24,1)=1.D0
        M(24,2)=1.D0
        M(24,3)=1.D0
C        N ET L DOIVENT ETRE UNITAIRES
        DO 100 J=13,24
          DO 90 K=1,3
            M(J,K)=M(J,K)/SQRT3
            N(J,K)=N(J,K)/SQRT(6.D0)
   90     CONTINUE
  100   CONTINUE
      ELSEIF (NOMFAM.EQ.'UNIAXIAL') THEN
        N(1,1)=1.D0
        N(1,2)=0.D0
        N(1,3)=0.D0
        M(1,1)=1.D0
        M(1,2)=0.D0
        M(1,3)=0.D0
      ELSEIF (NOMFAM(1:4).EQ.'UTIL') THEN
        DO 120 I=1,NBSYS
          NORN=SQRT(TBSYS(I,1)**2+TBSYS(I,2)**2+TBSYS(I,3)**2)
          NORM=SQRT(TBSYS(I,4)**2+TBSYS(I,5)**2+TBSYS(I,6)**2)
          DO 110 J=1,3
            N(I,J)=TBSYS(I,J)/NORN
            M(I,J)=TBSYS(I,J+3)/NORM
  110     CONTINUE
  120   CONTINUE

      ENDIF
C     POUR LE SYSTEME K, EXPRESSION DE N ET L DANS REPERE GLOBAL
      K=NUSYS
      DO 130 J=1,3
        NL(J)=N(K,J)
        ML(J)=M(K,J)
  130 CONTINUE
      CALL UTPVLG(1,3,PGL2,NL,NG)
      CALL UTPVLG(1,3,PGL2,ML,MG)
C     rotation de reseau
      IF (IR.EQ.1) THEN
        CALL DCOPY(3,NG,1,NGR,1)
        CALL DCOPY(3,MG,1,MGR,1)
        CALL PMAVEC('ZERO',3,Q,NGR,NG)
        CALL PMAVEC('ZERO',3,Q,MGR,MG)
      ELSE
        CALL ASSERT(IR.EQ.0)
      ENDIF
      DO 140 J=1,3
        MUS(J)=NG(J)*MG(J)
  140 CONTINUE
C     SQRT(2) PAR HOMOGENEITE AVEC NMPL3D.
      MUS(4)=0.5D0*(NG(1)*MG(2)+NG(2)*MG(1))*SQRT2
      MUS(5)=0.5D0*(NG(1)*MG(3)+NG(3)*MG(1))*SQRT2
      MUS(6)=0.5D0*(NG(2)*MG(3)+NG(3)*MG(2))*SQRT2

  150 CONTINUE
      END
