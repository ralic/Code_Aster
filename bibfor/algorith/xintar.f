      SUBROUTINE XINTAR(ELP,NDIM,IA,TABCO,TABLS,INTAR)
      IMPLICIT NONE

      INTEGER       IA,NDIM
      CHARACTER*8   ELP
      REAL*8        INTAR(NDIM),TABCO(*),TABLS(*)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 31/01/2012   AUTEUR REZETTE C.REZETTE 
C TOLE CRS_1404
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
C                      TROUVER LE PT D'INTERSECTION ENTRE L'ARETE
C                      ET LA FISSURE
C
C     ENTREE
C       ELP     : TYPE DE L'ELEMENT
C       IA      : NUMERO DE L'ARETE REPEREE SUR L'ELEMENT
C       TABCO   : COORDONNEES DES NOEUDS DE L'ELEMENT
C       TABLS   : VALEUR DES LSN DES NOEUDS DE L'ELEMENT
C
C     SORTIE
C       INTAR   : COORDONNÉES DES POINTS D'INTERSECTION
C     ----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  ------------------------
C
      REAL*8          C1,C2,C3,A,B,C,D,SOL,SOL1,SOL2,MIN,MAX
      REAL*8          XE(NDIM),R8PREM,CRIVAL
      INTEGER         NNO
      PARAMETER       (CRIVAL=1.D-8)
C
C---------------------------------------------------------------------
C     DEBUT
C---------------------------------------------------------------------
      CALL JEMARQ()

      SOL=0.D0
      SOL1=0.D0
      SOL2=0.D0
      CALL VECINI(NDIM,0.D0,XE)

      IF(ELP.EQ.'TR6') THEN
        MAX=1.D0
        MIN=0.D0
        IF (IA.EQ.1) THEN
C ARETE 1-2-4
          C1=TABLS(2)
          C2=TABLS(4)
          C3=TABLS(1)
        ELSEIF (IA.EQ.2) THEN
C ARETE 2-3-5
          C1=TABLS(2)
          C2=TABLS(5)
          C3=TABLS(3)
        ELSEIF (IA.EQ.3) THEN
C ARETE 3-1-6
          C1=TABLS(3)
          C2=TABLS(6)
          C3=TABLS(1)
        ENDIF
        A = 2*C1-4*C2+2*C3
        B = -C1+4*C2-3*C3
        C = C3

      ELSEIF(ELP.EQ.'SE3')THEN
        MAX=1.D0
        MIN=-1.D0
        C1=TABLS(1)
        C2=TABLS(2)
        C3=TABLS(3)
        A = C1/2+C2/2-C3
        B = (C2-C1)/2
        C = C3

      ELSEIF(ELP.EQ.'QU8')THEN
        MAX=1.D0
        MIN=-1.D0
        IF (IA.EQ.1) THEN
C ARETE 1-2-5
          C1=TABLS(1)
          C2=TABLS(2)
          C3=TABLS(5)
        ELSEIF (IA.EQ.2) THEN
C ARETE 2-3-6
          C1=TABLS(2)
          C2=TABLS(3)
          C3=TABLS(6)
        ELSEIF (IA.EQ.3) THEN
C ARETE 3-4-7
          C1=TABLS(4)
          C2=TABLS(3)
          C3=TABLS(7)
        ELSEIF (IA.EQ.4) THEN
C ARETE 4-1-8
          C1=TABLS(1)
          C2=TABLS(4)
          C3=TABLS(8)
        ENDIF
        A = C1/2+C2/2-C3
        B = -C1/2+C2/2
        C = C3

      ELSE
        CALL ASSERT(1.EQ.2)
      ENDIF

      D = B*B-4*A*C

      IF (ABS(A).LE.CRIVAL.AND. ABS(B).GT.CRIVAL) THEN
        SOL=-C/B
      ELSEIF (ABS(A).GT.CRIVAL) THEN
        IF (ABS(D).LE.R8PREM()) THEN
          SOL=-B/(2*A)
        ELSEIF (D.GT.R8PREM()) THEN
          SOL1=(-B-SQRT(D))/(2*A)
          SOL2=(-B+SQRT(D))/(2*A)
          IF (SOL1.GT.MIN .AND. SOL1.LT.MAX) THEN
            SOL=SOL1
          ELSE
            IF (SOL2.GT.MIN .AND. SOL2.LT.MAX) THEN
              SOL=SOL2
            ELSE
              CALL ASSERT(1.EQ.2)
            ENDIF
          ENDIF
        ELSEIF (D.LT.-R8PREM()) THEN
C       LE POLYNOME N'A PAS DE SOLUTION
        CALL U2MESS('F','XFEM_65')
        ENDIF
      ENDIF

      IF(ELP.EQ.'TR6')THEN
        NNO=6
        IF (IA.EQ.1) THEN
C ARETE 1-2-4
          XE(1)=SOL
        ELSEIF (IA.EQ.2) THEN
C ARETE 2-3-5
          XE(1)=SOL
          XE(2)=1-XE(1)
        ELSEIF (IA.EQ.3) THEN
C ARETE 3-1-6
          XE(2)=SOL
        ENDIF

      ELSEIF(ELP.EQ.'SE3')THEN
        NNO=3
        XE(1)=SOL

      ELSEIF(ELP.EQ.'QU8')THEN
        NNO=8
        IF (IA.EQ.1) THEN
C ARETE 1-2-5
          XE(1)=SOL
          XE(2)=-1
        ELSEIF (IA.EQ.2) THEN
C ARETE 2-3-6
          XE(1)=1
          XE(2)=SOL
        ELSEIF (IA.EQ.3) THEN
C ARETE 3-4-7
          XE(1)=SOL
          XE(2)=1
        ELSEIF (IA.EQ.4) THEN
C ARETE 4-1-8
          XE(1)=-1
          XE(2)=SOL
        ENDIF
      ELSE
        CALL ASSERT(1.EQ.2)
      ENDIF

      CALL REEREL(ELP,NNO,NDIM,TABCO,XE,INTAR)

C---------------------------------------------------------------------
C     FIN
C---------------------------------------------------------------------
      CALL JEDEMA()
      END
