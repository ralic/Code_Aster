      SUBROUTINE TSTVEC(IAD,LONG,TYPE,SOMMI,SOMMR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/09/2003   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_18 CRS_508
      IMPLICIT REAL*8 (A-H,O-Z)

C BUT : RECUPERER 2 NOMBRES RESUMANT UN VECTEUR JEVEUX

C IN: IAD   I  : ADRESSE DU VECTEUR
C IN: LONG  I  : LONGUEUR DU VECTEUR
C IN: TYPE  K3 : TYPE DES ELEMENTS DU VECTEUR :
C                   I/L/R/C/K8/K16/K24/K32/K80

C OUT: SOMMI   I      : SOMME(V(I)) QUELQUE SOIT LE TYPE DE V
C OUT: SOMMR   R      : SOMME(ABS(V(I))) SI V EST DE TYPE "R/C"


      CHARACTER*3 TYPE
      REAL*8 SOMMR
      INTEGER SOMMI
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C --------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------
      LOGICAL L
      CHARACTER*8 K8
      REAL*8 X
      INTEGER I,IX

      EQUIVALENCE (X,IX)
      EQUIVALENCE (L,IX)
      EQUIVALENCE (K8,IX)


      RUNDF = R8VIDE()


C     -- CALCUL DE SOMMR :
C     --------------------
      SOMMR = 0.D0
      IF (TYPE.EQ.'R') THEN
        DO 10,K = 1,LONG
          X = ZR(IAD-1+K)
          IF (X.NE.RUNDF) THEN
            IF (ABS(X).LT.1.D300) SOMMR = SOMMR + X
          END IF
   10   CONTINUE
      END IF
      IF (TYPE.EQ.'C') THEN
        DO 20,K = 1,LONG
          X = DBLE(ZC(IAD-1+K))
          IF (X.NE.RUNDF) THEN
            IF (ABS(X).LT.1.D300) SOMMR = SOMMR + X
          END IF
          X = DIMAG(ZC(IAD-1+K))
          IF (X.NE.RUNDF) THEN
            IF (ABS(X).LT.1.D300) SOMMR = SOMMR + X
          END IF
   20   CONTINUE
      END IF


C     -- CALCUL DE SOMMI :
C     --------------------
      SOMMI = 0
      IF (TYPE.EQ.'I') THEN
        DO 30,K = 1,LONG
          I = ZI(IAD-1+K)
          SOMMI = SOMMI + I
   30   CONTINUE
      ELSE IF (TYPE.EQ.'L') THEN
        DO 40,K = 1,LONG
          L = ZL(IAD-1+K)
          SOMMI = SOMMI + IX
   40   CONTINUE
      ELSE IF (TYPE.EQ.'R') THEN
        DO 50,K = 1,LONG
          X = ZR(IAD-1+K)
          SOMMI = SOMMI + IX
   50   CONTINUE
      ELSE IF (TYPE.EQ.'C') THEN
        DO 60,K = 1,LONG
          X = DBLE(ZC(IAD-1+K))
          SOMMI = SOMMI + IX
          X = DIMAG(ZC(IAD-1+K))
          SOMMI = SOMMI + IX
   60   CONTINUE
      ELSE IF (TYPE.EQ.'K8') THEN
        DO 70,K = 1,LONG
          K8 = ZK8(IAD-1+K)
          SOMMI = SOMMI + IX
   70   CONTINUE
      ELSE IF (TYPE.EQ.'K16') THEN
        DO 80,K = 1,LONG
          K8 = ZK16(IAD-1+K) (1:8)
          SOMMI = SOMMI + IX
          K8 = ZK16(IAD-1+K) (9:16)
          SOMMI = SOMMI + IX
   80   CONTINUE
      ELSE IF (TYPE.EQ.'K24') THEN
        DO 90,K = 1,LONG
          K8 = ZK24(IAD-1+K) (1:8)
          SOMMI = SOMMI + IX
          K8 = ZK24(IAD-1+K) (9:16)
          SOMMI = SOMMI + IX
          K8 = ZK24(IAD-1+K) (17:24)
          SOMMI = SOMMI + IX
   90   CONTINUE
      ELSE IF (TYPE.EQ.'K32') THEN
        DO 100,K = 1,LONG
          K8 = ZK32(IAD-1+K) (1:8)
          SOMMI = SOMMI + IX
          K8 = ZK32(IAD-1+K) (9:16)
          SOMMI = SOMMI + IX
          K8 = ZK32(IAD-1+K) (17:24)
          SOMMI = SOMMI + IX
          K8 = ZK32(IAD-1+K) (25:32)
          SOMMI = SOMMI + IX
  100   CONTINUE
      ELSE IF (TYPE.EQ.'K80') THEN
        DO 110,K = 1,LONG
          K8 = ZK80(IAD-1+K) (1:8)
          SOMMI = SOMMI + IX
          K8 = ZK80(IAD-1+K) (9:16)
          SOMMI = SOMMI + IX
          K8 = ZK80(IAD-1+K) (17:24)
          SOMMI = SOMMI + IX
          K8 = ZK80(IAD-1+K) (25:32)
          SOMMI = SOMMI + IX
          K8 = ZK80(IAD-1+K) (33:40)
          SOMMI = SOMMI + IX
          K8 = ZK80(IAD-1+K) (41:48)
          SOMMI = SOMMI + IX
          K8 = ZK80(IAD-1+K) (49:56)
          SOMMI = SOMMI + IX
          K8 = ZK80(IAD-1+K) (57:64)
          SOMMI = SOMMI + IX
          K8 = ZK80(IAD-1+K) (65:72)
          SOMMI = SOMMI + IX
          K8 = ZK80(IAD-1+K) (73:80)
          SOMMI = SOMMI + IX
  110   CONTINUE
      END IF


      END
