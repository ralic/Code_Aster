      SUBROUTINE MLTDCA(NBLOC,LGBLOC,NCBLOC,DECAL,SEQ,NBSN,NBND,SUPND,
     +                  ADRESS,GLOBAL,LGSN,FACTOL,FACTOU,SM,X,
     +                  INVP,PERM,AD,TRAV,TYPSYM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 07/01/2002   AUTEUR JFBHHUC C.ROSE 
C RESPONSABLE JFBHHUC C.ROSE
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
C     VERSION COMPLEXE DE MLTDRA
      IMPLICIT NONE
      INTEGER NBSN,NBND,NBLOC,LGBLOC(NBSN),NCBLOC(NBND),DECAL(NBSN)
      INTEGER SEQ(NBSN),SUPND(NBSN+1),GLOBAL(9515),LGSN(NBSN)
      INTEGER ADRESS(NBSN+1),INVP(NBND),PERM(NBND),AD(NBND)
      INTEGER NBSOL,TYPSYM
      COMPLEX*16 SM(NBND),X(NBND),TRAV(NBND)
      CHARACTER*32 JEXNUM
      CHARACTER*24 FACTOL,FACTOU,FACTOR
      INTEGER IL,K0
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER IB,NC,ISND,LONG,L,I,NDJ
C
      INTEGER SEUIN,SEUIK
      PARAMETER(SEUIN=1500,SEUIK=300)
      INTEGER LDA,NN,KK
      INTEGER ADFAC1,DEB1
      INTEGER SNI,K,J,DEB,FIN,ADFAC,NDK,GJ,DEBNDK,IFAC
      COMPLEX*16 S
      CALL JEMARQ()

      DO 110 J = 1,NBND
          X(INVP(J)) = SM(J)
  110 CONTINUE
      DO 120 J = 1,NBND
          SM(J) = X(J)
  120 CONTINUE
C     DESCENTE  L * Y = B
      ISND = 0
      DO 180 IB = 1,NBLOC
          CALL JEVEUO(JEXNUM(FACTOL,IB),'L',IFAC)
          ADFAC = IFAC - 1
          DO 170 NC = 1,NCBLOC(IB)
              ISND = ISND + 1
              SNI = SEQ(ISND)
              LONG = ADRESS(SNI+1) - ADRESS(SNI)
              L = LGSN(SNI)
              K = 1
              DO 130 I = ADRESS(SNI),ADRESS(SNI+1) - 1
                   TRAV(K) = X(GLOBAL(I))
                  K = K + 1
  130         CONTINUE
              AD(1) = DECAL(SNI)
              NDJ = SUPND(SNI) - 1
              DO 150 J = 1,L - 1
                  NDJ = NDJ + 1
C                 RANGEMENT DU TERME DIAGONAL
                  SM(NDJ) = ZC(IFAC-1+AD(J))

CRAY DIR$ IVDEP
                  K = 1
                  DO 140 I = J + 1,L
                     TRAV(I) = TRAV(I) -
     +                    ZC(IFAC-1+AD(J)+K)*TRAV(J)
                      K = K + 1
  140             CONTINUE
CMODIF POUR SGEMV   AD(J+1) = AD(J) + LONG - J + 1
CMODIF POUR SGEMV AD(J) = AD(J) + L - J + 1
                  AD(J+1) = AD(J) + LONG  + 1
                  AD(J) = AD(J) + L - J +1
  150         CONTINUE
              NDJ = NDJ + 1
C                 RANGEMENT DU TERME DIAGONAL
              SM(NDJ) = ZC(IFAC-1+ AD(L))
              AD(L) = AD(L) + 1
              IF (LONG.GT.L) THEN
C                  CALL SSPMVC(LONG-L,L,ZC(IFAC),AD,TRAV,
C     +                         TRAV(L+1))
                 NN= LONG - L
                 KK= L
                 LDA = LONG
                 IF(NN.LT.SEUIN.OR.KK.LT.SEUIK) THEN
                    CALL SSPMVC(NN,KK,ZC(IFAC),AD,TRAV,TRAV(L+1))
                 ELSE
                   CALL CGEMW(NN,KK,ZC(IFAC+AD(1)-1),LDA,TRAV,TRAV(L+1))
                  ENDIF
              END IF
              K = 1
              DO 160 I = ADRESS(SNI),ADRESS(SNI+1) - 1
                  X(GLOBAL(I)) = TRAV(K)
                  K = K + 1
  160         CONTINUE
  170     CONTINUE
          CALL JELIBE(JEXNUM(FACTOL,IB))
  180 CONTINUE

      IF( TYPSYM.NE.0 ) THEN
         FACTOR = FACTOL
         DEB1=1
      ELSE
         FACTOR = FACTOU
         DEB1 = NBND
      ENDIF
C=======================================================================
C     D * Z = Y
      DO 190 J = DEB1,NBND
            X(J) = X(J)/SM(J)
 190     CONTINUE
C=======================================================================
C     REMONTEE  U * X = Z
      ISND = NBSN + 1
      DO 260 IB = NBLOC,1,-1
          CALL JEVEUO(JEXNUM(FACTOR,IB),'L',IFAC)
          IF (IB.NE.NBLOC) THEN
              ADFAC = LGBLOC(IB) + IFAC
          ELSE
              ADFAC = LGBLOC(IB) + IFAC - LGSN(NBSN)
          END IF
          DO 250 NC = 1,NCBLOC(IB)
              ISND = ISND - 1
              SNI = SEQ(ISND)
              L = LGSN(SNI)
              FIN = ADRESS(SNI+1) - 1
              IF (SNI.EQ.NBSN) THEN
                  DEBNDK = SUPND(SNI+1) - 2
                  DEB = ADRESS(SNI) + LGSN(SNI) - 1
                  IL = L - 1
              ELSE
                  DEB = ADRESS(SNI) + LGSN(SNI)
                  DEBNDK = SUPND(SNI+1) - 1
                  IL = L
              END IF
              IF (L.GT.1) THEN
                  K = 1
                  DO 200 I = ADRESS(SNI),ADRESS(SNI+1) - 1
                      TRAV(K) = X(GLOBAL(I))
                      K = K + 1
  200             CONTINUE
                  K0 = K
              END IF
              DO 230 NDK = DEBNDK,SUPND(SNI),-1
                  S = 0.D0
                  IF (L.GT.1) THEN
                      K = K0
CRAY DIR$ IVDEP
                      DO 210 J = FIN,DEB,-1
                          ADFAC = ADFAC - 1
                          K = K - 1
                      S = S + ZC(ADFAC)*TRAV(K)
  210                 CONTINUE
                      DEB = DEB - 1
                      ADFAC = ADFAC - 1
                      TRAV(IL) =TRAV(IL) - S
                      IF( TYPSYM.EQ.0) TRAV(IL) =TRAV(IL) /ZC(ADFAC)
C DECALAGE  POUR SGEMV
                      ADFAC = ADFAC - (NDK-SUPND(SNI))
                  ELSE
                      K = K0
                      DO 220 J = FIN,DEB,-1
                          GJ = GLOBAL(J)
                          ADFAC = ADFAC - 1
                          K = K - 1
                      S = S + ZC(ADFAC)*X(GJ)
  220                 CONTINUE
                      DEB = DEB - 1
                      ADFAC = ADFAC - 1
                      X(NDK) = X(NDK) - S
                      IF( TYPSYM.EQ.0) X(NDK) = X(NDK) /ZC(ADFAC)
C DECALAGE  POUR SGEMV
                      ADFAC = ADFAC - (NDK-SUPND(SNI))
                  END IF
                  IL = IL - 1
  230         CONTINUE
              IF (L.GT.1) THEN
                  K = 1
                  DO 240 I = ADRESS(SNI),ADRESS(SNI+1) - 1
                      X(GLOBAL(I)) = TRAV(K)
                      K = K + 1
  240             CONTINUE
              END IF
  250     CONTINUE
          CALL JELIBE(JEXNUM(FACTOR,IB))
  260 CONTINUE
C     ON RANGE DANS SM  LA SOLUTION DANS LA NUMEROTATION INITIALE
      DO 270 J = 1,NBND
          SM(PERM(J)) = X(J)
 270  CONTINUE
      CALL JEDEMA()
      END
