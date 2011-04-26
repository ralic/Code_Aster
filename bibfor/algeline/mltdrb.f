      SUBROUTINE MLTDRB(NBLOC,NCBLOC,DECAL,SEQ,NBSN,NBND,SUPND,
     +     ADRESS,GLOBAL,LGSN,FACTOL,FACTOU,X,TEMP,
     +     INVP,PERM,AD,TRAV,TYPSYM,NBSM,S)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     TOLE CRP_21 CRP_4
      IMPLICIT NONE
      INTEGER NBSN,NBND,NBLOC,NCBLOC(NBND),DECAL(NBSN)
      INTEGER*4 GLOBAL(*)
      INTEGER SEQ(NBSN),SUPND(NBSN+1),LGSN(NBSN)
      INTEGER ADRESS(NBSN+1),INVP(NBND),PERM(NBND),AD(NBND)
      INTEGER TYPSYM,NBSM
      REAL*8 TEMP(NBND),X(NBND,NBSM),TRAV(NBND,NBSM),S(NBSM)
      CHARACTER*32 JEXNUM
      CHARACTER*24 FACTOL,FACTOU,FACTOR
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
      INTEGER IB,NC,ISND,LONG,L,I,NDJ,P
C
      INTEGER DEB1
      INTEGER SNI,K,J,DEB,IFAC,ISM
      INTEGER SEUIL,TRANCH,NPROC,LARG,MLNBPR
      INTEGER OPTA,OPTB
      CALL JEMARQ()
      OPTB=1
      NPROC=MLNBPR()
      TRANCH = (NBSM + NPROC - 1) /NPROC
      SEUIL = NPROC - MOD(TRANCH*NPROC-NBSM,NPROC)
      DO 130 ISM=1,NBSM
         DO 110 J = 1,NBND
            TEMP(INVP(J)) = X(J,ISM)
 110     CONTINUE
         DO 120 J = 1,NBND
            X(J,ISM) = TEMP(J)
 120     CONTINUE
 130  CONTINUE

C     DESCENTE  L * Y = B
      ISND = 0
      DO 180 IB = 1,NBLOC
         CALL JEVEUO(JEXNUM(FACTOL,IB),'L',IFAC)
         DO 170 NC = 1,NCBLOC(IB)
            ISND = ISND + 1
            SNI = SEQ(ISND)
            LONG = ADRESS(SNI+1) - ADRESS(SNI)
            L = LGSN(SNI)
            DO 135 ISM=1,NBSM
               K = 1
               DO 125 I = ADRESS(SNI),ADRESS(SNI+1) - 1
                  TRAV(K,ISM) = X(GLOBAL(I),ISM)
                  K = K + 1
 125           CONTINUE
 135        CONTINUE
            AD(1) = DECAL(SNI)
            NDJ = SUPND(SNI) - 1
            DO 150 J = 1,L - 1
               NDJ = NDJ + 1
C     CALCUL DU BLOC  DIAGONAL
               TEMP(NDJ) = ZR(IFAC-1+AD(J))
               DO 145 ISM=1,NBSM
                  K = 1
                  DO 140 I = J + 1,L
                     TRAV(I,ISM) = TRAV(I,ISM) -
     +                    ZR(IFAC-1+AD(J)+K) *TRAV(J,ISM)
                     K = K + 1
 140              CONTINUE
 145           CONTINUE
               AD(J+1) = AD(J) + LONG + 1
               AD(J) = AD(J) + L - J + 1
 150        CONTINUE
            NDJ = NDJ + 1
C     RANGEMENT DU TERME DIAGONAL
            TEMP(NDJ) = ZR(IFAC-1+ AD(L))
            AD(L) = AD(L) + 1
            IF (LONG.GT.L) THEN
               P = L
               OPTA=1
               DO 152 ISM=1,NPROC
                  IF(ISM.GT.SEUIL) THEN
                     LARG=TRANCH - 1
                     DEB = SEUIL*TRANCH + (ISM -SEUIL-1)*LARG+ 1
                  ELSE
                     DEB = (ISM-1)*TRANCH + 1
                     LARG=TRANCH
                  ENDIF
C APPEL AU PRODUIT PAR BLOCS
                  CALL MLFMUL(TRAV(P+1,DEB),ZR(IFAC+AD(1)-1),
     +                 TRAV(1,DEB),NBND,LONG,P,LARG,OPTA,OPTB)
 152           CONTINUE
            ENDIF
            DO 153 ISM=1,NBSM
               K = 1
               DO 160 I = ADRESS(SNI),ADRESS(SNI+1) - 1
                  X(GLOBAL(I),ISM) = TRAV(K,ISM)
                  K = K + 1
 160           CONTINUE
 153        CONTINUE
 170     CONTINUE
         CALL JELIBE(JEXNUM(FACTOL,IB))
 180  CONTINUE

      IF( TYPSYM.NE.0 ) THEN
         FACTOR = FACTOL
         DEB1=1
      ELSE
         FACTOR = FACTOU
C     ON DIVISE PAR LE TERME DIAGONAL DANS LA REMONTEE EN NON-SYMETRIQUE
         DEB1 = NBND +1
      ENDIF
C=======================================================================
C     D * Z = Y
      DO 194 ISM=1,NBSM
         DO 190 J = DEB1,NBND
            X(J,ISM) = X(J,ISM)/TEMP(J)
 190     CONTINUE
 194  CONTINUE
C=======================================================================
C     REMONTEE  U * X = Z
      ISND = NBSN + 1
      OPTA=0
      DO 260 IB = NBLOC,1,-1
         CALL JEVEUO(JEXNUM(FACTOR,IB),'L',IFAC)
         DO 250 NC = 1,NCBLOC(IB)
            ISND = ISND - 1
            SNI = SEQ(ISND)
            L = LGSN(SNI)
            LONG = ADRESS(SNI+1) - ADRESS(SNI)
            DEB = ADRESS(SNI) + LGSN(SNI)
            DO 205 ISM=1,NBSM
               K = 1
               DO 200 I = ADRESS(SNI),ADRESS(SNI+1) - 1
                  TRAV(K,ISM) = X(GLOBAL(I),ISM)
                  K = K + 1
 200           CONTINUE
 205        CONTINUE
            P=L
            LARG=NBSM
            DEB=1
            IF(LONG.GT.P) THEN
C APPEL AU PRODUIT PAR BLOCS
               CALL MLFMLT(TRAV(1,DEB),ZR(IFAC-1 + DECAL(SNI) + P),
     +              TRAV(P+1,DEB),NBND,LONG,P,LARG,OPTA,OPTB)
            ENDIF
C     PARTIE DIAGONALE
            AD(1)=DECAL(SNI)
            DO 300 J = 1,L-1
               AD(J+1) = AD(J)+ LONG+1
 300        CONTINUE
            DO 350 J = L ,1,-1

               DO 345 ISM=1,NBSM
                  K = 1
                  S(ISM) =0.D0
                  DO 340 I = J + 1,L
                     S(ISM) =S(ISM) +
     +                    ZR(IFAC-1+AD(J)+K) *TRAV(I,ISM)
                     K = K + 1
 340              CONTINUE
                  TRAV(J,ISM) = TRAV(J,ISM) - S(ISM)
                 IF(TYPSYM.EQ.0) THEN
                     TRAV(J,ISM)= TRAV(J,ISM)/ZR(IFAC-1+AD(J))
               ENDIF
 345           CONTINUE
 350        CONTINUE
            DO 221 ISM=1,NBSM
               K = 1
               DO 240 I = ADRESS(SNI),ADRESS(SNI+1) - 1
                  X(GLOBAL(I),ISM) = TRAV(K,ISM)
                  K = K + 1
 240           CONTINUE
 221        CONTINUE
 250     CONTINUE
         CALL JELIBE(JEXNUM(FACTOR,IB))
 260  CONTINUE
C     ON RANGE DANS SM  LA SOLUTION DANS LA NUMEROTATION INITIALE
      DO 265 ISM=1,NBSM
         DO 270 J = 1,NBND

            TEMP(PERM(J)) = X(J,ISM)
 270     CONTINUE

         DO 275 J = 1,NBND
            X(J,ISM) = TEMP(J)
 275     CONTINUE
 265  CONTINUE
      CALL JEDEMA()
      END
