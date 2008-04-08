      SUBROUTINE ARLTE(NDIM  ,NG    ,PG    ,
     &                 F1    ,DF1   ,NN1   ,L1,
     &                 F2    ,DF2   ,NN2   ,L2)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      INTEGER NDIM
      INTEGER NG
      REAL*8  PG(NG)
      INTEGER NN1
      REAL*8  F1(NN1,*),DF1(NDIM,NN1,*)
      REAL*8  L1(*)
      INTEGER NN2
      REAL*8  F2(NN2,*),DF2(NDIM,NN2,*)
      REAL*8  L2(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C MATRICES ELEMENTAIRES DE COUPLAGE
C
C ----------------------------------------------------------------------
C
C
C MAILLE 1 : SUPPORT DES MULTIPLICATEURS
C
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  NN1    : NOMBRE DE NOEUDS MAILLE 1
C IN  NN2    : NOMBRE DE NOEUDS MAILLE 2
C IN  NG     : NOMBRE DE POINTS D'INTEGRATION
C IN  PG     : POIDS D'INTEGRATION
C IN  F1     : FNCTS FORME PTS D'INTEGRATION MAILLE 1
C IN  DF1    : DERIVEES FNCTS FORME ASSOCIEES
C IN  F2     : FNCTS FORME PTS D'INTEGRATION MAILLE 2
C IN  DF2    : DERIVEES FNCT FORME ASSOCIEES
C I/O L1     : MATRICES ELEMENTAIRES MAILLE 1
C               DIM: ((1+NDIM*NDIM)*NN1*NN1)
C I/O L2     : MATRICES ELEMENTAIRES MAILLE 2
C               DIM: ((1+NDIM*NDIM)*NN1*NN2)
C
C STRUCTURE DE L* :
C ( W1.1*W2.1  , W1.1*W2.2  , ..., W1.2*W2.1, W1.2*W2.2, ...,
C   W1.1X*W2.1X, W1.1Y*W2.1X, [W1.1Z*W2.1X], W1.1X*W2.1Y, ...,
C   W1.1X*W2.2X, ..., W1.2X*W2.1X, ... )
C
C ----------------------------------------------------------------------
C
      INTEGER NN11,NN12,P1,P2,P3,P4
      INTEGER I,J,K,L,M
      REAL*8  R0,R1,R2
C
C ----------------------------------------------------------------------
C
      NN11 = NN1*NN1
      NN12 = NN1*NN2
C
C --- FORMULE D'INTEGRATION
C

      DO 99 I = 1, NG

        R0 = PG(I)

        P1 = 1
        P2 = 1
        P3 = NN11
        P4 = NN12
C
C --- INTEGRATION MATRICES L1
C
        DO 10 J = 1, NN1
          R1 = R0 * F1(J,I)

          DO 20 K = 1, NN1
            L1(P1) = L1(P1) + R1 * F1(K,I)
            P1 = P1 + 1

            DO 21 L = 1, NDIM
              R2 = R0 * DF1(L,K,I)




              DO 22 M = 1, NDIM

                P3 = P3 + 1
                L1(P3) = L1(P3) + R2*DF1(M,J,I)

  22          CONTINUE

  21        CONTINUE

  20      CONTINUE
C
C --- INTEGRATION MATRICES L2
C
          DO 30 K = 1, NN2

            L2(P2) = L2(P2) + R1 * F2(K,I)
            P2 = P2 + 1

            DO 31 L = 1, NDIM

              R2 = R0 * DF2(L,K,I)

              DO 32 M = 1, NDIM
                P4 = P4 + 1
                L2(P4) = L2(P4) + R2*DF1(M,J,I)

 32           CONTINUE

 31         CONTINUE

 30       CONTINUE


 10    CONTINUE


 99   CONTINUE



      END
