      SUBROUTINE ARLTE(DIM,PG,NG,F1,DF1,NN1,F2,DF2,NN2,L1,L2)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C ---------------------------------------------------------------------
C               MATRICES ELEMENTAIRES DE COUPLAGE ARLEQUIN
C ---------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER  DIM                 : DIMENSION DE L'ESPACE
C REAL*8   PG(NG)              : POIDS D'INTEGRATION
C INTEGER  NG                  : NOMBRE DE POINTS D'INTEGRATION
C REAL*8   F1(NN1,NG)          : FNCTS FORME PTS D'INTEGRATION  MAILLE 1
C REAL*8   DF1(DIM,NN1,NG)     : DERIVEES FNCTS FORME ASSOCIEES
C INTEGER  NN1                 : NOMBRE DE NOEUDS DE LA MAILLE 1
C REAL*8   F2(NN2,NG)          : FNCTS FORME PTS D'INTEGRATION MAILLE 2
C REAL*8   DF2(DIM,NN2,NG)     : DERIVEES FNCT FORME ASSOCIEES
C INTEGER  NN2                 : NOMBRE DE NOEUDS DE LA MAILLE 2
C
C          MAILLE 1 : SUPPORT DES MULTIPLICATEURS 
C
C VARIABLES D'ENTREE / SORTIE
C REAL*8   L1((1+DIM*DIM)*NN1*NN1) : MATRICES ELEMENTAIRES MAILLE 1
C REAL*8   L2((1+DIM*DIM)*NN1*NN2) : MATRICES ELEMENTAIRES MAILLE 2
C
C STRUCTURE DE L* :
C ( W1.1*W2.1, W1.1*W2.2, ..., W1.2*W2.1, W1.2*W2.2, ...,
C   W1.1X*W2.1X, W1.1Y*W2.1X, [W1.1Z*W2.1X], W1.1X*W2.1Y, ...,
C   W1.1X*W2.2X, ..., W1.2X*W2.1X, ... )
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES 

      INTEGER DIM,NG,NN1,NN2,NN11,NN12,P1,P2,P3,P4,I,J,K,L,M
      REAL*8  PG(*),F1(NN1,*),DF1(DIM,NN1,*),F2(NN2,*),DF2(DIM,NN2,*)
      REAL*8  L1(*),L2(*),R0,R1,R2

      NN11 = NN1*NN1
      NN12 = NN1*NN2

C --- FORMULE D'INTEGRATION

      DO 10 I = 1, NG

        R0 = PG(I)

        P1 = 1
        P2 = 1
        P3 = NN11
        P4 = NN12

C ----- INTEGRATION MATRICES L1

        DO 10 J = 1, NN1

          R1 = R0 * F1(J,I)

          DO 20 K = 1, NN1

            L1(P1) = L1(P1) + R1 * F1(K,I)
            P1 = P1 + 1

            DO 20 L = 1, DIM

              R2 = R0 * DF1(L,K,I)

              DO 20 M = 1, DIM

                P3 = P3 + 1
                L1(P3) = L1(P3) + R2*DF1(M,J,I)

 20       CONTINUE

C ------- INTEGRATION MATRICES L2

          DO 10 K = 1, NN2

            L2(P2) = L2(P2) + R1 * F2(K,I)
            P2 = P2 + 1
            
            DO 10 L = 1, DIM

              R2 = R0 * DF2(L,K,I)

              DO 10 M = 1, DIM

                P4 = P4 + 1
                L2(P4) = L2(P4) + R2*DF1(M,J,I)

 10   CONTINUE

      END
