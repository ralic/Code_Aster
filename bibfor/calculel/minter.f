      SUBROUTINE MINTER(DIM,M1,M2,D1,D2,MM1,MM2,PAN1,PAN2,IR)

      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C A_UTIL
C ----------------------------------------------------------------------
C     TEST APPROCHE D'INTERSECTION DE LA MAILLE M1 AVEC LA MAILLE M2
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER  DIM      : DIMENSION DE L'ESPACE
C INTEGER  M1       : MAILLE 1
C INTEGER  M2       : MAILLE 2
C INTEGER  D1(*)    : SD BOITE.DIME ASSOCIEE A M1 (CF BOITE)
C INTEGER  D2(*)    : SD BOITE.DIME ASSOCIEE A M2 (CF BOITE)
C REAL*8   MM1(*)   : SD BOITE.MINMAX ASSOCIEE A M1 (CF BOITE)
C REAL*8   MM2(*)   : SD BOITE.MINMAX ASSOCIEE A M2 (CF BOITE)
C REAL*8   PAN1(*)  : SD BOITE.PAN ASSOCIEE A M1 (CF BOITE)
C REAL*8   PAN2(*)  : SD BOITE.PAN ASSOCIEE A M2 (CF BOITE)
C
C VARIABLE DE SORTIE
C LOGICAL  IR       : .TRUE. SI CONVEXES ENGLOBANT M1 ET M2 S'INTERSECT
C ----------------------------------------------------------------------

C --- VARIABLES
      LOGICAL IR
      INTEGER M1,M2,DIM,D1(*),D2(*),NP1,NP2,I,J,K,NS,P1,P2
      REAL*8  MM1(*),MM2(*),PAN1(DIM+2,*),PAN2(DIM+2,*)
      REAL*8  ZERO,A(4,9),B(4,3),C(9),S2(9),R0,R1

      ZERO = 0.D0

C --- INTERSECTION MINMAX

      IR = .FALSE.
      P1 = 2*DIM*(M1-1)
      P2 = 2*DIM*(M2-1)

      DO 10 I = 1, DIM
        P1 = P1 + 1
        P2 = P2 + 1
        R0 = MAX(MM1(P1),MM2(P2))
        P1 = P1 + 1
        P2 = P2 + 1
        R1 = MIN(MM1(P1),MM2(P2))
        IF (R0.GT.R1) GOTO 80
 10   CONTINUE

C --- INTERSECTION PAN

      IR = .TRUE.

      P1 = D1(1+2*M1)
      NP1 = D1(3+2*M1) - P1
      P1 = P1 - 1
      P2 = D2(1+2*M2)
      NP2 = D2(3+2*M2) - P2
      P2 = P2 - 1

      DO 20 J = 1, DIM
        DO 20 I = 1, DIM
          B(I,J) = -PAN1(I,P1+J)
 20   CONTINUE

      K = 0
      DO 30 J = J, NP1
        K = K + 1
        DO 40 I = 1, DIM
          A(I,K) = PAN1(I,P1+J)
 40     CONTINUE
        S2(K) = PAN1(I,P1+J)
 30   CONTINUE

      DO 50 J = 1, NP2
        K = K + 1
        DO 60 I = 1, DIM
          A(I,K) = PAN2(I,P2+J)
 60     CONTINUE
        S2(K) = PAN2(I,P2+J)
 50   CONTINUE

      NS = NP1 + NP2 - DIM

      CALL MGAUSS(B,A,4,DIM,NS,ZERO,IR)
      IF (.NOT.IR) THEN
        CALL UTMESS('F','MINTER','BASE SINGULIERE')
        GOTO 80
      ENDIF

      CALL MMPROD(PAN1(1,P1+1),DIM+2,DIM+1,1,0,DIM,A,4,0,0,NS,C)

      DO 70 J = 1, NS
        S2(J) = S2(J) + C(J)
 70   CONTINUE

      CALL SMPLX2(A,S2,4,DIM,NS,IR)

 80   CONTINUE

      END
