      SUBROUTINE ARLAS3(DIME  ,LCARA ,NN1   ,NN2   ,IJ     ,
     &                  MA2   ,NORM  ,CK    ,C)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      INTEGER  DIME,NN1,NN2
      INTEGER  IJ(NN2/2,*)
      INTEGER  MA2(*)
      REAL*8   LCARA
      REAL*8   NORM(DIME,*)
      REAL*8   CK(*),C(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C GESTION DES RELATIONS LINEAIRES
C ASSEMBLAGE MATRICE ELEMENTAIRE ARLEQUIN
C   MAILLE SOLIDE / MAILLE COQUE
C
C ----------------------------------------------------------------------
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  LCARA  : LONGUEUR CARACTERISTIQUE POUR TERME DE COUPLAGE
C IN  NN1    : NOMBRE DE NOEUDS MAILLE 1
C IN  NN2    : NOMBRE DE NOEUDS MAILLE 2
C IN  IJ     : POINTEURS DANS C (CF ARLAS0)
C IN  MA2    : CONNECTIVITE MAILLE 2
C IN  NORM   : NORMALES LISSEES POUR COQUE (CF LISNOR)
C IN  CK     : MATRICES ELEMENTAIRES (CF ARLTE)
C I/O C      : MATRICE MORSE (CF ARLFAC)
C
C MATRICE PONCTUELLE DANS C : (X1.X2, X1.Y2, [X1.Z2], Y1.X2, ...,
C                 (N X X1.*2).1, (N X X1.*2).2, ..., (N X Y1.*2).1, ...)
C
C ----------------------------------------------------------------------
C
      INTEGER NOECOQ(2,9),NN12,D,D2,DR
      INTEGER NBNO,IN2,I,J,K,J1,J2,P,P0,P1,P2,Q,Q1,Q2
      REAL*8  TR1,TR2,R1,R2,C1(9),B2(9),C2(9)
C
C ----------------------------------------------------------------------
C
      IF (DIME.EQ.2) THEN
        D2 = 4
        DR = 2
        D = 6
      ELSE IF (DIME.EQ.3) THEN
        D2 = 9
        DR = 9
        D = 18
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      NBNO = NN2/2
      NN12 = NN1*NN2

C --- ASSEMBLAGE DE LA MATRICE ELEMENTAIRE CK

      P0 = -NN2

      CALL NOCOQU(DIME,NBNO,NOECOQ)

      DO 10 I = 1, NN1

        P0 = P0 + NN2

        DO 10 J = 1, NBNO

          Q = 1 + D*(IJ(J,I)-1)

          IN2 = MA2(J)

          J1 = NOECOQ(2,J)
          J2 = NOECOQ(1,J)

          P1 = P0 + J1
          P2 = P0 + J2

          Q1 = NN12 + D2*(P1-1)
          Q2 = NN12 + D2*(P2-1)

C ------- CALCUL DE LA TRACE DE CK+ ET CK-

          TR1 = 0.D0
          TR2 = 0.D0

          P = 1
          DO 20 K = 1, DIME
            TR1 = TR1 + CK(Q1+P)
            TR2 = TR2 + CK(Q2+P)
            P = P + DIME + 1
 20       CONTINUE

C ------- CALCUL DES MATRICES PONCTUELLES C1 ET C2

          DO 30 K = 1, D2
            Q1 = Q1 + 1
            Q2 = Q2 + 1
            C1(K) = LCARA*(CK(Q1)+CK(Q2))
            B2(K) = LCARA*(CK(Q1)-CK(Q2))
 30       CONTINUE

          R1 = CK(P1) + CK(P2) + LCARA*(TR1+TR2)
          R2 = CK(P1) - CK(P2) + LCARA*(TR1-TR2)

          P = 1
          DO 40 K = 1, DIME
            C1(P) = C1(P) + R1
            B2(P) = B2(P) + R2
            P = P + DIME + 1
 40       CONTINUE

          IF (DIME.EQ.2) THEN

            R1 = NORM(1,IN2)
            R2 = NORM(2,IN2)

            C2(1) = R1*B2(2) - R2*B2(1)
            C2(2) = R1*B2(4) - R2*B2(3)

          ELSE

            P = 1
            DO 50 K = 1, DIME
              CALL PROVEC(NORM(1,IN2),B2(P),C2(P))
              P = P + DIME
 50         CONTINUE

          ENDIF

C ------- ASSEMBLAGE DES MATRICES PONCTUELLES C1 ET C2

          DO 60 K = 1, D2
            C(Q) = C(Q) + C1(K)
            Q = Q + 1
 60       CONTINUE

          DO 70 K = 1, DR
            C(Q) = C(Q) + C2(K)
            Q = Q + 1
 70       CONTINUE

 10   CONTINUE

      END
