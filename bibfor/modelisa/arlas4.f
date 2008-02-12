      SUBROUTINE ARLAS4(DIME  ,LCARA ,NN1   ,NN2   ,IJ    ,
     &                  MA1   ,MA2   ,NORM  ,TANG  ,CK    ,
     &                  C)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER  DIME,NN1,NN2
      INTEGER  IJ(NN2/2,*)
      INTEGER  MA1(*)
      INTEGER  MA2(*)      
      REAL*8   LCARA
      REAL*8   NORM(DIME,*)
      REAL*8   CK(*),C(*)
      REAL*8   TANG(DIME*(DIME-1),*)
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C GESTION DES RELATIONS LINEAIRES
C ASSEMBLAGE MATRICE ELEMENTAIRE ARLEQUIN 
C   MAILLE COQUE / MAILLE COQUE
C
C ----------------------------------------------------------------------
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  LCARA  : LONGUEUR CARACTERISTIQUE POUR TERME DE COUPLAGE
C IN  NN1    : NOMBRE DE NOEUDS MAILLE 1
C IN  NN2    : NOMBRE DE NOEUDS MAILLE 2
C IN  IJ     : POINTEURS DANS C (CF ARLAS0)
C IN  MA1    : CONNECTIVITE MAILLE 1
C IN  MA2    : CONNECTIVITE MAILLE 2
C IN  TANG   : NORMALES LISSEES POUR COQUE (CF LISNOR)
C IN  NORM   : TANGENTES LISSEES POUR COQUE (CF LISNOR)
C IN  CK     : MATRICES ELEMENTAIRES (CF ARLTE)
C I/O C      : MATRICE MORSE (CF ARLFAC)
C
C MATRICE PONCTUELLE DANS C : (X1.X2, X1.Y2, [X1.Z2], Y1.X2, ...,
C                              T1.X2, T1.Y2, [T1.Z2, T2.X2, ...],
C        (N X X1.*2).1, [(N X X1.*2).2, ...,] (N X Y1.*2).1, ...,
C        (N X T1.*2).1, [(N X T1.*2).2, ..., (N X T2.*2).1, ...] )
C
C ----------------------------------------------------------------------
C
      INTEGER NR1,NR2,NN12,IN1,IN2
      INTEGER D,D2,DT,DR,DTR,D2N2,NOCOQ1(2,9),NOCOQ2(2,9),I1,I2,J1,J2
      INTEGER I,J,K,P,P1,P2,P3,P4,P5,P6,Q,Q1,Q2,Q3,Q4,Q5,Q6
      REAL*8  C1(9),B2(9),C2(6),B3(9),C3(9),B4(9),C4(6)
      REAL*8  TR1,TR2,TR3,TR4,R1,R2,R3,R4,S1,S2,S3,S4
C
C ----------------------------------------------------------------------
C
      IF (DIME.EQ.2) THEN
        D2 = 4
        DT = 2
        DR = 2
        DTR = 1
        D = 9
      ELSE IF (DIME.EQ.3) THEN
        D2 = 9
        DT = 6
        DR = 9
        DTR = 6
        D = 30
      ELSE
        CALL ASSERT(.FALSE.)         
      ENDIF

      NR1 = NN1/2
      NR2 = NN2/2
      NN12 = NN1*NN2
      D2N2 = D2*NN2

C --- ASSEMBLAGE DE LA MATRICE ELEMENTAIRE CK

      CALL NOCOQU(DIME,NR1,NOCOQ1)
      CALL NOCOQU(DIME,NR2,NOCOQ2)

      DO 10 I = 1, NR1

        IN1 = MA1(I)

        I1 = NOCOQ1(2,I)
        I2 = NOCOQ1(1,I)

        P5 = NN2*(I1-1)
        P6 = NN2*(I2-1)
        Q5 = NN12 + D2N2*(I1-1)
        Q6 = NN12 + D2N2*(I2-1)

        DO 10 J = 1, NR2

          Q = 1 + D*(IJ(J,I)-1)

          IN2 = MA2(J)

          J1 = NOCOQ2(2,J)
          J2 = NOCOQ2(1,J)

          P1 = P5 + J1 
          P2 = P6 + J1
          P3 = P5 + J2
          P4 = P6 + J2 

          Q1 = Q5 + D2*(J1-1)
          Q2 = Q6 + D2*(J1-1)
          Q3 = Q5 + D2*(J2-1)
          Q4 = Q6 + D2*(J2-1)

C ------- CALCUL DE LA TRACE DE CK++, CK-+, CK+- ET CK--

          TR1 = 0.D0
          TR2 = 0.D0
          TR3 = 0.D0
          TR4 = 0.D0

          P = 1
          DO 20 K = 1, DIME

            TR1 = TR1 + CK(Q1+P)
            TR2 = TR2 + CK(Q2+P)
            TR3 = TR3 + CK(Q3+P)
            TR4 = TR4 + CK(Q4+P)

            P = P + DIME + 1

 20       CONTINUE
          
C ------- CALCUL DES MATRICES PONCTUELLES C1, C2, C3 ET C4

          DO 30 K = 1, D2

            Q1 = Q1 + 1
            Q2 = Q2 + 1
            Q3 = Q3 + 1
            Q4 = Q4 + 1

            S1 = CK(Q1)
            S2 = CK(Q2)
            S3 = CK(Q3)
            S4 = CK(Q4)

            C1(K) = LCARA*(S1+S2+S3+S4)
            B2(K) = LCARA*(S1-S2+S3-S4)
            B3(K) = LCARA*(S1+S2-S3-S4)
            B4(K) = LCARA*(S1-S2-S3+S4)

 30       CONTINUE

          S1 = CK(P1)
          S2 = CK(P2)
          S3 = CK(P3)
          S4 = CK(P4)

          R1 = S1 + S2 + S3 + S4 + LCARA*(TR1+TR2+TR3+TR4)
          R2 = S1 - S2 + S3 - S4 + LCARA*(TR1-TR2+TR3-TR4)
          R3 = S1 + S2 - S3 - S4 + LCARA*(TR1+TR2-TR3-TR4)
          R4 = S1 - S2 - S3 + S4 + LCARA*(TR1-TR2-TR3+TR4)
 
          P = 1
          DO 40 K = 1, DIME

            C1(P) = C1(P) + R1
            B2(P) = B2(P) + R2
            B3(P) = B3(P) + R3
            B4(P) = B4(P) + R4

            P = P + DIME + 1

 40       CONTINUE  

          CALL MMPROD(B2,DIME,0,DIME,0,DIME,TANG(1,IN1),
     &                DIME,0,0,DIME-1,C2)
          CALL MMPROD(B4,DIME,0,DIME,0,DIME,TANG(1,IN1),
     &                DIME,0,0,DIME-1,B2)

          IF (DIME.EQ.2) THEN

            R1 = NORM(1,IN2)
            R2 = NORM(2,IN2)

            C3(1) = R1*B3(2) - R2*B3(1)
            C3(2) = R1*B3(4) - R2*B3(3)
            C4(1) = R1*B2(2) - R2*B2(1)

          ELSE

            P = 1
            DO 50 K = 1, DIME
              CALL PROVEC(NORM(1,IN2),B3(P),C3(P))
              P = P + DIME
 50         CONTINUE

            P = 1
            DO 60 K = 2, DIME
              CALL PROVEC(NORM(1,IN2),B2(P),C4(P))
              P = P + DIME
 60         CONTINUE

          ENDIF

C ------- ASSEMBLAGE DES MATRICES PONCTUELLES

          DO 70 K = 1, D2
            C(Q) = C(Q) + C1(K)
            Q = Q + 1
 70       CONTINUE

          DO 80 K = 1, DT
            C(Q) = C(Q) + C2(K)
            Q = Q + 1
 80       CONTINUE

          DO 90 K = 1, DR
            C(Q) = C(Q) + C3(K)
            Q = Q + 1
 90       CONTINUE

          DO 100 K = 1, DTR
            C(Q) = C(Q) + C4(K)
            Q = Q + 1
 100      CONTINUE

 10   CONTINUE

      END
