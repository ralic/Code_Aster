      SUBROUTINE BOITEL(CNO,TYPEMA,NOEPAN,NPAN,DIME,MINMAX,PAN)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C ----------------------------------------------------------------------
C                 BOITES ENGLOBANTES POUR UNE MAILLE LINEAIRE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C REAL*8      CNO(DIME,*)    : COORD. NOEUDS DE LA MAILLE (CF CONOEU)
C CHARACTER*8 TYPEMA         : TYPE DE LA MAILLE
C INTEGER     NOEPAN(*)      : NOEUDS DEFINISSANT LES PANS (CF NOPAN)
C INTEGER     NPAN           : NOMBRE DE PANS DE LA MAILLE
C INTEGER     DIME           : DIMENSION DE L'ESPACE
C
C VARIABLES DE SORTIE
C REAL*8      MINMAX(2,DIME) : BOITE ENGLOBANT LA MAILLE SUIVANT X,Y,[Z]
C REAL*8      PAN(DIME+2,*)  : EQUATION DES PANS DU CONVEXE ENGLOBANT
C                              ET INSCRIT DE LA MAILLE (CF BOITE)
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8  TYPEMA
      INTEGER      NOEPAN(*),NPAN,DIME,I,J,K1,K2,N,NNR,P0
      REAL*8       CNO(DIME,*),MINMAX(2,*),PAN(DIME+2,*)
      REAL*8       X1(3),X2(3),X3(3),R1,R2,R3,R,S1,S2
      LOGICAL      SENS

C --- NNR

      IF (DIME.EQ.2) THEN
        IF (TYPEMA(1:4).EQ.'TRIA') THEN
          NNR = 3
        ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
          NNR = 4
        ENDIF
      ELSE
        IF (TYPEMA(1:5).EQ.'TETRA') THEN
          NNR = 4
        ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN
          NNR = 6
        ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN
          NNR = 8
        ENDIF
      ENDIF

C --- MINMAX
      
      DO 10 J = 1, DIME
        R = CNO(J,1)
        MINMAX(1,J) = R
        MINMAX(2,J) = R
10    CONTINUE

      DO 20 I = 2, NNR
        DO 20 J = 1, DIME
          R = CNO(J,I)
          IF (R.LT.MINMAX(1,J)) THEN
            MINMAX(1,J) = R
          ELSEIF (R.GT.MINMAX(2,J)) THEN 
            MINMAX(2,J) = R
          ENDIF
 20   CONTINUE

C --- PAN

      P0 = 0

      IF (DIME.EQ.2) THEN

C ----- PAN 2D

C ----- ORIENTATION DE LA MAILLE

        R1 = CNO(1,1)
        R2 = CNO(2,1)
        S1 = (CNO(1,2)-R1)*(CNO(2,3)-R2)
        S2 = (CNO(2,2)-R2)*(CNO(1,3)-R1)
        SENS = (S1.GT.S2)

C ----- CALCUL DES PANS

        DO 30 I = 1, NPAN

          P0 = P0 + 1
          N = NOEPAN(P0)
          K1 = NOEPAN(P0+1)
          K2 = NOEPAN(P0+2)
          DO 40 J = 1, 2
            X1(J) = CNO(J,K1)
            X2(J) = CNO(J,K2)
 40       CONTINUE
          P0 = P0 + N

          IF (SENS) THEN

            R1 = X2(2) - X1(2)
            R2 = X1(1) - X2(1)
            S1 = X1(1)*X2(2) - X1(2)*X2(1)

          ELSE

            R1 = X1(2) - X2(2)
            R2 = X2(1) - X1(1)
            S1 = X2(1)*X1(2) - X2(2)*X1(1)

          ENDIF

          S2 = S1

          IF ((NNR.EQ.4).AND.(TYPEMA(5:5).NE.'4')) THEN
            
            DO 50 J = 1, NNR
              R = R1*CNO(1,J) + R2*CNO(2,J)
              IF (R.GT.S1) S1 = R
 50         CONTINUE

          ENDIF 

          PAN(1,I) = R1
          PAN(2,I) = R2
          PAN(3,I) = -S1
          PAN(4,I) = -S2

 30     CONTINUE

      ELSE

C ----- PAN 3D

        DO 60 I = 1, NPAN

          P0 = P0 + 1
          N = NOEPAN(P0)
          
          IF (N.LT.0) THEN

C --------- FACE TRIANGLE

            K1 = NOEPAN(P0+1) 
            DO 70 J = 1, 3
              X1(J) = CNO(J,K1)
 70         CONTINUE
            K1 = NOEPAN(P0+2)
            K2 = NOEPAN(P0+3)
            DO 80 J = 1, 3
              X2(J) = CNO(J,K1) - X1(J)
              X3(J) = CNO(J,K2) - X1(J)
 80         CONTINUE
         
          ELSE

C --------- FACE QUADRANGLE

            K1 = NOEPAN(P0+1) 
            K2 = NOEPAN(P0+3)
            DO 90 J = 1, 3
              X2(J) = CNO(J,K2) - CNO(J,K1)
 90         CONTINUE
            K1 = NOEPAN(P0+2)
            K2 = NOEPAN(P0+4)
            DO 100 J = 1, 3
              X3(J) = CNO(J,K2) - CNO(J,K1)
 100        CONTINUE

          ENDIF             

          R1 = X2(2)*X3(3)-X2(3)*X3(2)
          R2 = X2(3)*X3(1)-X2(1)*X3(3)
          R3 = X2(1)*X3(2)-X2(2)*X3(1)

          IF ((N.EQ.-3).OR.(NNR.EQ.4)) THEN
            S1 = R1*X1(1) + R2*X1(2) + R3*X1(3)
            S2 = S1
          ELSE
            S1 = R1*CNO(1,1) + R2*CNO(2,1) + R3*CNO(3,1)     
            DO 110 J = 2, NNR
              R = R1*CNO(1,J) + R2*CNO(2,J) + R3*CNO(3,J)
              IF (R.GT.S1) S1 = R
 110        CONTINUE
            K1 = NOEPAN(P0+1)
            K2 = NOEPAN(P0+2)
            S2 = MIN(R1*CNO(1,K1)+R2*CNO(2,K1)+R3*CNO(3,K1),
     &               R1*CNO(1,K2)+R2*CNO(2,K2)+R3*CNO(3,K2) ) 
          ENDIF

          PAN(1,I) = R1
          PAN(2,I) = R2
          PAN(3,I) = R3
          PAN(4,I) = -S1
          PAN(5,I) = -S2
          P0 = P0 + ABS(N)
      
60      CONTINUE

      ENDIF

      END
