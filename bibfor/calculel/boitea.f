      SUBROUTINE BOITEA(CNOEUD,NOAR,PAAR,NARE,NPAN,DIME,MINMAX,PAN)

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
C     CORRECTION DE BOITEL PRENANT EN COMPTE LES ARETES QUADRATIQUES
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C REAL*8   CNOEUD(DIME,*) : COORD. NOEUDS DE LA MAILLE (CF CONOEU)
C INTEGER  NOAR(3,*)      : NOEUDS DES ARETES QUADRATIQUES (CF NOAREQ)
C INTEGER  PAAR(DIME-1,*) : PANS TOUCHANT LES ARETES QUADR. (CF PANARQ)
C INTEGER  NARE           : NOMBRE D'ARETES QUADRATIQUES DE LA MAILLE
C INTEGER  NPAN           : NOMBRE DE PANS DE LA MAILLE
C INTEGER  DIME           : DIMENSION DE L'ESPACE
C
C VARIABLES D'ENTREE/SORTIE
C REAL*8   MINMAX(2,DIME) : BOITE ENGLOBANT LA MAILLE SUIVANT X,Y,[Z]
C REAL*8   PAN(DIME+2,*)  : EQUATION DES PANS DU CONVEXE ENGLOBANT
C                           ET INSCRIT DE LA MAILLE (CF BOITE)
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- FONCTIONS
      REAL*8  R8DOT

C --- VARIABLES
      INTEGER DIME,NOAR(3,*),PAAR(DIME-1,*),NARE,NPAN,I,J,K1,K2,K3
      REAL*8  CNOEUD(DIME,*),MINMAX(2,*),PAN(DIME+2,*)
      REAL*8  R,R1,R2,R3,X1(3),X2(3),X3(3)

      DO 10 I = 1, NARE

        K1 = NOAR(1,I)
        K2 = NOAR(2,I)
        K3 = NOAR(3,I)
        
        DO 20 J = 1, DIME

          R1 = CNOEUD(J,K1)
          R2 = CNOEUD(J,K2)
          R3 = CNOEUD(J,K3)

          X1(J) = R3
          X2(J) = R2 - R1
          X3(J) = R1 + R2 - 2*R3

 20     CONTINUE

C ----- MINMAX

        DO 30 J = 1, DIME

          IF (ABS(X2(J)).LT.(2.D0*ABS(X3(J)))) THEN
       
            R = X1(J) - 0.125D0*X2(J)*X2(J)/X3(J)
            IF (R.LT.MINMAX(1,J)) THEN
              MINMAX(1,J) = R
            ELSEIF (R.GT.MINMAX(2,J)) THEN
              MINMAX(2,J) = R
            ENDIF

          ENDIF

 30     CONTINUE

C ----- PANS CONVEXE ENGLOBANT

        K2 = 1 + DIME

        DO 40 J = 1, NPAN

          R2 = R8DOT(DIME,PAN(1,J),1,X2,1)
          R3 = R8DOT(DIME,PAN(1,J),1,X3,1)

          IF (ABS(R2).LT.(2.D0*ABS(R3))) THEN
 
            R = 0.125D0*R2*R2/R3 - R8DOT(DIME,PAN(1,J),1,X1,1)
            IF (R.LT.PAN(K2,J)) PAN(K2,J) = R

          ENDIF

 40     CONTINUE

C ----- PANS CONVEXE INSCRIT

        K2 = 2 + DIME

        DO 10 J = 1, DIME-1

          K3 = PAAR(J,I)
          R2 = R8DOT(DIME,PAN(1,K3),1,X2,1)
          R3 = R8DOT(DIME,PAN(1,K3),1,X3,1)

          IF (ABS(R2).LT.(2.D0*ABS(R3))) THEN
 
            R = 0.125D0*R2*R2/R3 - R8DOT(DIME,PAN(1,K3),1,X1,1)
            IF (R.GT.PAN(K2,K3)) PAN(K2,K3) = R

          ENDIF

 10   CONTINUE
        
      END
