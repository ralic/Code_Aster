      SUBROUTINE ZEROF3 (FONCT,X1,X2,TOL,NITMAX,SOLU)
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER  NITMAX
      REAL*8   FONCT,X1,X2,TOL
      EXTERNAL  FONCT
      REAL*8   SOLU
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_7
C
C     RECHERCHE DU ZERO D'UNE FONCTION PAR LA METHODE DE BRENT
C     (CF. NUMERICAL RECIPES IN FORTRAN P.354) : FONCT(X) = 0.
C
C     ON DONNE UN INTERVALLE (X1,X2). S'IL N'ENCADRE PAS DE MANIERE
C     NON AMBIGUE LA SOLUTION (VALEURS DE SIGNE OPPOSE PRISES PAR LA
C     FONCTION), ON APPELLE UNE ROUTINE QUI VA CHERCHER UN INTERVALLE
C     ADMISSIBLE EN ELARGISSANT CELUI-LA.
C
C     ENSUITE, LA METHODE UTILISEE COMBINE INTERPOLATION QUADRATIQUE
C     INVERSE ET BISSECTION.
C ----------------------------------------------------------------------
C
C IN  : FONCT  : NOM DE LA FONCTION DONT ON CHERCHE LE ZERO.
C       X1     : BORNE INFERIEURE PROPOSEE.
C       X2     : BORNE SUPERIEURE PROPOSEE.
C       TOL    : TOLERANCE ABSOLUE SUR LE ZERO CHERCHE.
C       NITMAX : NOMBRE MAXIMUM D'ITERATIONS AUTORISEES.
C
C OUT : SOLU   : VALEUR DE LA RACINE CHERCHEE
C
C ----------------- DECLARATION DES VARIABLES LOCALES ------------------
C
      INTEGER SUCCES
      REAL*8  EPS,A,B,C,D,E,FA,FB,FC,P,Q,R,S,XM,TOL1
      PARAMETER (EPS = 1.D-20)
C
C ----------------------------------------------------------------------
C
      A = X1
      B = X2
      FA = FONCT(A)
      FB = FONCT(B)
      IF (FA*FB.GT.0.D0) THEN
         CALL ENCADR (FONCT,X1,X2,F1,F2,NITMAX,SUCCES)
         IF (SUCCES.EQ.0) GO TO 9995
         A = X1
         B = X2
         FA = F1
         FB = F2
      END IF
      C = B
      FC = FB
C
      DO 10 ITER = 1,NITMAX
C
C  -1- MODIFICATION EVENTUELLE DE L'INTERVALLE
C
         IF (FB*FC.GT.0.D0) THEN
            C = A
            FC = FA
            D = B - A
            E = D
         END IF
         IF (ABS(FC).LT.ABS(FB)) THEN
            A = B
            B = C
            C = A
            FA = FB
            FB = FC
            FC = FA
         END IF
C
C -2- TEST DE CONVERGENCE
C
         TOL1 = 2.D0*EPS*ABS(B) + 0.5D0*TOL
         XM = 0.5D0 * (C-B)
         IF ((ABS(XM).LE.TOL1).OR.(FB.EQ.0.D0)) THEN
             SOLU = B
             GO TO 9999
         END IF
C
C -3- INTERPOLATION QUADRATIQUE INVERSE ???
C
         IF ((ABS(E).GE.TOL1).AND.(ABS(FA).GT.ABS(FB))) THEN
C
C -3.1- INTERPOLATION  QUADRATIQUE INVERSE
C
            S = FB/FA
            IF (A.EQ.C) THEN
               P = 2.D0*XM*S
               Q = 1.D0 - S
            ELSE
               Q = FA/FC
               R = FB/FC
               P = S * ( 2.D0*XM*Q*(Q-R) - (B-A)*(R-1.D0) )
               Q = (Q-1.D0) * (R-1.D0) * (S-1.D0)
            END IF
C
            IF (P.GT.0.D0) Q = -Q
            P = ABS(P)
            IF (2.D0*P.LT.MIN(3.D0*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
C -3.1.1- INTERPOLATION ACCEPTEE
               E = D
               D = P/Q
            ELSE
C -3.1.2- INTERPOLATION REFUSEE, ON FAIT BISSECTION
               D = XM
               E = D
            END IF
C
         ELSE
C
C -3.2- BORNES DECROISSENT TROP LENTEMENT => BISSECTION
C
            D = XM
            E = D
C
         END IF
C
C -3.3- LE DERNIER MEILLEUR ESSAI REMPLACE A
C
         A = B
         FA = FB
C
C -3.4-  ACTUALISATION DE B (MEILLEURE VALEUR A CE JOUR)
C
         IF (ABS(D).GT.TOL1) THEN
            B = B+D
         ELSE
            B = B+SIGN(TOL1,XM)
         END IF
         FB = FONCT(B)
C
   10 CONTINUE
C
      CALL U2MESS('F','ALGORITH11_74')
      GO TO 9999
C
 9995 CONTINUE
      CALL U2MESS('F','ALGORITH11_75')
C
C ----------------------------------------------------------------------
C
 9999 CONTINUE
      END
