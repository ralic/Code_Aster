      SUBROUTINE VPQLTS (DIAG,SURDIA,NEQ,VECPRO,MXCMP,MXITER,IER,NITQR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER  NEQ,MXCMP,MXITER,IER,NITQR
      REAL*8   DIAG(1),SURDIA(1),VECPRO(MXCMP,1)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/09/96   AUTEUR D6BHHBQ B.QUINNEZ 
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
C     CALCUL DE TOUTES LES VALEURS PROPRES ET DES VECTEURS PROPRES
C     ASSOCIES PAR LA METHODE QL-IMPLICITE POUR UNE MATRICE TRIDIAGONALE
C     SYMETRIQUE
C     ------------------------------------------------------------------
C VAR  DIAG  :    : TABLEAU DE R8 (1 .. NEQ)
C            EN ENTREE: VECTEUR CONTENANT LA DIAGONALE DE LA MATRICE
C            EN SORTIE: CONTIENT LES VALEURS PROPRES EN ORDRE QUELCONQUE
C VAR  SURDIA:    : TABLEAU DE R8 (1 .. NEQ)
C            EN ENTREE: CONTIENT LA  SUR-DIAGONALE     2,...,NEQ.
C            EN SORTIE: LA SUR-DIAGONALE EST PERDUE (ZONE DE TRAVAIL)
C IN   NEQ   : IS : ORDRE DE LA MATRICE
C OUT  VECPRO :   : TABLEAU DE R8 (1 .. NEQ)X(1 .. NEQ)
C            EN SORTIE:  CONTIENT LES VECTEURS PROPRES
C                        LA COLONNE J CONTIENT LE J-IEME VECTEUR PROPRE
C                        QUI CORRESPOND A LA VALEUR PROPRE DIAG(J).
C IN   MXCMP :  1-ERE DIMENSION (EXACTE) DE VECPRO (POUR LE FORTRAN)
C                   SI MXCMP < NEQ ALORS ON NE CALCULE PAS LES VECTEURS
C                                     ET VECPRO N'EST ALORS PAS UTILISE
C IN   MXITER:  NOMBRE MAXIMUM D'ITERATION (MXITER=30 EST UN BON CHOIX)
C OUT  IER   : IS : 0  PAS DE PROBLEME
C                   J  CONVERGENCE NON ATTEINTE A LA J VALEUR PROPRE
C OUT  NITQR : NOMBRE MAXIMAL ATTEINT D'ITERATIONS POUR AVOIR CONVERGE
C     ------------------------------------------------------------------
C     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
C        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
C        PAGE ????
C     ------------------------------------------------------------------
      REAL*8   B,C,F,G,H,P,R,S
      REAL*8   ZERO, UN, DEUX , EPSMAC, R8PREM
C     ------------------------------------------------------------------
      IER    = 0
      ZERO   = 0.0D0
      EPSMAC = R8PREM()
      UN     = 1.0D0
      DEUX   = 2.0D0
      NITQR = 0
       
      IF (NEQ .EQ. 1) GOTO 99999
C
C     --- TRANSFERT DES ELEMENTS SUR-DIAGONAUX ---
      DO 5  I=2,NEQ
         SURDIA(I-1) = SURDIA(I)
    5 CONTINUE
C
C     --- SI NECESSAIRE CREATION DE LA MATRICE UNITE ---
      IF (MXCMP .GE. NEQ )  THEN
         DO 6 IEQ = 1, NEQ
            DO 7 JEQ = 1, NEQ
               VECPRO(IEQ,JEQ) = ZERO
    7       CONTINUE
            VECPRO(IEQ,IEQ) = UN
    6    CONTINUE
      ENDIF
C
      SURDIA(NEQ) = ZERO
      B = ZERO
      F = ZERO
      DO  60  J=1,  NEQ
         JTER = 0
         H = EPSMAC*(ABS(DIAG(J))+ABS(SURDIA(J)))
         IF (B.LT.H) B = H
C
C        --- RECHERCHE DU PLUS PETIT ELEMENT SUR-DIAGONAL ---
         DO 10  M=J,NEQ
            K=M
            IF (ABS(SURDIA(K)) .LE. B) GO TO 15
   10    CONTINUE
   15    CONTINUE
         M = K
         IF (M.EQ.J) GO TO 55
   20    CONTINUE
         IF (JTER .EQ. MXITER) GO TO 9999
         JTER = JTER+1
         IF (JTER .GT. NITQR) THEN
            NITQR = JTER
         ENDIF   
C
C        --- PREPARATION DU DECALAGE ---
         G = DIAG(J)
         P = (DIAG(J+1)-G)/(DEUX*SURDIA(J))
         R = ABS(P)
         IF (EPSMAC*ABS(P) .LT. UN ) R = SQRT(P*P+UN)
         DIAG(J) = SURDIA(J)/(P+SIGN(R,P))
         H = G-DIAG(J)
         DO 25 I = J+1,NEQ
            DIAG(I) = DIAG(I)-H
   25    CONTINUE
         F = F+H
C
C        --- ON APPLIQUE LA TRANSFORMATION QL ---
         P = DIAG(M)
         C = UN
         S = ZERO
         DO 45 I= M-1, J , -1
            G = C*SURDIA(I)
            H = C*P
            IF (ABS(P).GE.ABS(SURDIA(I))) THEN
               C = SURDIA(I)/P
               R = SQRT(C*C+UN)
               SURDIA(I+1) = S*P*R
               S = C/R
               C = UN/R
            ELSE
               C = P/SURDIA(I)
               R = SQRT(C*C+UN)
               SURDIA(I+1) = S*SURDIA(I)*R
               S = UN/R
               C = C*S
            ENDIF
            P = C*DIAG(I)-S*G
            DIAG(I+1) = H+S*(C*G+S*DIAG(I))
            IF (MXCMP .GE. NEQ) THEN
C              --- CALCUL DU VECTEUR PROPRE ---
               DO 40 K=1,NEQ
                  H = VECPRO(K,I+1)
                  VECPRO(K,I+1) = S*VECPRO(K,I)+C*H
                  VECPRO(K,I)   = C*VECPRO(K,I)-S*H
   40          CONTINUE
            ENDIF
   45    CONTINUE
         SURDIA(J) = S*P
         DIAG(J)   = C*P
         IF ( ABS(SURDIA(J)) .GT.B) GO TO 20
   55    CONTINUE
         DIAG(J) = DIAG(J) + F
   60 CONTINUE
      GOTO 99999
C
 9999 CONTINUE
      IER = J
99999 CONTINUE
      END
