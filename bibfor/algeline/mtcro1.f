      SUBROUTINE MTCRO1(N,A,NMAX,X)
      IMPLICIT NONE
C
      INTEGER       NMAX, N
      REAL*8        A(NMAX,*), X(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     ROUTINE UTILITAIRE POUR RESOUDRE UNE DES EQUATIONS DU SYSTEME
C     A*X = B
C     OPERATEUR APPELANT : MTCROG
C ----------------------------------------------------------------------
C     RESOLUTION D UN SYSTEME LINEAIRE AX = B PAR LA METHODE DE CROUT
C     POUR UNE MATRICE A QUELCONQUE DE DIMENSION N*N
C     SI B EST DE DIMENSION N*1, IL S AGIT D UN SIMPLE SYSTEME
C     LINEAIRE. SI B EST DE DIMENSION N*N, IL S AGIT DE L INVERSION
C     D UNE MATRICE
C ----------------------------------------------------------------------
C IN  : M      : NOMBRE DE LIGNES EFFECTIVES DE A
C IN  : A      : MATRICE A DE DIMESION NMAX*N. A EST UNE MATRICE
C                TRIANGULAIRE INFERIEURE, NON UNITAIRE.
C IN  : NMAX   : PREMIERE DIMENSION DU TABLEAU A
C IN/OUT: X    : VECTEUR DE DIMESION SUPERIEURE OU EGALE A N
C                X CONTIENT EN ENTREE LES N ELEMENTS DU VECTEUR B, ET
C                EN SORTIE, LA SOLUTION X
C ----------------------------------------------------------------------
      REAL*8       ZERO
C-----------------------------------------------------------------------
      INTEGER I ,J 
C-----------------------------------------------------------------------
      DATA ZERO    /0.D0/
C ----------------------------------------------------------------------
C
C
      IF(N.LT.0.OR.NMAX.LT.1.OR.NMAX.LT.N) THEN
         CALL U2MESS('A','ALGELINE2_12')
C
      ELSE
         DO 20, J = 1, N
            IF( X( J ).NE.ZERO )THEN
                X( J ) = X( J )/A( J, J )
                DO 10, I = J + 1, N
                    X( I ) = X( I ) - X( J )*A( I, J )
  10            CONTINUE
            ENDIF
  20     CONTINUE
      ENDIF
C
      END
