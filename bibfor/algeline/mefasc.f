      SUBROUTINE MEFASC(NDIM,NBCYL,NBGRP,NBTRON,NUMGRP,IDIR,IGRP,SOM,
     &                  RINT,DCENT,FICENT,D,FI,A,B)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER       NDIM(14),NBCYL,NBGRP,NBTRON,NUMGRP(*),IDIR,IGRP
      REAL*8        DCENT(NBCYL),FICENT(NBCYL),RINT(*),SOM(9)
      REAL*8        D(NBCYL,NBCYL),FI(NBCYL,NBCYL)
      REAL*8        A(2*NBTRON*(NBCYL+1),*),B(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 05/10/1999   AUTEUR KXBADNG A.ADOBES 
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
C ----------------------------------------------------------------------
C     ASSEMBLAGE POUR L ENCEINTE CIRCULAIRE
C     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST, MEFCIR
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"
C ----------------------------------------------------------------------
C IN  : NDIM   : TABLEAU DES DIMENSIONS
C IN  : NBCYL  : NOMBRE DE CYLINDRES
C IN  : NBGRP  : NOMBRE DE GROUPES D EQUIVALENCE
C IN  : NBTRON : ORDRE DE TRONCATURE DES SERIES DE LAURENT DANS LA BASE
C                MODALE
C IN  : NUMGRP : INDICES DES GROUPES D EQUIVALENCE
C IN  : IDIR   : INDICES DE CYLINDRE
C IN  : IGRP   : INDICES DE GROUPE DE CYLINDRE
C IN  : SOM    : XEXT,YEXT,REXT
C IN  : RINT   : RAYONS DES CYLINDRES
C IN  : DCENT  : DISTANCE DU CENTRE DES CYLINDRES AU CENTRE DE
C                L ENCEINTE
C IN  : FICENT : ANGLE POLAIRE PAR RAPPORT AU CENTRE DE L ENCEINTE
C IN  : D      : DISTANCE RELATIVE ENTRE LES CENTRES DES CYLINDRES
C IN  : FI     : ANGLE POLAIRE RELATIF PAR RAPPORT AU CENTRE DE CHAQUE
C                CYLINDRE
C IN  : A      : TABLEAU DE TRAVAIL: SOUS MATRICE DU SYSTEME A.X = B
C IN  : B      : TABLEAU DE TRAVAIL: SECOND MEMBRE DU SYSTEME A.X = B
C ----------------------------------------------------------------------
      INTEGER      I,J,K,L,NI,NJ,NK,NL
      REAL*8       MEFAC1,MEFAC2,COEF
C ----------------------------------------------------------------------
C
C --- LECTURE DES DIMENSIONS
      NBCYL  = NDIM(3)
      NBGRP  = NDIM(4)
      NBTRON = NDIM(5)
C
C
      REXT = SOM(3)
C
C
      DO 1 J = 1,NBTRON
         NJ = 2*J
         DO 11 K = 1,NBCYL
            NK = 2*NBTRON*K
            DO 111 L = 1,J
               NL = NK+2*L
               IF(DCENT(K).EQ.0.D0.AND.J.EQ.L) THEN
                  COEF = MEFAC1(J,L)*
     &                 (RINT(K)**(L+1))/(REXT**(J+1))
               ELSE IF(DCENT(K).EQ.0.D0.AND.J.NE.L) THEN
                  COEF = 0.D0
               ELSE
                  COEF = MEFAC1(J,L)*(DCENT(K)**(J-L))*
     &                 (RINT(K)**(L+1))/(REXT**(J+1))
               ENDIF
               A(NJ-1,NL-1) = -COEF*COS((J-L)*FICENT(K))
               A(NJ,NL-1) = COEF*SIN((J-L)*FICENT(K))
               A(NJ-1,NL) = COEF*SIN((J-L)*FICENT(K))
               A(NJ,NL) = COEF*COS((J-L)*FICENT(K))
 111        CONTINUE
  11     CONTINUE
         A(NJ-1,NJ-1) = J
         A(NJ,NJ) = -J
C
   1  CONTINUE
C
      DO 2 I = 1,NBCYL
         NI = 2*NBTRON*I
         DO 21 J = 1,NBTRON
            NJ = NI+2*J
            DO 211 K = 1,NBCYL
               NK = 2*NBTRON*K
               IF (K.NE.I) THEN
                  DO 2111 L = 1,NBTRON
                     NL = NK+2*L
                     COEF = MEFAC2(L,J)*(RINT(I)**(J-1))*
     &                    (RINT(K)**(L+1))/(D(I,K)**(L+J))
                     COEF = COEF*((-1)**L)
                     A(NJ-1,NL-1) = COEF*COS((J+L)*FI(I,K))
                     A(NJ,NL-1) = COEF*SIN((J+L)*FI(I,K))
                     A(NJ-1,NL) = COEF*SIN((J+L)*FI(I,K))
                     A(NJ,NL) = -COEF*COS((J+L)*FI(I,K))
 2111             CONTINUE
               ELSE
                  NL = NK+2*J
                  A(NJ-1,NL-1) = -J
                  A(NJ,NL) = -J
               ENDIF
 211        CONTINUE
C
            DO 221 L = J,NBTRON
               NL = 2*L
               IF(DCENT(I).EQ.0.D0.AND.J.EQ.L) THEN
                  COEF = MEFAC1(L,J)*(RINT(I)**(J-1))
     &                 /(REXT**(L-1))
               ELSE IF(DCENT(I).EQ.0.D0.AND.J.NE.L) THEN
                  COEF = 0.D0
               ELSE
                  COEF = MEFAC1(L,J)*(RINT(I)**(J-1))*
     &                 (DCENT(I)**(L-J))/(REXT**(L-1))
               ENDIF
               A(NJ-1,NL-1) = COEF*COS((L-J)*FICENT(I))
               A(NJ,NL-1) = -COEF*SIN((L-J)*FICENT(I))
               A(NJ-1,NL) = COEF*SIN((L-J)*FICENT(I))
               A(NJ,NL) = COEF*COS((L-J)*FICENT(I))
 221        CONTINUE
C
  21     CONTINUE
         IF (NUMGRP(I).EQ.IGRP) THEN
            B(2*NBTRON*I+IDIR) = 1.D0
         ENDIF
   2  CONTINUE
C
      END
