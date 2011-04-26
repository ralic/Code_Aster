      SUBROUTINE GDMMAS (KP,NNO,PJACOB,EN,GRANI,ROT0,   MASS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, CALCULE LA
C           MATRICE DE MASSE EN POSITION DE REFERENCE.
C
C     IN  : KP        : NUMERO DU POINT DE GAUSS
C           NNO       : NOMBRE DE NOEUDS
C           PJACOB    : POIDS * JACOBIEN
C           EN        : FONCTIONS DE FORME
C           GRANI     : DIAGONALE DU TENSEUR D'INERTIE EN AXES LOCAUX
C                       POUR LES 3 1ERES COMPOSANTES, RHO*A POUR LA 4EME
C           ROT0      : MATRICE DE ROTATION DES AXES PRINCIPAUX D'INERT.
C                       AU POINT DE GAUSS DANS LA POSITION DE REFERENCE,
C                       PAR RAPPORT AUX AXES GENERAUX
C
C     OUT : MASS      : MATRICE DE MASSE (CUMUL DES CONTRIBUTIONS DES
C                       POINTS DE GAUSS)
C ------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 EN(3,2),GRANI(4),ROT0(3,3),MASS(18,18),IMAS(6,6),
     &IRO(3,3),AMAT1(3,3)
C
      ZERO = 0.D0
      DO 2 J=1,6
      DO 1 I=1,6
      IMAS(I,J) = ZERO
    1 CONTINUE
    2 CONTINUE
C
      DO 3 I=1,3
      IMAS(I,I) = GRANI(4)
    3 CONTINUE
C
      DO 5 J=1,3
      DO 4 I=1,3
      AMAT1(I,J) = GRANI(I) * ROT0(J,I)
    4 CONTINUE
    5 CONTINUE
      CALL PROMAT (ROT0 ,3,3,3,AMAT1,3,3,3,   IRO  )
C
      DO 7 J=1,3
      DO 6 I=1,3
      IMAS(3+I,3+J) = IRO(I,J)
    6 CONTINUE
    7 CONTINUE
C
      DO 52 J=1,NNO
      DO 51 I=1,NNO
      COEF = PJACOB * EN(I,KP) * EN(J,KP)
      CALL CUMUMA (I,J,IMAS,COEF,     MASS)
   51 CONTINUE
   52 CONTINUE
      END
