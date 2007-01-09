      SUBROUTINE SMPLX2(A     ,B     ,DIME  ,NC    ,NL    ,
     &                  IRET)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
      INTEGER  NL,DIME,NC
      REAL*8   A(DIME,NL),B(NL)
      LOGICAL  IRET
C      
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C TEST SI LE DOMAINE DEFINI PAR LES INEQUATIONS
C                     T[A] * X + B <= 0 ET X >= 0        
C EST NON-VIDE VIA LA METHODE DU SIMPLEXE
C
C ----------------------------------------------------------------------
C
C
C
C IN  A      : MATRICE DES INEQUATIONS
C IN  B      : SECOND MEMBRE DES INEQUATIONS
C IN  DIME   : DIMENSION DE A
C IN  NL     : NOMBRE D'INEQUATIONS
C IN  NC     : NOMBRE DE VARIABLES (NC < DIM)
C OUT IRET   : .TRUE. SI LE DOMAINE EST NON-VIDE
C
C ----------------------------------------------------------------------
C
      INTEGER  I,I1,J,J0,J1
      REAL*8   R,R0,R1,R2
C
C ----------------------------------------------------------------------
C

      I1 = NC + 1
      IRET = .TRUE.

C --- 1 VARIABLE ARTIFICIELLE

      J0 = 1
      R0 = B(1)
      DO 10 J = 2, NL
        R = B(J)
        IF (R.GT.R0) THEN
          J0 = J
          R0 = R
        ENDIF
 10   CONTINUE
 
      IF (R0.LE.0.D0) GOTO 110 

      DO 20 I = 1, NC
        A(I,J0) = -A(I,J0)
 20   CONTINUE

      B(J0) = -1.D0
 
      DO 30 J = 1, NL
        IF (B(J).LT.0.D0) THEN
          A(I1,J) = 0.D0
        ELSE
          DO 40 I = 1, NC
            A(I,J) = A(I,J) + A(I,J0)
 40       CONTINUE
          A(I,J) = -1.D0
          B(J) = B(J) - R0
        ENDIF
 30   CONTINUE

      B(J0) = -R0
      A(I1,J0) = -1.D0

 50   CONTINUE

C --- 2 TEST D'OPTIMALITE

      I1 = NC + 1
      R0 = A(I1,J0)
      DO 60 I = 1, NC
        R = A(I,J0)
        IF (R.LE.R0) GOTO 60
        R0 = R
        I1 = I
 60   CONTINUE

      IF (R0.LE.0.D0) THEN 
        IRET = .FALSE.
        GOTO 110
      ENDIF

C --- 3 DIRECTION DE DESCENTE
 
      R1 = B(J0)/R0
      J1 = J0
      DO 70 J = 1, NL
        IF (J.EQ.J0) GOTO 70
        R = A(I1,J)
        IF (R.LE.0.D0) GOTO 70 
        R2 = B(J)/R
        IF (R2.LE.R1) GOTO 70
        R0 = R
        R1 = R2
        J1 = J
 70   CONTINUE
     
      IF (J1.EQ.J0) GOTO 110

C --- 4 CHANGEMENT DE SOMMET

      A(I1,J1) = 1.D0/R0
      B(J1) = R1

      DO 80 I = 1, NC+1
        IF (I.EQ.I1) GOTO 80
        A(I,J1) = A(I,J1)/R0
 80   CONTINUE

      DO 90 J = 1, NL
        IF (J.EQ.J1) GOTO 90
        R = A(I1,J)
        DO 100 I = 1, NC+1
          IF (I.EQ.I1) GOTO 100
          A(I,J) = A(I,J) - A(I,J1)*R
 100    CONTINUE
        B(J) = B(J) - R*R1
        A(I1,J) = -R/R0
 90   CONTINUE

      GOTO 50

 110  CONTINUE      

      END
