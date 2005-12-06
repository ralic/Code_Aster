      SUBROUTINE I3ICFS(EPSI,FGLO,SGT,FLOC1,FLOC2,NBPT,IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER NBPT
      REAL*8 EPSI,FGLO(3,*),SGT(*),FLOC1(*),FLOC2(*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     ------------------------------------------------------------------
C     INTERSECTION SGT COUPE DE FACE GAUCHE
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  FGLO   : R : TABLE(1..3,1..3) : COORDO DE LA COUPE DANS (S1,EI)
C IN  SGT    : R : TABLE(1..6)      : COORDO DU SEGMENT  DANS (S1,EI)
C VAR FLOC1  : R : TABLE(1..3)
C            :   :   IN  : COORDO DEBUT COUPE DANS REPERE COUPE
C            :   :   OUT : REPERAGE POINT 1 CALCULE (ABSC_CURV)
C VAR FLOC2  : R : TABLE(1..3)
C            :   :   IN  : COORDO FIN COUPE DANS REPERE COUPE
C            :   :   OUT : REPERAGE POINT 2 CALCULE (ABSC_CURV)
C OUT NBPT   : I : NBR DE POINT CALCULES (-2 = INFINI)
C OUT IRET   : I : CODE RETOUR -1 = DEGENERESCENCE
C     ------------------------------------------------------------------
C     PARAMETRAGE COUPE = (-1,1)
C     PARAMETRAGE SGT   = ( 0,1)
C     ------------------------------------------------------------------
C
      REAL*8  ZERO,UNSUR2,UN,DEUX
      REAL*8  X1,X2,Y1,Y2,XA,YA,XB,YB,D,A11,A12,A21,A22,B1,B2
C
C======================================================================
C
      ZERO   = 0.0D0
      UNSUR2 = 0.5D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      IRET   = 0
      NBPT   = 0
      XA     = SGT(1)
      YA     = SGT(2)
      XB     = SGT(4)
      YB     = SGT(5)
      X1     = FGLO(1,1)
      Y1     = FGLO(2,1)
      X2     = FGLO(1,2)
      Y2     = FGLO(2,2)
      D      = UN/(FLOC2(1)-FLOC1(1))
      XA     = ((XA-X1)*(X2-X1)+(YA-Y1)*(Y2-Y1))*D
      XB     = ((XB-X1)*(X2-X1)+(YB-Y1)*(Y2-Y1))*D
      YA     = SGT(3)
      YB     = SGT(6)
      X1     = FLOC1(1)
      Y1     = FLOC1(2)
      X2     = FLOC2(1)
      Y2     = FLOC2(2)
      A11    =  XB-XA
      A21    =  YB-YA
      A12    = (X1-X2)*UNSUR2
      A22    = (Y1-Y2)*UNSUR2
      B1     = (X1+X2)*UNSUR2 - XA
      B2     = (Y1+Y2)*UNSUR2 - YA
      D      = MAX(ABS(A11),ABS(A12))
      IF ( ABS(D) .GT. EPSI ) THEN
         D   = UN/D
         A11 = A11*D
         A12 = A12*D
         B1  = B1 *D
      ENDIF
      D   = MAX(ABS(A21),ABS(A22))
      IF ( ABS(D) .GT. EPSI ) THEN
         D   = UN/D
         A21 = A21*D
         A22 = A22*D
         B2  = B2 *D
      ENDIF
      IF ( IRET .NE. -1 ) THEN
         D = A11*A22 - A12*A21
         IF ( ABS(D) .LE. EPSI ) THEN
            D = A11*B2 - A21*B1
            IF ( ABS(D) .LE. EPSI ) THEN
               D  = UN/(X2-X1)
               B1 = DEUX*(XA - X1)*D - UN
               B2 = DEUX*(XB - X1)*D - UN
               D  = B1
               B1 = MIN(B1,B2)
               B2 = MAX(D ,B2)
               B1 = MAX(-UN,B1)
               B2 = MIN( UN,B2)
               IF ( B1 .GE. B2 ) THEN
                  NBPT = 0
               ELSE
                  D  = UN/(XB-XA)
                  Y1 = (X1-XA)*D
                  Y2 = (X2-XA)*D
                  Y1 = MAX(Y1,ZERO)
                  Y2 = MIN(Y2,UN)
                  IF ( Y1 .GE. Y2 ) THEN
                     NBPT = 0
                  ELSE
                     NBPT     = -2
                     FLOC1(1) =  B1
                     FLOC1(2) =  Y1
                     FLOC2(1) =  B2
                     FLOC2(2) =  Y2
                  ENDIF
               ENDIF
            ELSE
               NBPT = 0
            ENDIF
         ELSE
            D  = UN/D
            X1 = (B1*A22 - B2*A12)*D
            X2 = (B2*A11 - B1*A21)*D
            IF ( (    -EPSI  .LE. X1) .AND. (X1 .LE. (UN+EPSI)) .AND.
     +           (-(UN+EPSI) .LE. X2) .AND. (X2 .LE. (UN+EPSI)) ) THEN
               NBPT     = 1
               FLOC1(1) = X2
               FLOC2(2) = X1
            ELSE
               NBPT = 0
            ENDIF
         ENDIF
      ENDIF
      END
