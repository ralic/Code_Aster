      SUBROUTINE I3SL32(EPSI,A,B,X,TYP)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*4 TYP
      REAL*8      EPSI,A(3,*),B(*),X(*)
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
C     RESOLUTION D' UN SL 3X2 POUR
C                  INTERSECTION DE 2 DROITES 3D
C                  CALCUL DES COORDONEES DE REF DANS UN TRIANGLE
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C VAR A      : R : IN : MATRICE ; OUT : SANS OBJET
C VAR B      : R : IN : SECOND MEMBRE ; OUT : SANS OBJET
C OUT X      : R : SOLUTION DANS LE CAS 'DETE' OU VIDE SINON
C OUT TYP    : K : 'DETE'/'INCO'/'INDE'
C     ------------------------------------------------------------------
C     RESTRICTION : SL DE RANG MINIMAL 1
C     ------------------------------------------------------------------
C
      REAL*8 A11,A12,A21,A22,A31,A32,B1,B2,B3
      REAL*8 DA12,DA13,DA23,DB12,DB13,DB23,DB,UN
C
C=======================================================================
C
      UN   = 1.0D0
      A11  = A(1,1)
      A12  = A(1,2)
      A21  = A(2,1)
      A22  = A(2,2)
      A31  = A(3,1)
      A32  = A(3,2)
      B1   =   B(1)
      B2   =   B(2)
      B3   =   B(3)
      DA12 = A11*A22 - A12*A21
      DA13 = A11*A32 - A12*A31
      DA23 = A21*A32 - A22*A31
      DB12 = A12*B2  - A22*B1
      DB13 = A12*B3  - A32*B1
      DB23 = A22*B3  - A32*B2
      IF ( ABS(DA12*DA12+DA13*DA13+DA23*DA23) .LE. EPSI ) THEN
         IF ( ABS(DB12*DB12+DB13*DB13+DB23*DB23) .LE. EPSI ) THEN
            TYP = 'INDE'
         ELSE
            TYP = 'INCO'
         ENDIF
      ELSE
         DB  = B1*DA23 - B2*DA13 + B3*DA12
         IF ( ABS(DB) .GT. EPSI ) THEN
            TYP = 'INCO'
         ELSE
            TYP = 'DETE'
         ENDIF
      ENDIF
      IF ( TYP . EQ. 'DETE' ) THEN
         IF ( ABS(DA12) .GE. MAX(ABS(DA13),ABS(DA23)) ) THEN
            DA12 = UN/DA12
            X(1) = (B1*A22 - B2*A12)*DA12
            X(2) = (B2*A11 - B1*A21)*DA12
         ELSE IF ( ABS(DA13) .GE. MAX(ABS(DA12),ABS(DA23)) ) THEN
            DA13 = UN/DA13
            X(1) = (B1*A32 - B3*A12)*DA13
            X(2) = (B3*A11 - B1*A31)*DA13
         ELSE
            DA23 = UN/DA23
            X(1) = (B2*A32 - B3*A22)*DA23
            X(2) = (B3*A21 - B2*A31)*DA23
         ENDIF
      ENDIF
      END
