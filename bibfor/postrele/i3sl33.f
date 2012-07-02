      SUBROUTINE I3SL33(EPSI,A,B,X,TYP)
      IMPLICIT NONE
C
      CHARACTER*4 TYP
      REAL*8      EPSI,A(3,*),B(*),X(*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     ------------------------------------------------------------------
C     RESOLUTION D' UN SL 3X3 POUR
C                  INTERSECTION SEGMENT-TRIANGLE
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C VAR A      : R : IN : MATRICE ; OUT : SANS OBJET
C VAR B      : R : IN : SECOND MEMBRE ; OUT : SANS OBJET
C OUT X      : R : INCONNUES
C OUT TYP    : K : 'DETE'/'INCO'/'INDE'
C     ------------------------------------------------------------------
C     RESTRICTION : SL DE RANG MINIMAL 1
C     ------------------------------------------------------------------
C
      REAL*8 A11,A12,A21,A22,A31,A32,B1,B2,B3,A13,A23,A33
      REAL*8 DA12,DA13,DA23,DB1,DB3,DB2,DA,DB,UN
C
C=======================================================================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      UN   = 1.0D0
      A11  = A(1,1)
      A12  = A(1,2)
      A13  = A(1,3)
      B1   = B(1)
      A21  = A(2,1)
      A22  = A(2,2)
      A23  = A(2,3)
      B2   = B(2)
      A31  = A(3,1)
      A32  = A(3,2)
      A33  = A(3,3)
      B3   = B(3)
      DA12 = A11*A22  - A12*A21
      DA13 = A11*A32  - A12*A31
      DA23 = A21*A32  - A22*A31
      DA   = A33*DA12 - A23*DA13 + A13*DA23
      DB3  =  B3*DA12 -  B2*DA13 +  B1*DA23
      DA12 = A21*A13  - A11*A23
      DA13 = A13*A31  - A11*A33
      DA23 = A23*A31  - A21*A33
      DB2  =  B3*DA12 -  B2*DA13 +  B1*DA23
      DA12 = A12*A23  - A22*A13
      DA13 = A12*A33  - A13*A32
      DA23 = A22*A33  - A32*A23
      DB1  =  B3*DA12 -  B2*DA13 +  B1*DA23
      IF ( ABS(DA) .GT. EPSI ) THEN
         TYP = 'DETE'
         DA  = UN/DA
         X(1) = DB1*DA
         X(2) = DB2*DA
         X(3) = DB3*DA
      ELSE
         DB  = DB1*DB1 + DB2*DB2 + DB3*DB3
         IF ( ABS(DB) .GT. EPSI ) THEN
            TYP = 'INCO'
         ELSE
            TYP = 'INDE'
         ENDIF
      ENDIF
      END
