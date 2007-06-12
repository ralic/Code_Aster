      SUBROUTINE D2DIAG(A,D,S,THETA)
      
        IMPLICIT  NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/09/2006   AUTEUR MARKOVIC D.MARKOVIC 
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

       REAL*8       ZERO

      REAL*8        A(2,2)
      REAL*8        D(2),S(2,2),THETA
      REAL*8        B(3),AUX
      DATA         ZERO  /1.D-10/
      

      B(1)=A(1,1)
      B(2)=A(2,2)
      B(3)=A(1,2)

      CALL DI2EPX(B,D)

      IF (ABS(A(1,2)) .GT. ZERO*(ABS(D(1))+ABS(D(2)))) THEN
        AUX = A(2,2)-A(1,1)
C         THETA = ATAN( (AUX + SQRT(AUX**2 + 4*A(1,2)**2))
C      &          / (2 * A(1,2)) )

        AUX = (AUX + SQRT(AUX**2 + 4*A(1,2)**2)) / (2 * A(1,2))
        THETA = ATAN2(AUX,1.0D0)
        
        S(1,1) = COS(THETA)
        S(2,2) = S(1,1)
        S(1,2) = SIN(THETA)
        S(2,1) = -S(1,2)
      ELSE
        THETA = 0.D0
        S(1,1) = 1.D0
        S(2,2) = 1.D0
        S(1,2) = 0.D0
        S(2,1) = 0.D0
      ENDIF
      END 
