      SUBROUTINE VPZTR2(NN,IA,A,X)
C
C**********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/05/96   AUTEUR KXBADNG T.FRIOU 
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
C**********************************************************************
C     ROUTINE BASEE SUR LA PROCEDURE INNERPROD
C     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.340)
C
C**********************************************************************
C                           DECLARATIONS
C**********************************************************************
C
      IMPLICIT NONE
C
C ARGUMENTS
C ---------
      INTEGER NN, IA
      REAL*8 A(IA,*), X(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER I, J
C
C**********************************************************************
C                   DEBUT DU CODE EXECUTABLE
C**********************************************************************
C
      DO 40 I=1, NN
        IF (X(I) .NE. 0.D0) THEN
            DO 20 J=I+1, NN
              X(J) = X(J) - X(I)*A(J,I)
 20         CONTINUE
        ENDIF
 40   CONTINUE
C
      END
