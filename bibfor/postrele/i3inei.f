      SUBROUTINE I3INEI(E1,E2,N1,N2,IRET)
      IMPLICIT NONE
C
      INTEGER E1(*),E2(*),N1,N2,IRET
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
C     INCLUSION DE E1 DANS E2
C     ------------------------------------------------------------------
C IN  E1     : I : ENSEMBLE 1
C IN  E2     : I : ENSEMBLE 2
C IN  N1     : I : CARD(E1)
C IN  N2     : I : CARD(E2)
C OUT IRET   : I : REPONSE : E1 C E2 ==> IRET = 1  SINON  IRET = 0
C     ------------------------------------------------------------------
C
      INTEGER I1,I2,N,CMPT
      LOGICAL INCLUS
C
C======================================================================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INCLUS = .TRUE.
      I1     =  1
100   CONTINUE
      IF ( INCLUS .AND. (I1 .LE. N1) ) THEN
         N    = E1(I1)
         CMPT = 0
         DO 10, I2 = 1, N2, 1
            CMPT = CMPT + MAX(0,1-ABS(N-E2(I2)))
10       CONTINUE
         INCLUS = (CMPT .GT. 0)
         I1     = I1 + 1
         GOTO 100
      ENDIF
      IF ( INCLUS ) THEN
         IRET = 1
      ELSE
         IRET = 0
      ENDIF
      END
