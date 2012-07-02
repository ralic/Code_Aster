        FUNCTION LCQEQV( X , Y  )
      IMPLICIT NONE
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C       ----------------------------------------------------------------
C       EGALITE DE 2 VECTEURS  X =? Y
C       IN  X      :  VECTEUR
C       IN  Y      :  VECTEUR
C       OUT LCQEQV :  REPONSE = 'OUI' OU 'NON'
C       ----------------------------------------------------------------
        REAL*8          EPSI
C-----------------------------------------------------------------------
      INTEGER I 
C-----------------------------------------------------------------------
        PARAMETER       ( EPSI = 1.D-9 )
        INTEGER         N , ND
        REAL*8          X(6),   Y(6)
        CHARACTER*3     LCQEQV
        COMMON /TDIM/   N , ND
C       ----------------------------------------------------------------
        DO 1 I = 1 , N
                IF ( ABS (X(I) - Y(I)) .GT. EPSI )THEN
                LCQEQV = 'NON'
                GOTO 9999
                ENDIF
 1      CONTINUE
        LCQEQV = 'OUI'
C
 9999   CONTINUE
        END
