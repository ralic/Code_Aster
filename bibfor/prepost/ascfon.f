      FUNCTION ASCFON(Y)
      IMPLICIT NONE
C---------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/03/99   AUTEUR AUBHHMB M.BONNAMY 
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
C     MACR_ASCOUF_MAIL
C 
C     FONCTION F(CP)=CC
C     OU CC EST LA TAILLE DE LA FISSURE  SUR LE COUDE 
C     ET CP LA TAILLE DE FISSURE SUR LA PLAQUE
C---------------------------------------------------------------
C
      REAL*8      ASCFON
C      
      REAL*8      RM, RC, EP, AZIM, AXEC, ORIEN
      CHARACTER*8 POS     
      COMMON/ ASCFOR / RC,RM,EP,ORIEN,AZIM,AXEC
      COMMON/ ASCFOC / POS
C
      REAL*8       X, Y, SIG, PI, AZIMR, R8PI
C      
      PI = R8PI()
      AZIMR = AZIM*2.D0*PI/360.D0
C         
      IF (POS.EQ.'DEB_INT') THEN
        X = RM-EP/2.D0
      ELSE
        X = RM+EP/2.D0
      END IF
C
      IF (ABS(ORIEN-45.D0).LT.1.D-2) THEN
         SIG = 1.D0
      ELSE
         SIG = -1.D0
      END IF
C
      ASCFON = - SIG*X*RC/(2.D0*RM*SIN(AZIMR)) * (
     &             LOG ( RM/X+RM/RC*(COS(AZIMR)-
     &                    SIN(AZIMR)*SIG*Y/(SQRT(2.D0)*RM))            
     &                  + SQRT( 1.D0 + ( RM/X+RM/RC*(COS(AZIMR)-
     &                    SIN(AZIMR)*SIG*Y/(SQRT(2.D0)*RM)) )**2 )
     &                  )
     &           - LOG ( RM/X+RM/RC*COS(AZIMR)             
     &                  + SQRT( 1.D0 +(RM/X+RM/RC*COS(AZIMR))**2)
     &                  )                           )
     &          - SIG*X*RC/(2.D0*RM*SIN(AZIMR)) * (
     &              ( RM/X+RM/RC* ( COS(AZIMR)-
     &                SIN(AZIMR)*SIG*Y/(SQRT(2.D0)*RM) )             
     &               )  * SQRT( 1.D0 + ( RM/X+RM/RC*(COS(AZIMR)-
     &                    SIN(AZIMR)*SIG*Y/(SQRT(2.D0)*RM)) )**2 )
     &            - ( RM/X+RM/RC*COS(AZIMR) )          
     &                  * SQRT( 1.D0 +(RM/X+RM/RC*COS(AZIMR))**2)
     &                                             )
     &          - 2.D0*AXEC        
C
      END
