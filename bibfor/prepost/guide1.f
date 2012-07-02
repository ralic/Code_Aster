      SUBROUTINE GUIDE1(RCARTE,D,THET,RAYO)
      IMPLICIT NONE
      REAL*8 THETA,PAS,ALPHA1,ALPHA2,RCARTE,D,PI,RHO,RAD
      REAL*8 THET(801),RAYO(801)
      REAL*8 TRIGOM
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
C  CALCUL D'UN OBSTACLE GUIDE A 1 ENCOCHE  
C              
C**********************************************************
C NOMBRE DE PAS DE DISCRETISATION
C-----------------------------------------------------------------------
      INTEGER I ,N 
      REAL*8 R8DGRD ,R8PI 
C-----------------------------------------------------------------------
      N=800
C**********************************************************
      PI = R8PI( )
      RAD = R8DGRD()             
      PAS = 360.D0/N
      THETA = 0.D0
      ALPHA1 = 20.D0
      ALPHA2 = TRIGOM('ASIN',D/RCARTE)*180.D0/PI
      DO 1 I=1,(N+1)
         IF ((THETA.LT.ALPHA1).OR.(THETA.GT.(360.D0-ALPHA1))) THEN
            RHO=D/TAN(ALPHA1*PI/180.D0)/COS(THETA*PI/180.D0)
         ENDIF
         IF ((THETA.GE.ALPHA1).AND.(THETA.LT.ALPHA2)) THEN
            RHO=D/SIN(THETA*PI/180.D0)
         ENDIF
         IF ((THETA.GT.(360.D0-ALPHA2)).AND.
     +        (THETA.LE.(360.D0-ALPHA1))) THEN
            RHO=-D/SIN(THETA*PI/180.D0)
         ENDIF
         IF ((THETA.GE.ALPHA2).AND.(THETA.LE.(360.D0-ALPHA2))) THEN
            RHO=RCARTE
         ENDIF
         THET(I) = THETA * RAD
         RAYO(I) = RHO
         THETA=THETA+PAS
    1 CONTINUE
      END
