      SUBROUTINE PRVITE(VEC1,LONG,IP1,IP2,ITP)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C      CALCUL DU PROFIL DE VITESSE
C ----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      REAL*8     ANGLE(71), VITE(71), ANGL, VEC1(LONG)
      INTEGER    IP(3)
C
C-----------------------------------------------------------------------
      INTEGER I ,IJ ,IP1 ,IP2 ,ITP ,J ,K 
      INTEGER KK ,LONG ,LONG2 
      REAL*8 ALFA ,BETA 
C-----------------------------------------------------------------------
      DATA (ANGLE(I),I=1,17)   /
     +   0.D0   ,  15.3D0  ,  30.D0   ,  40.D0   ,  48.5D0  ,  61.3D0  ,
     +  75.D0   ,  83.6D0  ,  90.D0   ,  96.4D0  , 105.D0   , 118.7D0  ,
     + 131.5D0  , 140.D0   , 150.D0   , 164.7D0  , 180.D0 /
C
      DATA (ANGLE(I),I=18,42)  /
     +   0.D0   ,  14.5D0  ,  29.D0   ,  33.49D0 ,  33.51D0 ,  39.D0   ,
     +  47.4D0  ,  60.D0   ,  70.99D0 ,  71.01D0 ,  75.4D0  ,  84.D0   ,
     +  90.D0   ,  96.D0   , 104.6D0  , 108.99D0 , 109.01D0 , 120.D0   ,
     + 132.6D0  , 141.D0   , 146.49D0 , 146.51D0 , 151.D0   , 165.D0   ,
     + 180.D0   /
C
      DATA (ANGLE(I),I=43,71)  /
     +   0.D0   ,  14.8D0  ,  25.7D0  ,  25.72D0 ,  30.7D0  ,  39.6D0  ,
     +  48.D0   ,  51.42D0 ,  51.44D0 ,  60.3D0  ,  75.2D0  ,  77.13D0 ,
     +  77.15D0 ,  84.D0   ,  90.D0   ,  96.D0   , 102.85D0 , 102.87D0 ,
     + 104.8D0  , 119.7D0  , 128.56D0 , 128.58D0 , 132.D0   , 140.4D0  ,
     + 149.3D0  , 154.28D0 , 154.3D0  , 165.2D0  , 180.D0  /
C
      DATA (VITE(J),J=1,17)   /
     +   0.D0   ,   1.08D0 ,   1.77D0 ,   1.72D0 ,   1.33D0 ,   0.95D0 ,
     +   0.67D0 ,   0.59D0 ,   0.58D0 ,   0.59D0 ,   0.67D0 ,   0.95D0 ,
     +   1.33D0 ,   1.72D0 ,   1.77D0 ,   1.08D0 ,   0.D0  /
C
      DATA (VITE(J),J=18,42)  /
     +   0.D0   ,   0.2D0  ,   0.7D0  ,   1.D0   ,   0.18D0 ,   0.32D0 ,
     +   0.48D0 ,   0.61D0 ,   0.7D0  ,   0.16D0 ,   0.29D0 ,   0.5D0  ,
     +   0.54D0 ,   0.5D0  ,   0.29D0 ,   0.16D0 ,   0.7D0  ,   0.61D0 ,
     +   0.48D0 ,   0.32D0 ,   0.18D0 ,   1.D0   ,   0.7D0  ,   0.2D0  ,
     +   0.D0   /
C
      DATA (VITE(J),J=43,71) /
     +   0.D0   ,   0.48D0 ,   0.69D0 ,   0.27D0 ,   0.54D0 ,   0.89D0 ,
     +   1.16D0 ,   1.24D0 ,   0.02D0 ,   0.08D0 ,   0.79D0 ,   0.86D0 ,
     +   0.17D0 ,   0.49D0 ,   0.55D0 ,   0.49D0 ,   0.17D0 ,   0.86D0 ,
     +   0.79D0 ,   0.08D0 ,   0.02D0 ,   1.24D0 ,   1.16D0 ,   0.89D0 ,
     +   0.54D0 ,   0.27D0 ,   0.69D0 ,   0.48D0 ,   0.D0 /
C
      DATA (IP(IJ),IJ=1,3)  / 1 , 18 , 43 /

C
      LONG2 = LONG/2
C
      DO 10 KK=1,LONG2
        IF ( KK.GT.IP1 .AND. KK.LT.IP2) THEN
          ANGL = 180.D0*(VEC1(KK)-VEC1(IP1))/(VEC1(IP2)-VEC1(IP1))
          K = IP(ITP)
  20      CONTINUE
          IF(ANGL.GT.ANGLE(K+1)) THEN
            K = K+1
            GOTO 20
          ENDIF
          ALFA = (VITE(K+1)-VITE(K))/(ANGLE(K+1)-ANGLE(K))
          BETA = (VITE(K)*ANGLE(K+1)-(VITE(K+1)*ANGLE(K)))/
     +             (ANGLE(K+1)-ANGLE(K))
          VEC1(LONG2+KK) = ALFA*ANGL+BETA
        ELSE
          VEC1(LONG2+KK) = 0.D0
        ENDIF
  10  CONTINUE
      END
