      SUBROUTINE SSPMVC(N,M,MAT,AD,T1,Y)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/02/97   AUTEUR JFBHHUC C.ROSE 
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
C     VERSION COMPLEXE DE SSPMVA 
      INTEGER N,M,AD(*)
      COMPLEX*16 MAT(*),T1(*),Y(*)
      INTEGER I,J,JMIN,JMAX,REST,K0,K1,K2,K3,K4,K5,K6,K7
      REST=M - (M/8)* 8
      IF( REST.LE.3) THEN
C
         IF( REST.LE.1) THEN

           IF( REST.EQ.1 ) THEN
              K1 = AD(1)
              DO 1 I=1,N
              Y(I) = (Y(I) ) - T1(1)*MAT(K1)
              K1 = K1 +1
1             CONTINUE
           ENDIF
         ELSE
           IF( REST.EQ.2 ) THEN
              K1 = AD(1)
              K2 = AD(2)
              DO 2 I=1,N
              Y(I) = ( (Y(I) ) - T1(1)*MAT(K1) )- T1(2)*MAT(K2)
              K1 = K1 +1
              K2 = K2 +1
2             CONTINUE
           ELSE
              K1 = AD(1)
              K2 = AD(2)
              K3 = AD(3)
              DO 3 I=1,N
              Y(I) = (((Y(I) ) - T1(1)*MAT(K1)) - T1(2)*MAT(K2))
     %                         - T1(3)*MAT(K3)
              K1 = K1 +1
              K2 = K2 +1
              K3 = K3 +1
3             CONTINUE
           ENDIF
         ENDIF
      ELSE
         IF( REST.LE.5) THEN

           IF( REST.EQ.4 ) THEN
              K1 = AD(1)
              K2 = AD(2)
              K3 = AD(3)
              K4 = AD(4)
              DO 4 I=1,N
              Y(I) = ((((Y(I) ) - T1(1)*MAT(K1)) - T1(2)*MAT(K2))
     %                         - T1(3)*MAT(K3))  - T1(4)*MAT(K4)
              K1 = K1 +1
              K2 = K2 +1
              K3 = K3 +1
              K4 = K4 +1
4             CONTINUE
           ELSE
              K1 = AD(1)
              K2 = AD(2)
              K3 = AD(3)
              K4 = AD(4)
              K5 = AD(5)
              DO 5 I=1,N
              Y(I) = (((((Y(I) ) - T1(1)*MAT(K1)) - T1(2)*MAT(K2))
     %                           - T1(3)*MAT(K3)) - T1(4)*MAT(K4))
     %                           - T1(5)*MAT(K5)
              K1 = K1 +1
              K2 = K2 +1
              K3 = K3 +1
              K4 = K4 +1
              K5 = K5 +1
5             CONTINUE
           ENDIF
      ELSE
           IF( REST.EQ.6 ) THEN
              K1 = AD(1)
              K2 = AD(2)
              K3 = AD(3)
              K4 = AD(4)
              K5 = AD(5)
              K6 = AD(6)
              DO 6 I=1,N
              Y(I) = ((((((Y(I) ) - T1(1)*MAT(K1)) - T1(2)*MAT(K2))
     %                           - T1(3)*MAT(K3)) - T1(4)*MAT(K4))
     %                           - T1(5)*MAT(K5)) - T1(6)*MAT(K6)
              K1 = K1 +1
              K2 = K2 +1
              K3 = K3 +1
              K4 = K4 +1
              K5 = K5 +1
              K6 = K6 +1
6             CONTINUE

           ELSE
              K1 = AD(1)
              K2 = AD(2)
              K3 = AD(3)
              K4 = AD(4)
              K5 = AD(5)
              K6 = AD(6)
              K7 = AD(7)
              DO 7 I=1,N
              Y(I) = (((((((Y(I) ) - T1(1)*MAT(K1)) - T1(2)*MAT(K2))
     %                           -  T1(3)*MAT(K3)) - T1(4)*MAT(K4))
     %                           -  T1(5)*MAT(K5)) - T1(6)*MAT(K6))
     %                           -  T1(7)*MAT(K7)
              K1 = K1 +1
              K2 = K2 +1
              K3 = K3 +1
              K4 = K4 +1
              K5 = K5 +1
              K6 = K6 +1
              K7 = K7 +1
7             CONTINUE 
          ENDIF
        ENDIF
      ENDIF
      JMIN= REST+8
       JMAX = M
      IF( JMAX.GE.JMIN) THEN
      DO 100 J=JMIN,JMAX,8
              K0 = AD(J)
              K1 = AD(J-1)
              K2 = AD(J-2)
              K3 = AD(J-3)
              K4 = AD(J-4)
              K5 = AD(J-5)
              K6 = AD(J-6)
              K7 = AD(J-7)
              DO 8 I=1,N
      Y(I) = (((((((( Y(I) ) - T1(J)*  MAT(K0)) -  T1(J-1)*MAT(K1)) 
     %                      - T1(J-2)*MAT(K2)) -  T1(J-3)*MAT(K3)) 
     %                      - T1(J-4)*MAT(K4)) -  T1(J-5)*MAT(K5)) 
     %                      - T1(J-6)*MAT(K6)) -  T1(J-7)*MAT(K7)    
              K0 = K0 +1
              K1 = K1 +1
              K2 = K2 +1
              K3 = K3 +1
              K4 = K4 +1
              K5 = K5 +1
              K6 = K6 +1
              K7 = K7 +1
8             CONTINUE 
C
100   CONTINUE
      ENDIF
       END
