      SUBROUTINE ROTATI (EULER,ROT)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/06/2010   AUTEUR CORUS M.CORUS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C-----------------------------------------------------------------------
C    M. CORUS     DATE 03/02/10
C-----------------------------------------------------------------------
C  BUT:      < CALCUL DE MATRICE DE ROTATION >
C
C  CONSTRUIRE LA MATRICE DE ROTATION COMPLETE A PARTIR DES ANGLES 
C  NAUTIQUES
C
C   NB : Equations de passage : un vecteur de coordonnees initiales 
C       (X,Y,Z) a pour image le vecteur (X1,Y1,Z1), tel que
C    _                  _    _                  _
C   | 1    0      0     |   | cos(B)  0  sin(B) |
C   | 0  cos(G) -sin(G) | x |   0     1   0     | x ...
C   |_0  sin(G)  cos(G)_|   |_-sin(B) 0 cos(B) _|
C
C   _                   _    _  _    _   _
C  | cos(A) -sin(A)  0  |   | X |   | X1 |
C  | sin(A)  cos(A)  0  | x | Y | = | Y1 |
C  |_ 0       0      1 _|   |_Z_|   |_Z1_|
C 
C   A (alpha), B(beta), gamma (G) sont les angle nautiques
C
C  ON CONSTRUIT ROT TELLE QUE :
C
C   _    _ _  _    _   _
C  |     || X |   | X1 |
C  | ROT || Y | = | Y1 |
C  |_   _||_Z_|   |_Z1_|
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C EULER    /I/: VECTEUR CONTENANT LES 3 ANGLES NAUTIQUES
C ROT      /I/: MATRICE 3x3 DE LA ROTATION

C-- VARIABLES EN ENTREES / SORTIE      
      REAL*8    EULER(3),ROTA(3,3)
      
C-- VARIABLES DE LA ROUTINE       
      REAL*8    ROTB(3,3),ROTC(3,3),ROTD(3,3),ROT(3,3)
      INTEGER   I1,J1,K1
      
      ROTA(1,1)=COS(EULER(1))
      ROTA(1,2)=-SIN(EULER(1))
      ROTA(1,3)=0.D0
      ROTA(2,1)=-ROTA(1,2)
      ROTA(2,2)=ROTA(1,1)
      ROTA(2,3)=0.D0
      ROTA(3,1)=0.D0
      ROTA(3,2)=0.D0
      ROTA(3,3)=1.D0
      
      ROTB(1,1)=COS(EULER(2))
      ROTB(1,2)=0.D0
      ROTB(1,3)=SIN(EULER(2))  
      ROTB(2,1)=0.D0
      ROTB(2,2)=1.D0
      ROTB(2,3)=0.D0
      ROTB(3,1)=-ROTB(1,3)    
      ROTB(3,2)=0.D0
      ROTB(3,3)=ROTB(1,1)
      
      ROTC(1,1)=1.D0
      ROTC(1,2)=0.D0
      ROTC(1,3)=0.D0
      ROTC(2,1)=0.D0
      ROTC(2,2)=COS(EULER(3))    
      ROTC(2,3)=-SIN(EULER(3))
      ROTC(3,1)=0.D0
      ROTC(3,2)= -ROTC(2,3)  
      ROTC(3,3)=ROTC(2,2)
      
      ROTD(1,1)=0.D0
      ROTD(1,2)=0.D0
      ROTD(1,3)=0.D0
      ROTD(2,1)=0.D0
      ROTD(2,2)=0.D0
      ROTD(2,3)=0.D0
      ROTD(3,1)=0.D0
      ROTD(3,2)=0.D0
      ROTD(3,3)=0.D0
      
      ROT(1,1)=0.D0
      ROT(1,2)=0.D0
      ROT(1,3)=0.D0
      ROT(2,1)=0.D0
      ROT(2,2)=0.D0
      ROT(2,3)=0.D0
      ROT(3,1)=0.D0
      ROT(3,2)=0.D0
      ROT(3,3)=0.D0
                
      DO 20 J1=1,3
        DO 30 I1=1,3
          DO 40 K1=1,3
              ROTD(I1,J1)=ROTD(I1,J1)+ROTB(I1,K1)*ROTA(K1,J1)
  40      CONTINUE        
  30    CONTINUE
  20  CONTINUE
      
      DO 50 J1=1,3
        DO 60 I1=1,3
          DO 70 K1=1,3
              ROT(I1,J1)=ROT(I1,J1)+ROTC(I1,K1)*ROTD(K1,J1)
  70      CONTINUE        
  60    CONTINUE
  50  CONTINUE
      
      END
