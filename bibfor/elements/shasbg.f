      SUBROUTINE SHASBG(BGLOB,B,P)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2003   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C--------------------------------------------------------
C ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
C-------------------------------------------------------
C TRANSFORME B(6,24) DANS LOCAL EN BGLOB(6,24) DANS GLOBAL
C AVEC P(3,3) MATRICE DE PASSAGE
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 BGLOB(6,24),B(3,8),P(3,3),BTMP(6,24)
      INTEGER J
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL ZDANUL(BGLOB,144)
C      CALL ZDANUL(BTMP,144)
      CALL R8INIR(144,0.D0,BGLOB,1)
      CALL R8INIR(144,0.D0,BTMP,1)
      
C PREMIERE LIGNE:
      DO 10 J = 1,8
C "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS11
        BTMP(1,J) = B(1,J)*P(1,1)*P(1,1) + B(2,J)*P(1,1)*P(2,1) +
     &              B(3,J)*P(3,1)*P(1,1)
   10 CONTINUE
      DO 20 J = 9,16
C "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS11
        BTMP(1,J) = P(2,1)*B(1,J-8)*P(1,1) + P(2,1)*B(2,J-8)*P(2,1) +
     &              P(2,1)*B(3,J-8)*P(3,1)
   20 CONTINUE
      DO 30 J = 17,24
C "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS11
        BTMP(1,J) = P(3,1)*B(1,J-16)*P(1,1) + P(3,1)*B(2,J-16)*P(2,1) +
     &              P(3,1)*B(3,J-16)*P(3,1)
   30 CONTINUE
C 2IEME LIGNE:
      DO 40 J = 1,8
C "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS22
        BTMP(2,J) = P(1,2)*B(1,J)*P(1,2) + P(1,2)*B(2,J)*P(2,2) +
     &              P(1,2)*B(3,J)*P(3,2)
   40 CONTINUE
      DO 50 J = 9,16
C "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS22
        BTMP(2,J) = P(2,2)*B(1,J-8)*P(1,2) + P(2,2)*B(2,J-8)*P(2,2) +
     &              P(2,2)*B(3,J-8)*P(3,2)
   50 CONTINUE
      DO 60 J = 17,24
C "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS22
        BTMP(2,J) = P(3,2)*B(1,J-16)*P(1,2) + P(3,2)*B(2,J-16)*P(2,2) +
     &              P(3,2)*B(3,J-16)*P(3,2)
   60 CONTINUE
C 3IEME LIGNE:
      DO 70 J = 1,8
C "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS33
        BTMP(3,J) = P(1,3)*B(1,J)*P(1,3) + P(1,3)*B(2,J)*P(2,3) +
     &              P(1,3)*B(3,J)*P(3,3)
   70 CONTINUE
      DO 80 J = 9,16
C "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS33
        BTMP(3,J) = P(2,3)*B(1,J-8)*P(1,3) + P(2,3)*B(2,J-8)*P(2,3) +
     &              P(2,3)*B(3,J-8)*P(3,3)
   80 CONTINUE
      DO 90 J = 17,24
C "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS33
        BTMP(3,J) = P(3,3)*B(1,J-16)*P(1,3) + P(3,3)*B(2,J-16)*P(2,3) +
     &              P(3,3)*B(3,J-16)*P(3,3)
   90 CONTINUE
C 4IEME LIGNE:
      DO 100 J = 1,8
C "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS12
        BTMP(4,J) = B(1,J)*P(1,1)*P(1,2)*2.D0 + B(2,J)*P(1,2)*P(2,1) +
     &              B(2,J)*P(1,1)*P(2,2) + B(3,J)*P(1,2)*P(3,1) +
     &              B(3,J)*P(1,1)*P(3,2)
  100 CONTINUE
      DO 110 J = 9,16
C "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS12
        BTMP(4,J) = B(1,J-8)*P(1,2)*P(2,1) + B(1,J-8)*P(1,1)*P(2,2) +
     &              B(2,J-8)*P(2,1)*P(2,2)*2.D0+B(3,J-8)*P(2,2)*P(3,1)+
     &              B(3,J-8)*P(2,1)*P(3,2)
  110 CONTINUE
      DO 120 J = 17,24
C "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS12
        BTMP(4,J) = B(1,J-16)*P(1,2)*P(3,1) + B(2,J-16)*P(2,2)*P(3,1) +
     &              B(1,J-16)*P(1,1)*P(3,2) + B(2,J-16)*P(2,1)*P(3,2) +
     &              B(3,J-16)*P(3,1)*P(3,2)*2.D0
  120 CONTINUE
C 5IEME LIGNE:
      DO 130 J = 1,8
C "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS23
        BTMP(5,J) = B(1,J)*P(1,2)*P(1,3)*2.D0 + B(2,J)*P(1,3)*P(2,2) +
     &              B(2,J)*P(1,2)*P(2,3) + B(3,J)*P(1,3)*P(3,2) +
     &              B(3,J)*P(1,2)*P(3,3)
  130 CONTINUE
      DO 140 J = 9,16
C "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS23
        BTMP(5,J) = B(1,J-8)*P(1,3)*P(2,2) + B(1,J-8)*P(1,2)*P(2,3) +
     &           B(2,J-8)*P(2,2)*P(2,3)*2.D0 + B(3,J-8)*P(2,3)*P(3,2) +
     &              B(3,J-8)*P(2,2)*P(3,3)
  140 CONTINUE
      DO 150 J = 17,24
C "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS23
        BTMP(5,J) = B(1,J-16)*P(1,3)*P(3,2) + B(2,J-16)*P(2,3)*P(3,2) +
     &              B(1,J-16)*P(1,2)*P(3,3) + B(2,J-16)*P(2,2)*P(3,3) +
     &              B(3,J-16)*P(3,2)*P(3,3)*2.D0
  150 CONTINUE
  
C 6IEME LIGNE:

      DO 160 J = 1,8
C "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS13
        BTMP(6,J) = B(1,J)*P(1,1)*P(1,3)*2.D0 + B(2,J)*P(1,3)*P(2,1) +
     &              B(2,J)*P(1,1)*P(2,3) + B(3,J)*P(1,3)*P(3,1) +
     &              B(3,J)*P(1,1)*P(3,3)
  160 CONTINUE
      DO 170 J = 9,16
C "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS13
        BTMP(6,J) = B(1,J-8)*P(1,3)*P(2,1) + B(1,J-8)*P(1,1)*P(2,3) +
     &           B(2,J-8)*P(2,1)*P(2,3)*2.D0 + B(3,J-8)*P(2,3)*P(3,1) +
     &              B(3,J-8)*P(2,1)*P(3,3)
  170 CONTINUE
      DO 180 J = 17,24
C "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS13
        BTMP(6,J) = B(1,J-16)*P(1,3)*P(3,1) + B(2,J-16)*P(2,3)*P(3,1) +
     &              B(1,J-16)*P(1,1)*P(3,3) + B(2,J-16)*P(2,1)*P(3,3) +
     &              B(3,J-16)*P(3,1)*P(3,3)*2.D0
  180 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 200 J = 1,24
        DO 190 I = 1,6
          BGLOB(I,J) = BTMP(I,J)
  190   CONTINUE
  200 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      END
