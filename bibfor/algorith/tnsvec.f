      SUBROUTINE TNSVEC(CHOIX, NDIM, MAT,VEC,R ) 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER NDIM, I,J, CHOIX
      REAL*8 MAT(3,3),VEC(2*NDIM),R
C     FONCTION TRANSFORMANT UNE MATRICE SYMETRIQUE (TENSEUR) 3,3
C     EN VECTEUR 6 ET MULTIPLIANT LES TERMES NON DIAGONAUX PAR 
C     UN REEL R ET INVERSEMENT. 
C     IN : CHOIX =3 OU 6 (DIMENSION DE L'OBJET EN ENTREE)
C     Si CHOIX=3
C        IN : MAT
C        OUT : VEC
C     SI CHOIX=6
C        IN : VEC
C        OUT : MAT     

      IF (CHOIX.EQ.3) THEN
      
C        TRANSFORMATION MATRICE EN VECTEUR
         DO 300 I=1,3
            VEC(I)=MAT(I,I)
 300     CONTINUE
         VEC(4)=MAT(1,2)*R
         VEC(5)=MAT(1,3)*R
         VEC(6)=MAT(2,3)*R
      
      
      ELSEIF (CHOIX.EQ.6) THEN
      
C        TRANSFORMATION VECTEUR EN MATRICE
         DO 600 I=1,3
            MAT(I,I)=VEC(I)
 600     CONTINUE
         MAT(1,2)=VEC(4)*R
         MAT(2,1)=VEC(4)*R
         IF(NDIM.EQ.2) THEN
            MAT(1,3)=0.D0
            MAT(2,3)=0.D0
            MAT(3,1)=0.D0
            MAT(3,2)=0.D0
         ELSE
            MAT(1,3)=VEC(5)*R
            MAT(3,1)=VEC(5)*R
            MAT(2,3)=VEC(6)*R
            MAT(3,2)=VEC(6)*R
         ENDIF
      ELSE
         CALL ASSERT(CHOIX.EQ.3)
      ENDIF

      END
