      FUNCTION PLVOL3(SC,FS,NF)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C        VOLUME D'UN POLYEDRE QUELCONQUE A FACES TRIANGULAIRES
C ----------------------------------------------------------------------
C VARIABLES EN ENTREE
C REAL*8     SC(3,*)  : COORDONNEES DES SOMMETS
C INTEGER    FS(3,*)  : CONNECTIVITE DES FACES 
C                       ORIENTATION NORMALE SORTANTE
C INTEGER    NF       : NOMBRE DE FACES
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
 
      INTEGER FS(3,*),NF,A,B,C,F,I
      REAL*8  SC(3,*),R(3),S(3),T(3),O(3),V,PLVOL3

C --- VOLUME DU POLYEDRE

      V = 0.D0

      A = FS(1,1)
      O(1) = SC(1,A)
      O(2) = SC(2,A)
      O(3) = SC(3,A)

      DO 10 F = 1, NF
         
        A = FS(1,F)
        B = FS(2,F)
        C = FS(3,F)

        DO 20 I = 1, 3
    
          R(I) = SC(I,A) - O(I)
          S(I) = SC(I,B) - O(I)
          T(I) = SC(I,C) - O(I)

 20     CONTINUE

        V = V + R(1)*S(2)*T(3) - R(2)*S(1)*T(3)
     &        + R(2)*S(3)*T(1) - R(3)*S(2)*T(1)
     &        + R(3)*S(1)*T(2) - R(1)*S(3)*T(2)
       
 10   CONTINUE

      PLVOL3 = V / 6.D0

      END
