      SUBROUTINE PLCENT(DIM,SC,PS,NP,G)
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
C CENTRE DE GRAVITE D'UN POLYGONE OU D'UN POLYEDRE A FACES TRIANGULAIRES
C ----------------------------------------------------------------------
C VARIABLES EN ENTREE
C REAL*8     SC(DIM,*)  : COORDONNEES DES SOMMETS
C INTEGER    PS(DIM,*)  : CONNECTIVITE DES PANS 
C                         ORIENTATION NORMALE SORTANTE
C INTEGER    NP         : NOMBRE DE PANS
C
C VARIABLES EN SORTIE
C REAL*8     G(DIM)       : COORDONNEES DU CENTRE DE GRAVITE
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
 
      INTEGER DIM,PS(DIM,*),NP,A,B,C,P,I
      REAL*8  SC(DIM,*),G(*),U(3),V(3),W(3),O(3),R,R0

C --- CENTRE DE GRAVITE D'UN POLYGONE

      IF (DIM.EQ.2) THEN

        R = 0.D0
        G(1) = 0.D0
        G(2) = 0.D0

        DO 10 P = 1, NP

          A = PS(1,P)
          B = PS(2,P)

          R0 = SC(1,A)*SC(2,B) - SC(2,A)*SC(1,B)

          G(1) = G(1) + (SC(1,A) + SC(1,B))*R0
          G(2) = G(2) + (SC(2,A) + SC(2,B))*R0

          R = R + R0

 10     CONTINUE

        R = R*3.D0
        G(1) = G(1)/R
        G(2) = G(2)/R

C --- CENTRE DE GRAVITE D'UN POLYEDRE

      ELSE

        R = 0.D0
        G(1) = 0.D0
        G(2) = 0.D0
        G(3) = 0.D0

        A = PS(1,1)
        O(1) = SC(1,A)
        O(2) = SC(2,A)
        O(3) = SC(3,A)

        DO 20 P = 1, NP
         
          A = PS(1,P)
          B = PS(2,P)
          C = PS(3,P)

          DO 30 I = 1, 3
 
            U(I) = SC(I,A) - O(I)
            V(I) = SC(I,B) - O(I)
            W(I) = SC(I,C) - O(I)
         
 30       CONTINUE

          R0 = U(1)*V(2)*W(3) - U(2)*V(1)*W(3)
     &       + U(2)*V(3)*W(1) - U(3)*V(2)*W(1)
     &       + U(3)*V(1)*W(2) - U(1)*V(3)*W(2)

          G(1) = G(1) + (U(1) + V(1) + W(1))*R0
          G(2) = G(2) + (U(2) + V(2) + W(2))*R0
          G(3) = G(3) + (U(3) + V(3) + W(3))*R0

          R = R + R0

 20     CONTINUE

        R = R*4.D0
        G(1) = O(1) + G(1)/R
        G(2) = O(2) + G(2)/R
        G(3) = O(3) + G(3)/R

      ENDIF

      END
