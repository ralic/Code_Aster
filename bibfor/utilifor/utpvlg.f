      SUBROUTINE UTPVLG ( NN , NC , P , VL , VG )
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
      IMPLICIT NONE
      REAL*8              P(3,3) , VL(*) , VG(*)
C     ------------------------------------------------------------------
C     PASSAGE EN 3D D'UN VECTEUR NN*NC DU REPERE LOCAL AU REPERE GLOBAL
C     ------------------------------------------------------------------
CIN   I   NN   NOMBRE DE NOEUDS
CIN   I   NC   NOMBRE DE COMPOSANTES
CIN   R   P    MATRICE DE PASSAGE 3D DE GLOBAL A LOCAL
CIN   R   VL   NN*NC COMPOSANTES DU VECTEUR DANS LOCAL
COUT  R   VG   NN*NC COMPOSANTES DU VECTEUR DANS GLOBAL
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,NC ,NN 
C-----------------------------------------------------------------------
      IF ( MOD(NC,3) .EQ. 0 ) THEN
         DO 10 I = 1 , NN * NC , 3
           VG(I  ) = P(1,1)*VL(I)   + P(2,1)*VL(I+1) + P(3,1)*VL(I+2)
           VG(I+1) = P(1,2)*VL(I)   + P(2,2)*VL(I+1) + P(3,2)*VL(I+2)
           VG(I+2) = P(1,3)*VL(I)   + P(2,3)*VL(I+1) + P(3,3)*VL(I+2)
 10      CONTINUE
C
      ELSEIF ( MOD(NC,3) .EQ. 1 ) THEN
         DO 20 I = 1 , NN * NC , 7
           VG(I  ) = P(1,1)*VL(I)   + P(2,1)*VL(I+1) + P(3,1)*VL(I+2)
           VG(I+1) = P(1,2)*VL(I)   + P(2,2)*VL(I+1) + P(3,2)*VL(I+2)
           VG(I+2) = P(1,3)*VL(I)   + P(2,3)*VL(I+1) + P(3,3)*VL(I+2)
           VG(I+3) = P(1,1)*VL(I+3) + P(2,1)*VL(I+4) + P(3,1)*VL(I+5)
           VG(I+4) = P(1,2)*VL(I+3) + P(2,2)*VL(I+4) + P(3,2)*VL(I+5)
           VG(I+5) = P(1,3)*VL(I+3) + P(2,3)*VL(I+4) + P(3,3)*VL(I+5)
           VG(I+6) = VL(I+6)
 20      CONTINUE
C
      ELSEIF ( MOD(NC,3) .EQ. 2 ) THEN
         DO 30 I = 1 , NN * NC , 8
           VG(I  ) = P(1,1)*VL(I)   + P(2,1)*VL(I+1) + P(3,1)*VL(I+2)
           VG(I+1) = P(1,2)*VL(I)   + P(2,2)*VL(I+1) + P(3,2)*VL(I+2)
           VG(I+2) = P(1,3)*VL(I)   + P(2,3)*VL(I+1) + P(3,3)*VL(I+2)
           VG(I+3) = P(1,1)*VL(I+3) + P(2,1)*VL(I+4) + P(3,1)*VL(I+5)
           VG(I+4) = P(1,2)*VL(I+3) + P(2,2)*VL(I+4) + P(3,2)*VL(I+5)
           VG(I+5) = P(1,3)*VL(I+3) + P(2,3)*VL(I+4) + P(3,3)*VL(I+5)
           VG(I+6) = VL(I+6)
           VG(I+7) = VL(I+7)
 30      CONTINUE
      ENDIF
C
      END
