      SUBROUTINE UT2VGL ( NN , NC , P , VG , VL )
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
      REAL*8              P(3,3) , VG(*) , VL(*)
C     ------------------------------------------------------------------
C     PASSAGE EN 2D D'UN VECTEUR NN*NC DU REPERE GLOBAL AU REPERE LOCAL
C     ------------------------------------------------------------------
CIN   I   NN   NOMBRE DE NOEUDS
CIN   I   NC   NOMBRE DE COMPOSANTES
CIN   R   P    MATRICE DE PASSAGE 3D DE GLOBAL A LOCAL
CIN   R   VG   NN*NC COMPOSANTES DU VECTEUR DANS GLOBAL
COUT  R   VL   NN*NC COMPOSANTES DU VECTEUR DANS LOCAL
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,NC ,NN 
C-----------------------------------------------------------------------
      IF ( MOD(NC,2) .EQ. 0 ) THEN
         DO 10 I = 1 , NN * NC , 2
           VL(I  ) = P(1,1)*VG(I)   + P(1,2)*VG(I+1)
           VL(I+1) = P(2,1)*VG(I)   + P(2,2)*VG(I+1) 
 10      CONTINUE
C
      ELSEIF ( MOD(NC,2) .EQ. 1 ) THEN
         DO 20 I = 1 , NN * NC , 3
           VL(I  ) = P(1,1)*VG(I)   + P(1,2)*VG(I+1)
           VL(I+1) = P(2,1)*VG(I)   + P(2,2)*VG(I+1) 
           VL(I+2) = VG(I+2)
 20      CONTINUE
C
      ENDIF
C
      END
