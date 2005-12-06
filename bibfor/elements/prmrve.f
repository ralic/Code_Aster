      SUBROUTINE PRMRVE(A,NL,NC,B,C)
      IMPLICIT NONE
      INTEGER I,J,NL,NC
      REAL*8 A(NL,NC),B(NC),C(NL),S
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/10/2004   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C-----------------------------------------------------------------------
C       CALCUL LE PRODUIT D'UNE MATRICE PAR UN VECTEUR 
C                         C(NL) = A(NL,NC) x B(NC)
C
C       IN      A = MATRICE (NL,NC)
C               B = VECTEUR (NC)
C       OUT     C = VECTEUR (NL)
C-----------------------------------------------------------------------
C
      DO 10 I=1,NL
         S=0.D0
         DO 20 J=1,NC
            S=S+A(I,J)*B(J)
 20      CONTINUE
         C(I)=S
 10   CONTINUE
      END
