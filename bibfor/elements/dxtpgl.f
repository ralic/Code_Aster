      SUBROUTINE DXTPGL ( XYZG , PGL )
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8               XYZG(3,*), PGL(3,3)
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/08/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     IN  XYZG  R  9   COORDONNEES  X1 Y1 Z1 X2 Y2 ...
C     OUT PGL   R 3,3  MATRICE DE PASSAGE GLOBAL INTRINSEQUE
C     -----------------------------------------------------------------
C     CONSTRUCTION DE LA MATRICE DE PASSAGE GLOBAL --> INTRINSEQUE
C     POUR UNE MAILLE TRIANGLE DKT OU DST
C                                                        3
C                                                        *
C        I : VECTEUR UNITAIRE PORTE PAR 12              *  *
C                                                      *     *
C        K : PERPENDICULAIRE A 12 ET A 13             *        *
C                                                    *           *
C        J : PRODUIT VECTORIEL K I                  ****************
C                                                  1               2
C
C     -----------------------------------------------------------------
      REAL*8   X21 , Y21 , Z21 , X31 , Y31 , Z31 , NORM
C     -----------------------------------------------------------------
      X21 = XYZG(1,2) - XYZG(1,1)
      Y21 = XYZG(2,2) - XYZG(2,1)
      Z21 = XYZG(3,2) - XYZG(3,1)
      X31 = XYZG(1,3) - XYZG(1,1)
      Y31 = XYZG(2,3) - XYZG(2,1)
      Z31 = XYZG(3,3) - XYZG(3,1)
C
      NORM = SQRT ( X21 * X21 + Y21 * Y21 + Z21 * Z21 )
      PGL(1,1) =   X21 / NORM
      PGL(1,2) =   Y21 / NORM
      PGL(1,3) =   Z21 / NORM
C
      PGL(3,1) =  Y21 * Z31 - Z21 * Y31
      PGL(3,2) =  Z21 * X31 - X21 * Z31
      PGL(3,3) =  X21 * Y31 - Y21 * X31
C
      NORM = SQRT ( PGL(3,1) * PGL(3,1) + PGL(3,2) * PGL(3,2) +
     &              PGL(3,3) * PGL(3,3) )
      PGL(3,1) = PGL(3,1) / NORM
      PGL(3,2) = PGL(3,2) / NORM
      PGL(3,3) = PGL(3,3) / NORM
C
      PGL(2,1) =  PGL(3,2) * PGL(1,3) - PGL(3,3) * PGL(1,2)
      PGL(2,2) =  PGL(3,3) * PGL(1,1) - PGL(3,1) * PGL(1,3)
      PGL(2,3) =  PGL(3,1) * PGL(1,2) - PGL(3,2) * PGL(1,1)
C
      END
