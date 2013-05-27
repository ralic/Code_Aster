function dis2no(geom, ino1, ino2)
    implicit none
    real(kind=8) :: dis2no
    include 'asterfort/padist.h'
    integer :: ino1, ino2
    real(kind=8) :: geom(*)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT : CALCULER LA DISTANCE ENTRE 2 NOEUDS
!           A PARTIR DES NUMEROS DES NOEUDS
!
! IN   GEOM   : VALEURS DES COORDONNEES DES NOEUDS DU MAILLAGE
! IN   INO1   : INDICE DU PREMIER NOEUD
! IN   INO2   : INDICE DU DEUXIEME NOEUDS
! OUT  DIS2NO : DISTANCE ENTRE LES 2 NOEUDS
!
!---------------------------------------------------------------------
!
    real(kind=8) :: a(3), b(3)
    integer :: i
!
!---------------------------------------------------------------------
!
    do 10 i = 1, 3
        a(i) = geom(3*(ino1-1)+i)
        b(i) = geom(3*(ino2-1)+i)
10  end do
    dis2no = padist(3,a,b)
!
end function
