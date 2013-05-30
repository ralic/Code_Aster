function padist(ndim, coor1, coor2)
    implicit none
    real(kind=8) :: padist
    integer :: ndim, i
    real(kind=8) :: coor1(*), coor2(*), d, x
!---------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     BUT: CALCULER LA DISTANCE ENTRE 2 NOEUDS
!
! IN   NDIM   : DIMENSION DES COORDONNEES
! IN   COOR1  : COORDONNEES DU NOEUD 1
! IN   COOR1  : COORDONNEES DU NOEUD 2
! OUT  PADIST : DISTANCE ENTRE LES 2 NOEUDS
!---------------------------------------------------------------------
    d = 0.d0
    do 10 i = 1, ndim
        x = coor1(i) - coor2(i)
        d = d + x*x
10  end do
    if (d .ne. 0.d0) d = sqrt( d )
    padist = d
end function
