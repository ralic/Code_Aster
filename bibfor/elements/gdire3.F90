subroutine gdire3(coord, a, b, c, m)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
! ----------------------------------------------------------------
!
! FONCTION REALISEE:
!
!         CALCUL DES COMPOSANTES DU VECTEUR UNITAIRE APPARTENANT
!         AU PLAN TANGENT A L'ELEMENT FORME DE 3 NOEUDS SOMMETS
!         ET QUI EST NORMAL A L'ARETE FORMEE PAR LES NOEUDS 1 ET 2
!-----------------------------------------------------------------
! ENTREE:
!         COORD: COORDONNEES DES 3 NOEUDS
!         M    : 1 SI TRIA
!                2 SI QUAD
!
! SORTIE:
!         A,B,C: COMPOSANTES DU VECTEUR NORME
!
!-----------------------------------------------------------------
!
    real(kind=8) :: coord(3, *), a, b, c, x1, x2, x3, y1, y2, y3, z1, z2, z3
    real(kind=8) :: eps
    real(kind=8) :: x12, y12, z12, x13, y13, z13, norm1, norm2, pscal, prod
!
    integer :: m
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    eps = 1.d-06
    x1 = coord(1,1)
    y1 = coord(2,1)
    z1 = coord(3,1)
    x2 = coord(1,2)
    y2 = coord(2,2)
    z2 = coord(3,2)
    if (m .eq. 1) then
        x3 = coord(1,3)
        y3 = coord(2,3)
        z3 = coord(3,3)
    else
        x3 = coord(1,4)
        y3 = coord(2,4)
        z3 = coord(3,4)
    endif
!
    x12 = x2 - x1
    y12 = y2 - y1
    z12 = z2 - z1
    x13 = x3 - x1
    y13 = y3 - y1
    z13 = z3 - z1
    norm2 = x12*x12 + y12*y12 + z12*z12
    pscal = x12*x13 + y12*y13 + z12*z13
    a = x13 - pscal*x12/norm2
    b = y13 - pscal*y12/norm2
    c = z13 - pscal*z12/norm2
    norm1 = sqrt(a*a + b*b + c*c)
    a = a/norm1
    b = b/norm1
    c = c/norm1
!
! ORIENTATION DU VECTEUR OBTENU
!
    prod = x13*a + y13*b + z13*c
    if (prod .ge. eps) then
        a = -a
        b = -b
        c = -c
    endif
!
end subroutine
