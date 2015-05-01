subroutine pmfsce(nno, x, y, surf, centre)
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
    integer :: nno
    real(kind=8) :: x(4), y(4), surf, centre(2), deux, trois, quatre, pvect
    parameter (deux=2.d+0,trois=3.d+0,quatre=4.d+0)
!
! --- LA SURFACE D'UN TRIANGLE EST EGALE A LA MOITIE DE LA VALEUR
! --- ABSOLUE DU PRODUIT VECTORIEL DE 2 COTES
    pvect = (x(2)-x(1))* (y(3)-y(1)) - (y(2)-y(1))* (x(3)-x(1))
    surf = abs(pvect/deux)
! --- CAS DU TRIANGLE
    if (nno .eq. 3) then
        centre(1) = (x(1)+x(2)+x(3))/trois
        centre(2) = (y(1)+y(2)+y(3))/trois
    else
! --- SI QUADRILATERE, ON COUPE EN 2 TRIANGLES
        centre(1) = (x(1)+x(2)+x(3)+x(4))/quatre
        centre(2) = (y(1)+y(2)+y(3)+y(4))/quatre
        pvect = (x(4)-x(1))* (y(3)-y(1)) - (y(4)-y(1))* (x(3)-x(1))
        surf = surf + abs(pvect/deux)
    endif
end subroutine
