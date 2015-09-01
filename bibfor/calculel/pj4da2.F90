subroutine pj4da2(ino2, geom2, i, geom1, tria3,&
                  cobary, d2, surf)
    implicit none
#include "asterf_types.h"
#include "asterc/r8maem.h"
#include "asterfort/pj3da3.h"
#include "asterfort/pj3da4.h"
    real(kind=8) :: cobary(3), geom1(*), geom2(*), d2, surf
    integer :: ino2, i, tria3(*)
! ----------------------------------------------------------------------
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

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!  but :
!    determiner la distance d entre le noeud ino2 et le tria3 i.
!    determiner les coordonnees barycentriques
!    du point de i le plus proche de ino2.

!  in   ino2       i  : numero du noeud de m2 cherche
!  in   geom2(*)   r  : coordonnees des noeuds du maillage m2
!  in   geom1(*)   r  : coordonnees des noeuds du maillage m1
!  in   i          i  : numero du tria3 candidat
!  in   tria3(*)   i  : objet '&&pjxxco.tria3'
!  out  cobary(3)  r  : coordonnees barycentriques de ino2 projete sur i
!  out  d2         r  : carre de la distance entre i et ino2
!  out  surf       r  : surface du tria3 i

! ----------------------------------------------------------------------
    integer :: k
    aster_logical :: ok
    real(kind=8) :: dp, l1, l2, l3, la, lb, lc
    real(kind=8) :: a(3), b(3), c(3), m(3), ab(3), ac(3), v(3)
! DEB ------------------------------------------------------------------

    do 1 k = 1, 3
        m(k)=geom2(3*(ino2-1)+k)
        a(k)=geom1(3*(tria3(1+4*(i-1)+1)-1)+k)
        b(k)=geom1(3*(tria3(1+4*(i-1)+2)-1)+k)
        c(k)=geom1(3*(tria3(1+4*(i-1)+3)-1)+k)
        ab(k)=b(k)-a(k)
        ac(k)=c(k)-a(k)
  1 end do

    d2=r8maem()
    dp=r8maem()


!   1. on cherche le point le plus proche a l'interieur du tria3
!   -------------------------------------------------------------
    call pj3da3(m, a, b, c, ok,&
                l1, l2, l3, dp)
    if ((ok) .and. (dp.lt.d2)) then
        d2=dp
        la=l1
        lb=l2
        lc=l3
    endif


!   2. on boucle sur les 3 arretes du tria3 :
!   -----------------------------------------
    call pj3da4(m, a, b, l1, l2,&
                dp)
    if (dp .lt. d2) then
        d2=dp
        la=l1
        lb=l2
        lc=0.d0
    endif

    call pj3da4(m, b, c, l1, l2,&
                dp)
    if (dp .lt. d2) then
        d2=dp
        lb=l1
        lc=l2
        la=0.d0
    endif

    call pj3da4(m, a, c, l1, l2,&
                dp)
    if (dp .lt. d2) then
        d2=dp
        la=l1
        lc=l2
        lb=0.d0
    endif


!   3. on calcule surf :
!   --------------------
    v(1)=ab(2)*ac(3)-ab(3)*ac(2)
    v(2)=ab(3)*ac(1)-ab(1)*ac(3)
    v(3)=ab(1)*ac(2)-ab(2)*ac(1)
    surf=sqrt(v(1)*v(1)+v(2)*v(2)+v(3)*v(3))
    surf=surf/2.d0


    cobary(1)=la
    cobary(2)=lb
    cobary(3)=lc

end subroutine
