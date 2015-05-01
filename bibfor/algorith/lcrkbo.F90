subroutine lcrkbo(a, b, l0, l1, etamin,&
                  etamax, vide, nsol, sol, sgn)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterf_types.h"
#include "asterfort/lcvpbo.h"
#include "asterfort/utmess.h"
    real(kind=8), intent(in) :: a, b, l0, l1, etamin, etamax
    aster_logical,intent(out) :: vide
    integer, intent(out) :: nsol, sgn(2)
    real(kind=8), intent(out) :: sol(2)
!
! --------------------------------------------------------------------------------------------------
!  SOLUTION Q(ETA) := (POS(A*ETA+B))**2 + L0 + ETA*L1 = 0
! --------------------------------------------------------------------------------------------------
!  IN  A,B    COMPOSANTES DU TERME QUADRATIQUE
!  IN  L0,L1  COMPOSANTES DU TERME AFFINE
!  IN  ETAMIN BORNE MIN
!  IN  ETAMAX BORNE MAX
!  OUT VIDE   .TRUE. SI Q TJRS POSITIF DANS L'INTERVALLE
!  OUT NSOL   NOMBRE DE SOLUTIONS (0, 1 OU 2)
!  OUT SOL    VALEURS DES SOLUTIONS
!  OUT SGN    SIGNE DE DQ/DETA EN CHAQUE SOLUTION
! --------------------------------------------------------------------------------------------------
    real(kind=8), parameter :: zero=0.d0
! --------------------------------------------------------------------------------------------------
    aster_logical :: vide1, vide2
    integer :: i, nsol1, nsol2, sgn1(2), sgn2(2), ptr
    real(kind=8) :: smin, smax, etas, sol1(2), sol2(2)
! --------------------------------------------------------------------------------------------------
!
!
! INITIALISATION
    smin = a*etamin+b
    smax = a*etamax+b
!
!
!  TERME QUADRATIQUE NUL PARTOUT
    if (smin .le. 0 .and. smax .le. 0) then
        call lcvpbo(zero, zero, l0, l1, etamin,&
                    etamax, vide, nsol, sol, sgn)
        goto 999
    endif
!
!
!  TERME QUADRATIQUE NON NUL PARTOUT : Q = (A*ETA)**2 + M1*ETA + M0
    if (smin .ge. 0 .and. smax .ge. 0) then
        call lcvpbo(a, b, l0, l1, etamin,&
                    etamax, vide, nsol, sol, sgn)
        goto 999
    endif
!
!
!  LE TERME A*ETA+B CHANGE DE SIGNE
    etas = -b/a
!
!  QUADRATIQUE PUIS NUL
    if (a .le. 0) then
        call lcvpbo(a, b, l0, l1, etamin,&
                    etas, vide1, nsol1, sol1, sgn1)
        call lcvpbo(zero, zero, l0, l1, etas,&
                    etamax, vide2, nsol2, sol2, sgn2)
!
!  NUL PUIS QUADRATIQUE
    else
        call lcvpbo(zero, zero, l0, l1, etamin,&
                    etas, vide1, nsol1, sol1, sgn1)
        call lcvpbo(a, b, l0, l1, etas,&
                    etamax, vide2, nsol2, sol2, sgn2)
    endif
!
    vide = vide1 .and. vide2
    nsol = nsol1+nsol2
    if (nsol .gt. 2) call utmess('F', 'PILOTAGE_83')
!
    ptr = 0
    do i = 1, nsol1
        ptr = ptr+1
        sol(ptr)=sol1(i)
        sgn(ptr)=sgn1(i)
    end do
    do i = 1, nsol2
        ptr = ptr+1
        sol(ptr)=sol2(i)
        sgn(ptr)=sgn2(i)
    end do
!
!
999 continue
end subroutine
