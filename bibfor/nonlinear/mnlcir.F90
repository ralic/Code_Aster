subroutine mnlcir(xdep, ydep, omega, alpha, eta,&
                  h, hf, nt, xsort)
    implicit none
!
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
! ----------------------------------------------------------------------
#include "jeveux.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "blas/dscal.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mnlfft.h"
#include "asterc/r8depi.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"

    integer :: h, hf, nt
    real(kind=8) :: omega, alpha, eta
    character(len=14) :: xdep, ydep, xsort
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    real(kind=8) :: depi, xt, yt, rk
    integer :: k, j, ix, iy, isor
    real(kind=8), pointer :: fn(:) => null()
    real(kind=8), pointer :: fx(:) => null()
    real(kind=8), pointer :: fy(:) => null()
    real(kind=8), pointer :: r(:) => null()
    real(kind=8), pointer :: t(:) => null()
!
    call jemarq()
!
    AS_ALLOCATE(vr=t, size=nt)
    AS_ALLOCATE(vr=fx, size=nt)
    AS_ALLOCATE(vr=fy, size=nt)
    AS_ALLOCATE(vr=fn, size=nt)
    AS_ALLOCATE(vr=r, size=nt)
!
    call jeveuo(xdep, 'E', ix)
    call jeveuo(ydep, 'E', iy)
    call jeveuo(xsort, 'E', isor)
!
    depi=r8depi()
    t(1)=0.d0
    do 10 k = 2, nt
        t(k)=t(k-1)+(depi/omega)/nt
10  continue
!
    do 20 k = 1, nt
        xt=zr(ix)
        yt=zr(iy)
        do 21 j = 1, h
            xt=xt+zr(ix+j)*dcos(dble(j)*omega*t(k))
            yt=yt+zr(iy+j)*dcos(dble(j)*omega*t(k))
            xt=xt+zr(ix+h+j)*dsin(dble(j)*omega*t(k))
            yt=yt+zr(iy+h+j)*dsin(dble(j)*omega*t(k))
21      continue
        rk=xt**2+yt**2
        r(k)=sqrt(rk)
        fn(k)=((r(k)-1.d0)+sqrt((r(k)-1.d0)**2+&
        4.d0*eta/alpha))/(2.d0/alpha)
        fx(k)=fn(k)*xt/r(k)
        fy(k)=fn(k)*yt/r(k)
20  continue
!
    call dscal(4*(2*hf+1), 0.d0, zr(isor), 1)
    call mnlfft(1, zr(isor), fx, hf, nt,&
                1)
    call mnlfft(1, zr(isor+(2*hf+1)), fy, hf, nt,&
                1)
    call mnlfft(1, zr(isor+2*(2*hf+1)), r, hf, nt,&
                1)
    call mnlfft(1, zr(isor+3*(2*hf+1)), fn, hf, nt,&
                1)
!
    AS_DEALLOCATE(vr=t)
    AS_DEALLOCATE(vr=fx)
    AS_DEALLOCATE(vr=fy)
    AS_DEALLOCATE(vr=fn)
    AS_DEALLOCATE(vr=r)

    call jedema()
!
end subroutine
