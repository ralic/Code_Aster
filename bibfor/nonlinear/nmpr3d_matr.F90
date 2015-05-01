subroutine nmpr3d_matr(nno, npg, poidsg, vff, dff, &
                       geom, p, matc)
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/r8inir.h"
#include "asterfort/subaco.h"
#include "asterfort/subacv.h"
#include "asterfort/sumetr.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(in) :: nno
    integer, intent(in) :: npg
    real(kind=8), intent(in) :: poidsg(npg)
    real(kind=8), intent(in) :: vff(nno, npg)
    real(kind=8), intent(in) :: dff(2, nno, npg)
    real(kind=8), intent(in) :: geom(3, nno)
    real(kind=8), intent(in) :: p(npg)
    real(kind=8), intent(out) :: matc(3, nno, 3, nno)
!
! --------------------------------------------------------------------------------------------------
!
! Loads computation
!
! Pressure for faces of 3D elements - Tangent matrix
!
! --------------------------------------------------------------------------------------------------
!
!
! In  nno       : number of nodes
! In  nng       : number of Gauss points
! In  poidsg    : weight of Gauss points
! In  vff       : shape functions at Gauss points
! In  dff       : derivative of shape functions at Gauss point point
! In  geom      : coordinates of nodes
! In  p         : pressure at Gauss points
! Out matc      : tangent matrix (following force)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: kpg, n, i, m, j
    real(kind=8) :: cova(3, 3), metr(2, 2), jac, cnva(3, 2)
    real(kind=8) :: t1, t2, t3, t, acv(2, 2)
!
! --------------------------------------------------------------------------------------------------
!
! - Initializations
!
    call r8inir(nno*nno*9, 0.d0, matc, 1)
!
! - Loop on Gauss points
!
    do kpg = 1, npg
!
! ----- Covariant basis
!
        call subaco(nno, dff(1, 1, kpg), geom, cova)
!
! ----- Metric tensor
!
        call sumetr(cova, metr, jac)
!
! ----- Contra-variant basis
!
        call subacv(cova, metr, jac, cnva, acv)
!
! ----- Tangent matrix
!
        do m = 1, nno
            do j = 1, 3
                do n = 1, nno
                    do i = 1, 3
                        t1 = (dff(1, m, kpg)*cnva(j,1) + &
                              dff(2, m, kpg)*cnva(j,2)) * vff(n,kpg)*cova(i,3)
                        t2 = dff(1,m,kpg)*cova(j,3) * vff(n,kpg)* cnva(i,1)
                        t3 = dff(2,m,kpg)*cova(j,3) * vff(n,kpg)* cnva(i,2)
                        t = poidsg(kpg) * p(kpg) * jac * (t1 - t2 - t3)
                        matc(i,n,j,m) = matc(i,n,j,m) + t
                    enddo
                enddo
            enddo
        enddo
     enddo
!
end subroutine
