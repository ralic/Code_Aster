subroutine nmpr3d_vect(nno, npg, poidsg, vff, dff,&
                       geom, p, vect)
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/r8inir.h"
#include "asterfort/subaco.h"
#include "asterfort/sumetr.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1306
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(in) :: nno
    integer, intent(in) :: npg
    real(kind=8), intent(in) :: poidsg(npg)
    real(kind=8), intent(in) :: vff(nno, npg)
    real(kind=8), intent(in) :: dff(2, nno, npg)
    real(kind=8), intent(in) :: geom(3, nno)
    real(kind=8), intent(in) :: p(npg)
    real(kind=8), intent(out) :: vect(3, nno)
!
! --------------------------------------------------------------------------------------------------
!
! Loads computation
!
! Pressure for faces of 3D elements - Second member
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
! Out vect      : second member
!
! --------------------------------------------------------------------------------------------------
!
    integer :: kpg, n, i
    real(kind=8) :: cova(3, 3), metr(2, 2), jac
!
! --------------------------------------------------------------------------------------------------
!
! - Initializations
!
    call r8inir(nno*3, 0.d0, vect, 1)
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
! ----- Second member
!
         do n = 1, nno
            do i = 1, 3
                vect(i,n) = vect(i,n) - poidsg(kpg)*jac * p(kpg) * cova(i,3)*vff(n,kpg)
            enddo
         enddo
     enddo
!
end subroutine
