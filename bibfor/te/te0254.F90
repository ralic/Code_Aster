subroutine te0254(option, nomte)
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W0104
!
    character(len=16), intent(in) :: option
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elements: AXIS_FLUIDE/2D_FLUIDE
! Option: MASS_MECA
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbres=2
    character(len=16), parameter :: nomres(nbres) = (/'RHO   ', 'CELE_R'/)
    real(kind=8) :: valres(nbres)
    integer :: icodre(nbres)
    real(kind=8) :: r
    character(len=8) :: fami, poum
    integer :: kpg, spt
    real(kind=8) :: a(2, 2, 9, 9)
    real(kind=8) :: dfdx(9), dfdy(9), poids, rho, celer
    integer :: ipoids, ivf, idfde, jv_geom, jv_mate
    integer :: nno, kp, npg, ik, ijkl, i, j, k, l
    integer :: jv_matr
    aster_logical :: l_axis
!
! --------------------------------------------------------------------------------------------------
!
    fami       = 'FPG1'
    kpg        = 1
    spt        = 1
    poum       = '+'
    a(:,:,:,:) = 0.d0
    l_axis     = lteatt('AXIS', 'OUI')
!
! - Get fields
!
    call jevech('PGEOMER', 'L', jv_geom)
    call jevech('PMATERC', 'L', jv_mate)
    call jevech('PMATUUR', 'E', jv_matr)
!
! - Get element parameters
!
    call elrefe_info(fami='RIGI', nno=nno, npg=npg, jpoids=ipoids, jvf=ivf, jdfde=idfde)
!
! - Get material properties
!
    call rcvalb(fami , kpg     , spt   , poum  , zi(jv_mate),&
                ' '  , 'FLUIDE', 0     , ' '   , [0.d0],&
                nbres, nomres  , valres, icodre, 1)
    rho   = valres(1)
    celer = valres(2)
!
! - Loop on Gauss points
!
    do kp = 1, npg
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(jv_geom),&
                    poids, dfdx, dfdy)
        if (l_axis) then
            r = 0.d0
            do i = 1, nno
                r = r + zr(jv_geom+2*(i-1))*zr(ivf+k+i-1)
            end do
            poids = poids*r
        endif
        do i = 1, nno
            do j = 1, i
!
! ----- Compute -RHO*(GRAD(PHI)**2)
!
                a(2,2,i,j) = a(2,2,i,j) - poids * (dfdx(i)*dfdx(j) + dfdy(i)*dfdy(j))*rho
!
! ----- Compute (P*PHI)/(CEL**2)
!
                if (celer .eq. 0.d0) then
                    a(1,2,i,j) = 0.d0
                else
                    a(1,2,i,j) = a(1,2,i,j) + poids * zr(ivf+k+i-1) * zr( ivf+k+j-1)/ celer / celer
                endif
            end do
        end do
    end do
!
! - Matrix is symmetric
!
    do i = 1, nno
        do j = 1, i
            a(2,1,i,j) = a(1,2,i,j)
        end do
    end do
!
! - Save matrix
!
    do k = 1, 2
        do l = 1, 2
            do i = 1, nno
                ik = ((2*i+k-3) * (2*i+k-2)) / 2
                do j = 1, i
                    ijkl = ik + 2 * (j-1) + l
                    zr(jv_matr+ijkl-1) = a(k,l,i,j)
                end do
            end do
        end do
    end do
!
end subroutine
