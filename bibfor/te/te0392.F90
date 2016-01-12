subroutine te0392(option, nomte)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/caatdb.h"
#include "asterfort/cast3d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/dmatmc.h"
#include "asterfort/elraga.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/invjac.h"
#include "asterfort/jevech.h"
#include "asterfort/ortrep.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/nbsigm.h"
#include "asterfort/get_elas_id.h"
#include "asterfort/get_elas_para.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
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
! Elements: 3D_SI
! Option: RIGI_MECA
!
! --------------------------------------------------------------------------------------------------
!
    integer :: idecno, idecpg, idfde2, igau, imate, imatuu, ipoid2
    integer :: nbsig, nno, npg1
    real(kind=8) :: jacgau
    real(kind=8) :: repere(7), xyzgau(3), instan
    integer :: igeom, ipoids, ivf, idfde
!
    aster_logical :: calbn
    integer :: i, ino, j, k, proj, nbpg2, ipg, ispg
    integer :: ndim, nnos, kp, idim
    real(kind=8) :: d(6, 6), s
    real(kind=8) :: poipg2(8), b(6, 81), b0(6, 3, 8)
    real(kind=8) :: jac, invja(3, 3), bi(3, 8), hx(3, 4), bary(3)
    real(kind=8) :: gam(4, 8), coopg2(24), h(8, 4), dh(4, 24)
    real(kind=8) :: bn(6, 3, 8)
    real(kind=8) :: dfdx(8), dfdy(8), dfdz(8)
    real(kind=8) :: nu, nub, nu12
    integer :: elas_id
    data h/ 1.d0, 1.d0, -1.d0,-1.d0,-1.d0,-1.d0, 1.d0, 1.d0,&
     &        1.d0,-1.d0, -1.d0, 1.d0,-1.d0, 1.d0, 1.d0,-1.d0,&
     &        1.d0,-1.d0,  1.d0,-1.d0, 1.d0,-1.d0, 1.d0,-1.d0,&
     &       -1.d0, 1.d0, -1.d0, 1.d0, 1.d0,-1.d0, 1.d0,-1.d0/
!
! --------------------------------------------------------------------------------------------------
!
!
! - Finite element informations
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg1,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde)
!
! - Initializations
!
    instan = 0.d0
    b(:,:) = 0.d0
    bary(:) = 0.d0
!
! - Number of stress components
!
    nbsig = nbsigm()
!
! - Geometry
!
    call jevech('PGEOMER', 'L', igeom)
!
! - Material parameters
!
    call jevech('PMATERC', 'L', imate)
!
! - Get type of elasticity (Isotropic/Orthotropic/Transverse isotropic)
!
    call get_elas_id(zi(imate), elas_id)
!
! - Orthotropic parameters
!
    do i = 1, nno
        do idim = 1, ndim
            bary(idim) = bary(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
        end do
    end do
    call ortrep(ndim, bary, repere)
!
    call jevech('PMATUUR', 'E', imatuu)
    do i = 1, 300
        zr(imatuu-1+i)=0.0d0
    end do
!
! - Compute [Bi] (mean value for derivate of shape functions)
!
    do ipg = 1, npg1
        call dfdm3d(nno, ipg, ipoids, idfde, zr(igeom),&
                    jac, dfdx, dfdy, dfdz)
        do ino = 1, nno
            bi(1,ino) = dfdx(ino)
            bi(2,ino) = dfdy(ino)
            bi(3,ino) = dfdz(ino)
        end do
    end do
!
    do igau = 1, npg1
!
        idecpg = nno* (igau-1) - 1
!
! ----- Coordinates for current Gauss point
!
        xyzgau(:) = 0.d0
        do i = 1, nno
            idecno = 3* (i-1) - 1
            xyzgau(1) = xyzgau(1) + zr(ivf+i+idecpg)*zr(igeom+1+ idecno)
            xyzgau(2) = xyzgau(2) + zr(ivf+i+idecpg)*zr(igeom+2+ idecno)
            xyzgau(3) = xyzgau(3) + zr(ivf+i+idecpg)*zr(igeom+3+ idecno)
        end do
!
! ----- Compute matrix [B]: displacement -> strain (first order)
!
        call dfdm3d(nno, igau, ipoids, idfde, zr(igeom),&
                    jacgau, dfdx, dfdy, dfdz)
!
! ----- Modify matrix [B] for underintegrated elements
!
        do i = 1, 8
            j= 3*(i-1) + 1
            b(1,j) = bi(1,i)
            b(2,j+1) = bi(2,i)
            b(3,j+2) = bi(3,i)
            b(4,j) = bi(2,i)
            b(4,j+1) = bi(1,i)
            b(5,j) = bi(3,i)
            b(5,j+2) = bi(1,i)
            b(6,j+1) = bi(3,i)
            b(6,j+2) = bi(2,i)
        end do
        do i = 1, nno
            do j = 1, 3
                do k = 1, 6
                    b0(k,j,i)=b(k,(i-1)*3+j)
                end do
            end do
        end do
!
! ----- Compute Hooke matrix [D]
!
        call dmatmc('RIGI', zi(imate), instan, '+', igau,&
                    1, repere, xyzgau, nbsig, d)
!
! ----- Compute "center" rigidity matrix [KC]
!
        call caatdb(nno, b0, d, b0, jacgau,&
                    zr(imatuu))
!
    end do
!
! - Gamma ratio
!
    do i = 1, 4
        do k = 1, 3
            hx(k,i) = 0.d0
            do j = 1, nno
                hx(k,i) = hx(k,i) + h(j,i) * zr(igeom-1+3*(j-1)+k)
            end do
        end do
    end do
!
    do i = 1, 4
        do j = 1, nno
            s = 0.d0
            do k = 1, 3
                s = s + hx(k,i) * bi(k,j)
            end do
            gam(i,j) = 0.125d0 * (h(j,i) - s)
        end do
    end do
!
! - Poisson ration for ASQBI
!
    ipg = 1
    ispg = 1
    call get_elas_para('RIGI', zi(imate), '+', ipg, ispg,&
                       elas_id, nu = nu, nu12 = nu12)
    if (elas_id .eq. 1) then
        nub = nu/(1.d0-nu)
    else
        nub = nu12/(1.d0-nu12)
    endif
!
! - Projection type
!           0 AUCUNE
!           1 ADS
!           2 ASBQI
!
    proj = 2
    calbn = .false.
!
! - Finite element informations for underintegrated element
!
    call elraga('HE8', 'FPG8    ', ndim, nbpg2, coopg2,&
                poipg2)
    call elrefe_info(elrefe='HE8', fami='MASS', ndim=ndim, nno=nno, nnos=nnos,&
                     npg=nbpg2, jpoids=ipoid2, jdfde=idfde2)
!
! - Compute corrected stabilization matrix [K_STAB]
!
    do ipg = 1, nbpg2
        kp = 3*(ipg-1)
        call invjac(nno, ipg, ipoid2, idfde2, zr(igeom),&
                    invja, jac)
        do i = 1, 3
            dh(1,kp+i) = coopg2(3*ipg-1) * invja(i,3) + coopg2(3*ipg) * invja(i,2)
        end do
        do i = 1, 3
            dh(2,kp+i) = coopg2(3*ipg-2) * invja(i,3) + coopg2(3*ipg) * invja(i,1)
        end do
        do i = 1, 3
            dh(3,kp+i) = coopg2(3*ipg-2) * invja(i,2) + coopg2(3*ipg- 1) * invja(i,1)
        end do
        do i = 1, 3
            dh(4,kp+i) = coopg2(3*ipg-2) * coopg2(3*ipg-1) * invja(i, 3) + coopg2(3*ipg-1) * coop&
                         &g2(3*ipg) * invja(i,1) + coopg2(3*ipg-2) * coopg2(3*ipg) * invja(i,2)
        end do
        call cast3d(proj, gam, dh, b0, nno,&
                    ipg, nub, nu, d, calbn,&
                    bn, jac, zr( imatuu))
    end do
!
end subroutine
