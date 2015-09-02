subroutine te0190(option, nomte)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/bmatmc.h"
#include "asterfort/btdbmc.h"
#include "asterfort/dmatmc.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/get_elas_id.h"
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
! aslint: disable=W0104
!
    character(len=16), intent(in) :: option
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elements: 2D Fourier
! Option: RIGI_MECA
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, igau, igeom, iharmo, imate, imatuu, j
    integer :: k, nbinco, nbsig, nh
    character(len=4) :: fami
    real(kind=8) :: b(486), btdb(81, 81), d(36), jacgau
    real(kind=8) :: repere(7), xyzgau(3), instan, nharm, bary(3)
    integer :: ndim, nno, nnos, npg1, ipoids, ivf, idfde,dimmod
    integer :: idim
    integer :: elas_id
!
! --------------------------------------------------------------------------------------------------
!
!
! - Finite element informations
!
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
                      npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde)
!
! - CAREFUL ! Dimension of model is 3, not 2
!
    dimmod = 3
!
! - Initializations
!
    instan    = 0.d0
    nbinco    = dimmod*nno
    nharm     = 0.d0
    btdb(:,:) = 0.d0
    xyzgau(:) = 0.d0
    bary(:)   = 0.d0
!
! - Number of stress components
!
    nbsig     = nbsigm()
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
    call ortrep(zi(imate), ndim, bary, repere)
!
! - Harmonic coefficient
!
    call jevech('PHARMON', 'L', iharmo)
    nh    = zi(iharmo)
    nharm = dble(nh)
!
! - Compute RIGI_MECA
!
    do igau = 1, npg1
!
! ----- Compute matrix [B]: displacement -> strain (first order)
!
        call bmatmc(igau, nbsig, zr(igeom), ipoids, ivf,&
                    idfde, nno, nharm, jacgau, b)
!
! ----- Compute Hooke matrix [D]
!
        call dmatmc(fami, zi(imate), instan, '+',&
                    igau, 1, repere, xyzgau, nbsig,&
                    d)
!
! ----- Compute rigidity matrix [K] = [B]Tx[D]x[B]
!
        call btdbmc(b, d, jacgau, dimmod, nno,&
                    nbsig, elas_id, btdb)
!
    end do
!
! - Set matrix in output field
!
    call jevech('PMATUUR', 'E', imatuu)
    k = 0
    do i = 1, nbinco
        do j = 1, i
            k = k + 1
            zr(imatuu+k-1) = btdb(i,j)
        end do
    end do
!
end subroutine
