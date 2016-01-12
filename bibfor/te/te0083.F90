subroutine te0083(option, nomte)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/bsigmc.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/metau1.h"
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/sigtmc.h"
#include "asterfort/tecach.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: option
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elements: 2D
! Option: CHAR_MECA_TEMP_R
!
! --------------------------------------------------------------------------------------------------
!
    character(len=4) :: fami
    real(kind=8) :: bsigma(81), sigth(162), repere(7), time, nharm, bary(3)
    integer :: idim
    integer :: i, idfde, igeom, imate, ipoids, iret, itemps
    integer :: ivectu, ivf, nbsig, ndim, nno
    integer :: npg
    real(kind=8) :: zero
    aster_logical :: l_meta
!
! --------------------------------------------------------------------------------------------------
!
    zero = 0.d0
    time = zero
    nharm = zero
    fami = 'RIGI'
    sigth(:) = zero
    bsigma(:) = zero
    bary(:) = 0.d0
!
! - Compute CHAR_MECA_TEMP_R for metallurgy
!
    call metau1(l_meta)
    if (l_meta) then
        goto 40
    endif
!
! - Finite element informations
!
    call elrefe_info(fami=fami, ndim=ndim, nno=nno, npg=npg, jpoids=ipoids,&
                     jvf=ivf, jdfde=idfde)
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
! - Orthotropic parameters
!
    do i = 1, nno
        do idim = 1, ndim
            bary(idim) = bary(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
        end do
    end do
    call ortrep(ndim, bary, repere)
!
! - Get time
!
    call tecach('ONO', 'PTEMPSR', 'L', iret, iad=itemps)
    if (itemps .ne. 0) then
        time = zr(itemps)
    endif
!
! - Compute thermal stresses {SIGTH}
!
    call sigtmc(fami, nno, ndim, nbsig, npg,&
                zr(ivf), zr(igeom), time, zi(imate), repere,&
                option, sigth)
!
! - Compute CHAR_MECA_TEMP_R: [B]Tx{SIGTH}
!
    call bsigmc(nno, ndim, nbsig, npg, ipoids,&
                ivf, idfde, zr(igeom), nharm, sigth,&
                bsigma)
!
! - Set output vector
!
    call jevech('PVECTUR', 'E', ivectu)
    do i = 1, ndim*nno
        zr(ivectu+i-1) = bsigma(i)
    end do
!
 40 continue
end subroutine
