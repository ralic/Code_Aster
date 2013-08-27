subroutine te0018(option, nomte)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/jevecd.h"
#include "asterfort/jevech.h"
#include "asterfort/nmpr3d_vect.h"
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
! Elements: 3D
! Option: CHAR_MECA_PRES_R
!         CHAR_MECA_EFON_R
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ndim, nno, npg, nnos, jgano, kpg, kdec, n
    integer :: ipoids, ivf, idf
    integer :: j_geom, j_pres, j_vect, j_effe
    real(kind=8) :: pres, pres_point(27), coef_mult
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(option.eq.'CHAR_MECA_PRES_R'.or.option.eq.'CHAR_MECA_EFON_R')
!
! - Finite element parameters
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idf, jgano)
!
! - IN fields
!
    call jevech('PGEOMER', 'L', j_geom)
!
! - OUT fields
!
    call jevech('PVECTUR', 'E', j_vect)
!
! - For pressure, no node affected -> 0
!
    if (option.eq.'CHAR_MECA_PRES_R') then
        call jevecd('PPRESSR', j_pres, 0.d0)
    elseif (option.eq.'CHAR_MECA_EFON_R') then
        call jevecd('PPREFFR', j_pres, 0.d0)
    endif
!
! - Multiplicative ratio for pressure (EFFE_FOND)
!
    coef_mult = 1.d0
    if (option.eq.'CHAR_MECA_EFON_R') then
        call jevech('PEFOND', 'L', j_effe)
        coef_mult = zr(j_effe-1+1)    
    endif
!
! - Evaluation of pressure at Gauss points (from nodes)
!
    do kpg = 0, npg-1
        kdec = kpg*nno
        pres = 0.d0
        do n = 0, nno-1
            pres = pres + zr(j_pres+n) * zr(ivf+kdec+n)
        end do
        pres_point(kpg+1) = coef_mult * pres
    end do
!
! - Second member
!
    call nmpr3d_vect(nno, npg, zr(ipoids), zr(ivf), zr(idf), &
                     zr(j_geom), pres_point, zr(j_vect))
!
end subroutine
