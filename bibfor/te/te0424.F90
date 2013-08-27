subroutine te0424(option, nomte)
!
    implicit      none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/jevecd.h"
#include "asterfort/jevech.h"
#include "asterfort/nmpr3d_vect.h"
#include "asterfort/nmpr3d_matr.h"
#include "blas/dcopy.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! Option: RIGI_MECA_PRSU_R 
!         CHAR_MECA_PRSU_R
!         RIGI_MECA_EFSU_R 
!         CHAR_MECA_EFSU_R
!
! --------------------------------------------------------------------------------------------------
!
    integer :: mxnoeu, mxnpg, mxvect, mxmatr
    parameter     (mxnoeu=9, mxnpg=27, mxvect=3*9, mxmatr=3*9*3*9)
!
    integer :: ndim, nno, npg, nnos, nddl
    integer :: iddl, ino, ipg
    integer :: jpoids, jvf, jdf, jgano
    integer :: kdec, i, j, k
    integer :: j_depm, j_depp, j_geom, j_pres, j_vect, j_matr, j_effe
    real(kind=8) :: pres, pres_point(mxnpg)
    real(kind=8) :: vect(mxvect), matr(mxmatr), coef_mult
!
! --------------------------------------------------------------------------------------------------
!
    if ((option.ne.'CHAR_MECA_PRSU_R').and.(option.ne.'RIGI_MECA_PRSU_R').and.&
        (option.ne.'CHAR_MECA_EFSU_R').and.(option.ne.'RIGI_MECA_EFSU_R')) then
        ASSERT(.false.)
    endif
!
! - Finite element parameters
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, jpoids, jvf, jdf, jgano)
    nddl = 3*nno
    ASSERT(nno .le.mxnoeu)
    ASSERT(npg .le.mxnpg)
!
! - IN fields
!
    call jevech('PGEOMER', 'L', j_geom)
    call jevech('PDEPLMR', 'L', j_depm)
    call jevech('PDEPLPR', 'L', j_depp)
!
! - New geometry
!
    do iddl = 1, nddl
        zr(j_geom+iddl-1) = zr(j_geom+iddl-1) + zr(j_depm+iddl-1) + zr(j_depp+iddl-1)
    end do
!
! - For pressure, no node affected -> 0
!
    if (option.eq.'CHAR_MECA_PRSU_R' .or. option.eq.'RIGI_MECA_PRSU_R') then
        call jevecd('PPRESSR', j_pres, 0.d0)
    elseif (option.eq.'CHAR_MECA_EFSU_R' .or. option.eq.'RIGI_MECA_EFSU_R') then
        call jevecd('PPREFFR', j_pres, 0.d0)
    endif
!
! - Multiplicative ratio for pressure (EFFE_FOND)
!
    coef_mult = 1.d0
    if (option.eq.'CHAR_MECA_EFSU_R'.or.option.eq.'RIGI_MECA_EFSU_R') then
        call jevech('PEFOND', 'L', j_effe)
        coef_mult = zr(j_effe-1+1)    
    endif
!
! - Evaluation of pressure at Gauss points (from nodes)
!
    do ipg = 1, npg
        kdec = (ipg-1) * nno
        pres = 0.d0
        do ino = 1, nno
            pres = pres + zr(j_pres+ino-1) * zr(jvf+kdec+ino-1)
        end do
        pres_point(ipg) = coef_mult * pres
    end do
!
! - Second member
!
    if (option(1:9) .eq. 'CHAR_MECA') then
        call nmpr3d_vect(nno, npg, zr(jpoids), zr(jvf), zr(jdf),&
                         zr(j_geom), pres_point, vect)
        call jevech('PVECTUR', 'E', j_vect)
        call dcopy(nddl, vect, 1, zr(j_vect), 1)
!
! - Tangent matrix
!
    else if (option(1:9).eq.'RIGI_MECA') then
        call nmpr3d_matr(nno, npg, zr(jpoids), zr(jvf), zr(jdf),&
                         zr(j_geom), pres_point, matr)
        call jevech('PMATUNS', 'E', j_matr)
        k = 0
        do i = 1, nddl
            do j = 1, nddl
                k = k + 1
                zr(j_matr-1+k) = matr((j-1)*nddl+i)
            end do
        end do
        ASSERT(k.eq.nddl*nddl)
    else
        ASSERT(.false.)
    endif
!
end subroutine
