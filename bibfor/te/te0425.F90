subroutine te0425(option, nomte)
!
    implicit      none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/nmpr3d_vect.h"
#include "asterfort/nmpr3d_matr.h"
#include "blas/dcopy.h"
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
! Option: RIGI_MECA_PRSU_F
!         CHAR_MECA_PRSU_F
!         RIGI_MECA_EFSU_F
!         CHAR_MECA_EFSU_F
!
! --------------------------------------------------------------------------------------------------
!
    integer :: mxnoeu, mxnpg, mxvect, mxmatr
    parameter     (mxnoeu=9,mxnpg=27,mxvect=3*9,mxmatr=3*9*3*9)
    integer :: mxpara
    parameter     (mxpara=7)
!
    character(len=8) :: nompar(mxpara)
    real(kind=8) :: valpar(mxpara)
    integer :: ier
    real(kind=8) :: x, y, z, xf, yf, zf
!
    integer :: ndim, nno, npg, nnos, nddl
    integer :: iddl, ino, ipg
    integer :: jpoids, jvf, jdf, jgano
    integer :: j_depm, j_depp, j_geom, j_pres, j_time, j_vect, j_matr, j_effe
    integer :: kdec, i, j, k
    real(kind=8) :: pres, pres_point(mxnpg)
    real(kind=8) :: vect(mxvect), matr(mxmatr), coef_mult, geom_reac(mxvect)
!
! --------------------------------------------------------------------------------------------------
!
    if ((option.ne.'CHAR_MECA_PRSU_F').and.(option.ne.'RIGI_MECA_PRSU_F').and.&
        (option.ne.'CHAR_MECA_EFSU_F').and.(option.ne.'RIGI_MECA_EFSU_F')) then
        ASSERT(.false.)
    endif
!
! - Finite element parameters
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=jpoids,jvf=jvf,jdfde=jdf,jgano=jgano)
    nddl = 3*nno
    ASSERT(nno .le.mxnoeu)
    ASSERT(npg .le.mxnpg)
!
! - IN fields
!
    call jevech('PGEOMER', 'L', j_geom)
    call jevech('PDEPLMR', 'L', j_depm)
    call jevech('PDEPLPR', 'L', j_depp)
    call jevech('PTEMPSR', 'L', j_time)
!
! - Multiplicative ratio for pressure (EFFE_FOND)
!
    coef_mult = 1.d0
    if (option.eq.'CHAR_MECA_EFSU_F'.or.option.eq.'RIGI_MECA_EFSU_F') then
        call jevech('PEFOND', 'L', j_effe)
        coef_mult = zr(j_effe-1+1)
    endif
!
! - Pressure
!
    if (option.eq.'CHAR_MECA_PRSU_F' .or. option.eq.'RIGI_MECA_PRSU_F') then
        call jevech('PPRESSF', 'L', j_pres)
    elseif (option.eq.'CHAR_MECA_EFSU_F' .or. option.eq.'RIGI_MECA_EFSU_F') then
        call jevech('PPREFFF', 'L', j_pres)
    endif
!
! - Parameters of function
!
    valpar(4) = zr(j_time)
    nompar(4) = 'INST'
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
    nompar(5) = 'XF'
    nompar(6) = 'YF'
    nompar(7) = 'ZF'
!
! - New geometry
!
    do iddl = 1, nddl
        geom_reac(iddl) = zr(j_geom+iddl-1) + zr(j_depm+iddl-1) + zr(j_depp+iddl-1)
    end do
!
! - Evaluation of pressure (function) at Gauss points (from nodes)
!
    do ipg = 1, npg
        kdec = (ipg-1) * nno
        x = 0.d0
        y = 0.d0
        z = 0.d0
        xf = 0.d0
        yf = 0.d0
        zf = 0.d0
        do ino = 1, nno
            x = x + zr(j_geom+3*(ino-1)+1-1) * zr(jvf+kdec+ino-1)
            y = y + zr(j_geom+3*(ino-1)+2-1) * zr(jvf+kdec+ino-1)
            z = z + zr(j_geom+3*(ino-1)+3-1) * zr(jvf+kdec+ino-1)
            xf = xf + geom_reac(3*(ino-1)+1) * zr(jvf+kdec+ino-1)
            yf = yf + geom_reac(3*(ino-1)+2) * zr(jvf+kdec+ino-1)
            zf = zf + geom_reac(3*(ino-1)+3) * zr(jvf+kdec+ino-1)
        end do
        valpar(1) = x
        valpar(2) = y
        valpar(3) = z
        valpar(5) = xf
        valpar(6) = yf
        valpar(7) = zf
        call fointe('FM', zk8(j_pres), mxpara, nompar, valpar,&
                    pres, ier)
        pres_point(ipg) = coef_mult * pres
     end do
!
! - Second member
!
    if (option(1:9) .eq. 'CHAR_MECA') then
        call nmpr3d_vect(nno, npg, zr(jpoids), zr(jvf), zr(jdf),&
                         geom_reac, pres_point, vect)
        call jevech('PVECTUR', 'E', j_vect)
        call dcopy(nddl, vect, 1, zr(j_vect), 1)
!
! - Tangent matrix
!
    else if (option(1:9).eq.'RIGI_MECA') then
        call nmpr3d_matr(nno, npg, zr(jpoids), zr(jvf), zr(jdf), &
                        geom_reac, pres_point, matr)
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
