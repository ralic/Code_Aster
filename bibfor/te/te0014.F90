subroutine te0014(option, nomte)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
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
! Option: CHAR_MECA_ROTA_R
!
! --------------------------------------------------------------------------------------------------
!
    integer :: icodre(1)
    character(len=16) :: phenom
    real(kind=8) :: amm(81, 81), ft(81), x(27), y(27), z(27)
    real(kind=8) :: xi, xij
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids
    real(kind=8) :: rho, om1, om2, om3, omm, omo, rri
    integer :: ipoids, ivf, idfde
    integer :: jgano, ndl, nno, kp, npg, ii, jj, i, j
    integer :: ndim,  l, ic
    integer :: iret, nnos
    real(kind=8) :: r8b
    integer :: j_geom, j_rota, j_vect, j_mate, j_deplm, j_deplp
    real(kind=8) :: rota_speed, rota_axis(3), rota_cent(3)
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(option.eq.'CHAR_MECA_ROTA_R')
!
! - Finite element parameters
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    ndl = 3*nno
    do i = 1, ndl
        do j = 1, ndl
            amm(i,j) = 0.d0
        end do
    end do
!
! - IN fields
!
    call jevech('PGEOMER', 'L', j_geom)
    call jevech('PMATERC', 'L', j_mate)
    call jevech('PROTATR', 'L', j_rota)
    call tecach('ONN', 'PDEPLMR', 'L', 1, j_deplm,&
                iret)
    call tecach('ONN', 'PDEPLPR', 'L', 1, j_deplp,&
                iret)
    rota_speed   = zr(j_rota-1+1)
    rota_axis(1) = zr(j_rota-1+2)
    rota_axis(2) = zr(j_rota-1+3)
    rota_axis(3) = zr(j_rota-1+4)
    rota_cent(1) = zr(j_rota-1+5)
    rota_cent(2) = zr(j_rota-1+6)
    rota_cent(3) = zr(j_rota-1+7)
!
! - OUT fields
!
    call jevech('PVECTUR', 'E', j_vect)
!
! - Material
!
    call rccoma(zi(j_mate), 'ELAS', 1, phenom, icodre(1))
    call rcvalb('FPG1', 1, 1, '+', zi(j_mate),&
                ' ', phenom, 0, ' ', r8b,&
                1, 'RHO', rho, icodre(1), 1)
!
! - Computation
!
    omm = rota_speed*rota_speed
    om1 = rota_speed*rota_axis(1)
    om2 = rota_speed*rota_axis(2)
    om3 = rota_speed*rota_axis(3)
    if (j_deplm .eq. 0 .or. j_deplp .eq. 0) then
        do i = 1, nno
            x(i) = zr(j_geom+3*i-3) - rota_cent(1)
            y(i) = zr(j_geom+3*i-2) - rota_cent(2)
            z(i) = zr(j_geom+3*i-1) - rota_cent(3)
        enddo
    else
        do i = 1, nno
            x(i) = zr(j_geom+3*i-3) + zr(j_deplm+3*i-3) + zr(j_deplp+3*i- 3) - rota_cent(1)
            y(i) = zr(j_geom+3*i-2) + zr(j_deplm+3*i-2) + zr(j_deplp+3*i- 2) - rota_cent(2)
            z(i) = zr(j_geom+3*i-1) + zr(j_deplm+3*i-1) + zr(j_deplp+3*i- 1) - rota_cent(3)
        enddo
    endif
    do i = 1, nno
        omo = om1*x(i) + om2*y(i) + om3*z(i)
        ft(3*i-2) = omm*x(i) - omo*om1
        ft(3*i-1) = omm*y(i) - omo*om2
        ft(3*i) = omm*z(i) - omo*om3
    enddo
!
! - Loop on point Gauss
!
    do kp = 1, npg
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(j_geom),&
                    dfdx, dfdy, dfdz, poids)
        do i = 1, nno
            xi = rho*poids*zr(ivf+l+i-1)
            ii = 3* (i-1)
            do j = 1, nno
                xij = xi*zr(ivf+l+j-1)
                jj = 3* (j-1)
                do ic = 1, 3
                    amm(ii+ic,jj+ic) = amm(ii+ic,jj+ic) + xij
                enddo
            enddo
        enddo
    end do
!
    do i = 1, ndl
        rri = 0.d0
        do j = 1, ndl
            rri = rri + amm(i,j)*ft(j)
        enddo
        amm(i,i) = rri
    end do
!
    do i = 1, ndl
        zr(j_vect+i-1) = amm(i,i)
    end do
!
end subroutine
