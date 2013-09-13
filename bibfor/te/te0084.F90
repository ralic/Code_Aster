subroutine te0084(option, nomte)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: option
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elements: AXIS, D_PLAN, C_PLAN
! Option: CHAR_MECA_ROTA_R
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: phenom
    integer :: icodre(1)
    real(kind=8) :: dfdx(9), dfdy(9), poids, rx, ry
    integer :: nno, kp, k, npg1, i, jgano, ndim, nnos
    integer :: ipoids, ivf, idfde
    real(kind=8) :: r8b, rho
    integer :: j_geom, j_rota, j_vect, j_mate
    real(kind=8) :: rota_speed, rota_axis(3), rota_cent(3)
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(option.eq.'CHAR_MECA_ROTA_R')
!
! - Finite element parameters
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
! - IN fields
!
    call jevech('PGEOMER', 'L', j_geom)
    call jevech('PMATERC', 'L', j_mate)
    call jevech('PROTATR', 'L', j_rota)
    rota_speed = zr(j_rota-1+1)
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
! - Checking
!
    if (nomte(3:4) .eq. 'DP' .or. nomte(3:4) .eq. 'CP') then
! AXE=direction Oz
        if (abs(rota_axis(3)) .le. r8miem()) then
            call utmess('F', 'CHARGES2_67')
        endif
        if (abs(rota_axis(1)) .gt. r8miem() .or. abs(rota_axis(2)) .gt. r8miem()) then
            call utmess('F', 'CHARGES2_67')
        endif
    else if (nomte(3:4).eq.'AX') then
! AXE=Oy et CENTRE=ORIGINE
        if (abs(rota_axis(1)) .gt. r8miem() .or. abs(rota_axis(3)) .gt. r8miem()) then
            call utmess('F', 'CHARGES2_65')
        endif
        if (abs(rota_axis(2)) .le. r8miem()) then
            call utmess('F', 'CHARGES2_65')
        endif
        if (abs(rota_cent(1)) .gt. r8miem() .or. abs(rota_cent(2)) .gt. r8miem() .or.&
            abs(rota_cent(3)) .gt. r8miem()) then
            call utmess('F', 'CHARGES2_66')
        endif
    endif
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
    do kp = 1, npg1
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(j_geom),&
                    dfdx, dfdy, poids)
        poids = poids * rho * rota_speed**2
        rx= 0.d0
        ry= 0.d0
        do i = 1, nno
            rx= rx+ zr(j_geom+2*i-2)*zr(ivf+k+i-1)
            ry= ry+ zr(j_geom+2*i-1)*zr(ivf+k+i-1)
        end do
        if (lteatt(' ','AXIS','OUI')) then
            poids = poids*rx
            do i = 1, nno
                k=(kp-1)*nno
                zr(j_vect+2*i-2) = zr(j_vect+2*i-2) + poids*rota_axis(2)**2*rx*zr(ivf+k+i-1)
            end do
        else
            rx = rx - rota_cent(1)
            ry = ry - rota_cent(2)
            do i = 1, nno
                k=(kp-1)*nno
                zr(j_vect+2*i-2) = zr(j_vect+2*i-2) + poids*rota_axis(3)**2*rx*zr(ivf+k+i-1)
                zr(j_vect+2*i-1) = zr(j_vect+2*i-1) + poids*rota_axis(3)**2*ry*zr(ivf+k+i-1)
            end do
        endif
    end do
end subroutine
