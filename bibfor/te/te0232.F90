subroutine te0232(option, nomte)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: option
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elements: COQUE_1D
! Option: CHAR_MECA_ROTA_R
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: elrefe, fami, poum
    integer :: icodre(1), kpg, spt
    real(kind=8) :: zero, dfdx(3), nx, ny, poids, cour, rx, ry
    integer :: nno, kp, k, npg, i
    integer :: ipoids, ivf, idfdk
    integer :: jgano, ndim, nnos
    real(kind=8) :: rho(1)
    integer :: j_geom, j_rota, j_vect, j_mate, j_caco
    real(kind=8) :: rota_speed, rota_axis(3), rota_cent(3)
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(option.eq.'CHAR_MECA_ROTA_R')
!
! - Finite element parameters
!
    call elref1(elrefe)
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdk, jgano)
!
! - IN fields
!
    call jevech('PGEOMER', 'L', j_geom)
    call jevech('PMATERC', 'L', j_mate)
    call jevech('PROTATR', 'L', j_rota)
    call jevech('PCACOQU', 'L', j_caco)
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
    if (nomte .eq. 'METDSE3' .or. nomte .eq. 'METCSE3') then
! AXE=direction Oz
        if (abs(rota_axis(3)) .le. r8miem()) then
            call utmess('F', 'CHARGES2_67')
        endif
        if (abs(rota_axis(1)) .gt. r8miem() .or. abs(rota_axis(2)) .gt. r8miem()) then
            call utmess('F', 'CHARGES2_67')
        endif
    else if (nomte.eq.'MECXSE3') then
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
    zero = 0.d0
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(j_mate),&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre, 1)
!
! - Computation
!
    do kp = 1, npg
        k = (kp-1)*nno
        call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(j_geom), dfdx,&
                    cour, poids, nx, ny)
        poids = poids*rho(1)*rota_speed**2*zr(j_caco)
        rx = zero
        ry = zero
        do i = 1, nno
            rx = rx + zr(j_geom+2*i-2)*zr(ivf+k+i-1)
            ry = ry + zr(j_geom+2*i-1)*zr(ivf+k+i-1)
        end do
        if (nomte .eq. 'MECXSE3') then
            poids = poids*rx
            do i = 1, nno
                zr(j_vect+3*i-3) = zr(j_vect+3*i-3) + poids*rota_axis(2)**2*rx*zr(ivf+k+i-1)
            end do
        else
            rx = rx - rota_cent(1)
            ry = ry - rota_cent(2)
            do i = 1, nno
                zr(j_vect+3*i-3) = zr(j_vect+3*i-3) + poids*rota_axis(3)**2*rx*zr(ivf+k+i-1)
                zr(j_vect+3*i-2) = zr(j_vect+3*i-2) + poids*rota_axis(3)**2*ry*zr(ivf+k+i-1)
            end do
        endif
    end do
end subroutine
