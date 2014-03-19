subroutine te0197(option, nomte)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
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
! Elements: AXIS FOURIER
! Option: CHAR_MECA_ROTA_R
!
! --------------------------------------------------------------------------------------------------
!
    integer :: icodre(1)
    character(len=8) :: fami, poum
    real(kind=8) :: poids, r
    integer :: nno, kp, npg1, i, ndim, jgano, nnos
    integer :: ipoids, ivf, idfde, kpg, spt
    integer :: j_geom, j_rota, j_vect, j_mate
    integer :: k
    real(kind=8) :: rho(1)
    real(kind=8) :: rota_speed, rota_axis(3), rota_cent(3)
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(option.eq.'CHAR_MECA_ROTA_R')
!
! - Finite element parameters
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
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
!
! - Material
!
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
    do kp = 1, npg1
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(j_geom),&
                    poids)
        r = 0.d0
        do i = 1, nno
            r = r + zr(j_geom+2*(i-1))*zr(ivf+k+i-1)
        end do
        poids = poids*rho(1)*r
        r = r - rota_cent(1)
        do i = 1, nno
            k=(kp-1)*nno
            zr(j_vect+3*i-3) = zr(j_vect+3*i-3) + poids * (rota_speed*rota_axis(2))**2 * r * zr(i&
                               &vf+k+i-1)
        end do
    end do
end subroutine
