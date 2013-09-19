subroutine te0294(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!         CALCUL DES VECTEURS ELEMENTAIRES
!         OPTION : 'SECM_ZZ1'
!
! ......................................................................
!
!
!
!
    integer :: i, k, kp, nno, nnos, npg, ndim, nbcmp
    integer :: ipoids, ivf, idfde, jgano, igeom, isief
    integer :: ivect1, ivect2, ivect3, ivect4, ivect5, ivect6
!
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, r
!
    logical :: laxi
!
! ----------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    if (ndim .eq. 2) then
        nbcmp = 4
    else if (ndim.eq.3) then
        nbcmp = 6
    else
        ASSERT(.false.)
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PSIEF_R', 'L', isief)
    call jevech('PVECTR1', 'E', ivect1)
    call jevech('PVECTR2', 'E', ivect2)
    call jevech('PVECTR3', 'E', ivect3)
    call jevech('PVECTR4', 'E', ivect4)
    if (ndim .eq. 3) then
        call jevech('PVECTR5', 'E', ivect5)
        call jevech('PVECTR6', 'E', ivect6)
    endif
!
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    do 10 kp = 1, npg
        k=(kp-1)*nno
        if (ndim .eq. 2) then
            call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy)
        else
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy, dfdz)
        endif
!
        if (laxi) then
            r = 0.d0
            do 20 i = 1, nno
                r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
20          continue
            poids = poids*r
        endif
!
        do 30 i = 1, nno
            k=(kp-1)*nno
            zr(ivect1+i-1) = zr(ivect1+i-1) + poids * zr(ivf+k+i-1) * zr(isief+nbcmp*(kp-1))
            zr(ivect2+i-1) = zr(ivect2+i-1) + poids * zr(ivf+k+i-1) * zr(isief+nbcmp*(kp-1)+1)
            zr(ivect3+i-1) = zr(ivect3+i-1) + poids * zr(ivf+k+i-1) * zr(isief+nbcmp*(kp-1)+2)
            zr(ivect4+i-1) = zr(ivect4+i-1) + poids * zr(ivf+k+i-1) * zr(isief+nbcmp*(kp-1)+3)
            if (ndim .eq. 3) then
                zr(ivect5+i-1) = zr(ivect5+i-1) + poids * zr(ivf+k+i- 1) * zr(isief+nbcmp*(kp-1)+&
                                 &4)
                zr(ivect6+i-1) = zr(ivect6+i-1) + poids * zr(ivf+k+i- 1) * zr(isief+nbcmp*(kp-1)+&
                                 &5)
            endif
30      continue
10  end do
!
end subroutine
