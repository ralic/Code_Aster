subroutine te0293(option, nomte)
    implicit none
#include "jeveux.h"
!
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
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
    integer :: i, ij, j, k, kp, nno, nnos, npg, ndim
    integer :: ipoids, ivf, idfde, jgano, igeom, imattt
!
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, r
!
    logical :: laxi
!
! ----------------------------------------------------------------------
!
    call elref4(' ', 'MASS', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATZZR', 'E', imattt)
!
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    do 10 kp = 1, npg
        k=(kp-1)*nno
        if (ndim .eq. 2) then
            call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, poids)
        else
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, dfdz, poids)
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
        ij = imattt - 1
        do 30 i = 1, nno
            do 30 j = 1, i
                ij = ij + 1
                zr(ij) = zr(ij) + poids * zr(ivf+k+i-1) * zr(ivf+k+j- 1)
30          continue
10  end do
!
end subroutine
