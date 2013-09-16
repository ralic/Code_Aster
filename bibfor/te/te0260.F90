subroutine te0260(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'RIGI_THER'
!                          ELEMENTS FOURIER
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: icodre(1)
    character(len=8) :: fami, poum
    real(kind=8) :: dfdr(9), dfdz(9), poids, r, theta, valres(1)
    integer :: nno, kp, npg1, i, j, k, itemps, imattt, ndim, nnos, jgano
    integer :: ipoids, ivf, idfde, igeom, imate, kpg, spt
!
!
!-----------------------------------------------------------------------
    integer :: iharm, ij, nh
    real(kind=8) :: r2, wij, xh, xh2
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PHARMON', 'L', iharm)
    nh = zi(iharm)
    if (nh .eq. -1) then
        call utmess('F', 'ELEMENTS3_63')
    endif
    xh = dble(nh)
    xh2 = xh*xh
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATTTR', 'E', imattt)
    call jevech('PTEMPSR', 'L', itemps)
    theta = zr(itemps+2)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THER', 1, 'INST', [zr(itemps)],&
                1, 'LAMBDA', valres, icodre, 1)
!
    do 101 kp = 1, npg1
        k = (kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdr, dfdz, poids)
!
        r = 0.d0
        do 102 i = 1, nno
            r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
102      continue
        r2 = r*r
        poids = poids*r
!
        ij = imattt - 1
        do 103 i = 1, nno
!
            do 103 j = 1, i
                wij = zr(ivf+k+i-1) * zr(ivf+k+j-1)
                ij = ij + 1
                zr(ij) = zr(ij) + poids * valres(1) * theta * &
                                ( dfdr(i)*dfdr(j) + dfdz(i)*dfdz(j) + xh2*wij/r2)
103          continue
101  end do
end subroutine
