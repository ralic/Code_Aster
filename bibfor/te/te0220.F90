subroutine te0220(option, nomte)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:
!                         CALCUL DE L'ENERGIE THERMIQUE A L'EQUILIBRE
!                         OPTION : 'EPOT_ELEM_TEMP'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    integer :: icodre, kpg, spt
    character(len=8) :: nompar, fami, poum
    real(kind=8) :: valres, valpar
    real(kind=8) :: dfdx(9), dfdy(9), poids, flux, fluy, epot
    integer :: ndim, nno, nnos, npg, kp, j, itempe, itemp, iener
    integer :: ipoids, ivf, idfde, jgano, igeom, imate, iret, nbpar
!     ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPER', 'L', itempe)
    call jevech('PENERDR', 'E', iener)
!
    call tecach('ONN', 'PTEMPSR', 'L', 1, itemp,&
                iret)
    if (itemp .eq. 0) then
        nbpar = 0
        nompar = ' '
        valpar = 0.d0
    else
        nbpar = 1
        nompar = 'INST'
        valpar = zr(itemp)
    endif
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THER', nbpar, nompar, valpar,&
                1, 'LAMBDA', valres, icodre, 1)
!
    epot = 0.d0
    do 101 kp = 1, npg
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, poids)
        flux = 0.d0
        fluy = 0.d0
        do 110 j = 1, nno
            flux = flux + zr(itempe+j-1)*dfdx(j)
            fluy = fluy + zr(itempe+j-1)*dfdy(j)
110      continue
!
        epot = epot - ( flux**2 + fluy**2 )*poids
101  end do
    zr(iener) = epot * valres / 2.d0
!
end subroutine
