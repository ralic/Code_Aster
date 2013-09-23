subroutine te0066(option, nomte)
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
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
!
    character(len=16) :: option, nomte
!.......................................................................
!
!     BUT: CALCUL DE L'ENERGIE THERMIQUE A L'EQUILIOBRE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'EPOT_ELEM_TEMP'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
    character(len=8) :: nompar, fami, poum
    integer :: icodre(1)
    real(kind=8) :: valpar, lambda(1), poids, epot
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), flux, fluy, fluz
    integer :: i, ipoids, ivf, idfde, igeom, imate, kpg, spt
    integer :: ndim, jgano, nno, kp, npg1, iener, itemp, itempe
!
!-----------------------------------------------------------------------
    integer :: iret, nbpar, nnos
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPER', 'L', itempe)
    call jevech('PENERDR', 'E', iener)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call tecach('ONN', 'PTEMPSR', 'L', iret, iad=itemp)
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
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THER', nbpar, nompar, [valpar],&
                1, 'LAMBDA', lambda, icodre, 1)
!
    epot = 0.d0
    do 30 kp = 1, npg1
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, dfdz, poids)
        flux = 0.d0
        fluy = 0.d0
        fluz = 0.d0
        do 20 i = 1, nno
            flux = flux + zr(itempe-1+i)*dfdx(i)
            fluy = fluy + zr(itempe-1+i)*dfdy(i)
            fluz = fluz + zr(itempe-1+i)*dfdz(i)
20      continue
!
        epot = epot - (flux**2+fluy**2+fluz**2)*poids
!
30  end do
    zr(iener) = epot*lambda(1)/2.d0
end subroutine
