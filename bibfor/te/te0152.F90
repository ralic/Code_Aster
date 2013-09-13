subroutine te0152(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
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
!     CALCULE DES TERMES PROPRES A UN STRUCTURE
!     OPTION : 'MASS_INER'              (ELEMENTS FLUIDES 3D)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: l, lcastr, nbres, ndim, nnos
    real(kind=8) :: r8bid, rho, xxi, yyi, zero, zzi
!-----------------------------------------------------------------------
    parameter (nbres=2)
    character(len=8) :: nomres(nbres), fami, poum
!
    integer :: icodre(nbres)
    real(kind=8) :: valres(nbres)
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, volume
    real(kind=8) :: x(27), y(27), z(27), xg, yg, zg, matine(6)
    integer :: ipoids, ivf, idfde, igeom
    integer :: jgano, nno, kp, npg, i, j, imate, kpg, spt
!     ------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    zero = 0.d0
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    nomres(1) = 'RHO'
    nomres(2) = 'CELE_R'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', r8bid,&
                2, nomres, valres, icodre, 1)
    rho = valres(1)
    if (rho .le. r8prem()) then
        call utmess('F', 'ELEMENTS5_45')
    endif
!
    do 20 i = 1, nno
        x(i) = zr(igeom+3* (i-1))
        y(i) = zr(igeom+3*i-2)
        z(i) = zr(igeom+3*i-1)
20  end do
!
    call jevech('PMASSINE', 'E', lcastr)
    do 30 i = 0, 3
        zr(lcastr+i) = zero
30  end do
    do 40 i = 1, 6
        matine(i) = zero
40  end do
!
!     --- BOUCLE SUR LES POINTS DE GAUSS
    volume = 0.d0
    do 70 kp = 1, npg
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, dfdz, poids)
!
        volume = volume + poids
        do 60 i = 1, nno
!           --- CDG ---
            zr(lcastr+1) = zr(lcastr+1) + poids*x(i)*zr(ivf+l+i-1)
            zr(lcastr+2) = zr(lcastr+2) + poids*y(i)*zr(ivf+l+i-1)
            zr(lcastr+3) = zr(lcastr+3) + poids*z(i)*zr(ivf+l+i-1)
!           --- INERTIE ---
            xxi = 0.d0
            yyi = 0.d0
            zzi = 0.d0
            do 50 j = 1, nno
                xxi = xxi + x(i)*zr(ivf+l+i-1)*x(j)*zr(ivf+l+j-1)
                yyi = yyi + y(i)*zr(ivf+l+i-1)*y(j)*zr(ivf+l+j-1)
                zzi = zzi + z(i)*zr(ivf+l+i-1)*z(j)*zr(ivf+l+j-1)
                matine(2) = matine(2) + poids*x(i)*zr(ivf+l+i-1)*y(j)* zr(ivf+l+j-1)
                matine(4) = matine(4) + poids*x(i)*zr(ivf+l+i-1)*z(j)* zr(ivf+l+j-1)
                matine(5) = matine(5) + poids*y(i)*zr(ivf+l+i-1)*z(j)* zr(ivf+l+j-1)
50          continue
            matine(1) = matine(1) + poids* (yyi+zzi)
            matine(3) = matine(3) + poids* (xxi+zzi)
            matine(6) = matine(6) + poids* (xxi+yyi)
60      continue
70  end do
!
    xg = zr(lcastr+1)/volume
    yg = zr(lcastr+2)/volume
    zg = zr(lcastr+3)/volume
    zr(lcastr) = volume*rho
    zr(lcastr+1) = xg
    zr(lcastr+2) = yg
    zr(lcastr+3) = zg
!
!     ---ON DONNE LES INERTIES EN G ---
    zr(lcastr+4) = matine(1)*rho - zr(lcastr)* (yg*yg+zg*zg)
    zr(lcastr+5) = matine(3)*rho - zr(lcastr)* (xg*xg+zg*zg)
    zr(lcastr+6) = matine(6)*rho - zr(lcastr)* (xg*xg+yg*yg)
    zr(lcastr+7) = matine(2)*rho - zr(lcastr)* (xg*yg)
    zr(lcastr+8) = matine(4)*rho - zr(lcastr)* (xg*zg)
    zr(lcastr+9) = matine(5)*rho - zr(lcastr)* (yg*zg)
!
end subroutine
