subroutine te0157(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterc/r8prem.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
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
!     CALCUL DE L' OPTION: 'MASS_INER' ELEMENTS FLUIDES
!                                       2-D AXI D-PLAN, C-PLAN
!
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
!     ------------------------------------------------------------------
!
    integer :: nbres, nno, kp, nnos, npg2, i, j, k, lcastr
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: ndim, jgano, kpg, spt
    parameter         ( nbres=2 )
    real(kind=8) :: valres(nbres)
    real(kind=8) :: rho, xg, yg, depi, zero
    real(kind=8) :: poids, r, x(9), y(9), volume
    real(kind=8) :: matine(6), xxi, yyi, xyi
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), fami, poum
!     ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg2, ipoids, ivf, idfde, jgano)
!
    zero = 0.d0
    depi = r8depi()
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call jevech('PMATERC', 'L', imate)
!
    nomres(1) = 'RHO'
    nomres(2) = 'CELE_R'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', [0.d0],&
                2, nomres, valres, icodre, 1)
    rho = valres(1)
    if (rho .le. r8prem()) then
        call utmess('F', 'ELEMENTS5_45')
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    do i = 1, nno
        x(i) = zr(igeom-2+2*i)
        y(i) = zr(igeom-1+2*i)
    end do
!
    call jevech('PMASSINE', 'E', lcastr)
    do i = 0, 3
        zr(lcastr+i) = zero
    end do
    do i = 1, 6
        matine(i) = zero
    end do
!
!     --- BOUCLE SUR LES POINTS DE GAUSS ---
    volume = zero
    do kp = 1, npg2
        k = (kp-1) * nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids)
        if (lteatt('AXIS','OUI')) then
            r = zero
            do i = 1, nno
                r = r + zr(igeom-2+2*i)*zr(ivf+k+i-1)
            end do
            poids = poids*r
        endif
        volume = volume + poids
        do i = 1, nno
!           --- CDG ---
            zr(lcastr+1) = zr(lcastr+1)+poids*x(i)*zr(ivf+k+i-1)
            zr(lcastr+2) = zr(lcastr+2)+poids*y(i)*zr(ivf+k+i-1)
!           --- INERTIE ---
            xxi = 0.d0
            xyi = 0.d0
            yyi = 0.d0
            do j = 1, nno
                xxi = xxi + x(i)*zr(ivf+k+i-1)*x(j)*zr(ivf+k+j-1)
                xyi = xyi + x(i)*zr(ivf+k+i-1)*y(j)*zr(ivf+k+j-1)
                yyi = yyi + y(i)*zr(ivf+k+i-1)*y(j)*zr(ivf+k+j-1)
            end do
            matine(1) = matine(1) + poids*yyi
            matine(2) = matine(2) + poids*xyi
            matine(3) = matine(3) + poids*xxi
        end do
    end do
!
    if (lteatt('AXIS','OUI')) then
        yg = zr(lcastr+2) / volume
        zr(lcastr) = depi * volume * rho
        zr(lcastr+3) = yg
        zr(lcastr+1) = zero
        zr(lcastr+2) = zero
!
!        --- ON DONNE LES INERTIES AU CDG ---
        matine(6) = matine(3) * rho * depi
        matine(1) = matine(1) * rho * depi + matine(6)/2.d0 - zr( lcastr)*yg*yg
        matine(2) = zero
        matine(3) = matine(1)
!
    else
        zr(lcastr) = volume * rho
        zr(lcastr+1) = zr(lcastr+1) / volume
        zr(lcastr+2) = zr(lcastr+2) / volume
        zr(lcastr+3) = zero
!
!        --- ON DONNE LES INERTIES AU CDG ---
        xg = zr(lcastr+1)
        yg = zr(lcastr+2)
        matine(1) = matine(1)*rho - zr(lcastr)*yg*yg
        matine(2) = matine(2)*rho - zr(lcastr)*xg*yg
        matine(3) = matine(3)*rho - zr(lcastr)*xg*xg
        matine(6) = matine(1) + matine(3)
    endif
    zr(lcastr+4) = matine(1)
    zr(lcastr+5) = matine(3)
    zr(lcastr+6) = matine(6)
    zr(lcastr+7) = matine(2)
    zr(lcastr+8) = matine(4)
    zr(lcastr+9) = matine(5)
!
end subroutine
