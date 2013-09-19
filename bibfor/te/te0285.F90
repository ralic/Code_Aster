subroutine te0285(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
!     ------------------------------------------------------------------
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
!     CALCUL DES OPTIONS: 'MASS_INER' ELEMENTS 2-D AXI D-PLAN, C-PLAN
!                         'CARA_GEOM' ELEMENTS 2-D D-PLAN
!
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
!     ------------------------------------------------------------------
!
    integer :: ndim, nno, nnos, kp, npg, i, j, k, lcastr, jgano
    integer :: ipoids, ivf, idfde, igeom, imate
    real(kind=8) :: rho(1), xg, yg, depi, zero
    real(kind=8) :: dfdx(9), dfdy(9), poids, r, x(9), y(9)
    real(kind=8) :: matine(6), r8b, xxi, xyi, yyi, volume
    real(kind=8) :: ixrp2, iyrp2, xp(9), yp(9), xpg, ypg
    integer :: icodre(1)
    character(len=8) :: elrefe
    character(len=16) :: phenom
!     ------------------------------------------------------------------
!
    call elref1(elrefe)
!
    zero = 0.d0
    depi = r8depi()
!
    call elref4(' ', 'MASS', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    if (option .eq. 'MASS_INER') then
        call jevech('PMATERC', 'L', imate)
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
!
        if (phenom .eq. 'ELAS' .or. phenom .eq. 'ELAS_ISTR' .or. phenom .eq. 'ELAS_ORTH') then
            call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                        ' ', phenom, 0, ' ', [0.d0],&
                        1, 'RHO', rho, icodre(1), 1)
            if (rho(1) .le. r8prem()) then
                call utmess('F', 'ELEMENTS5_45')
            endif
            call jevech('PMASSINE', 'E', lcastr)
        else
            call utmess('F', 'ELEMENTS_50')
        endif
    else if (option.eq.'CARA_GEOM') then
!
!       POUR LE CALCUL DES CARA_GEOM DE SECTION DE POUTRE RHO=1
        rho(1)=1.d0
        call jevech('PCARAGE', 'E', lcastr)
    else
!          OPTION DE CALCUL NON VALIDE
        ASSERT(.false.)
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    do 10 i = 1, nno
        x(i) = zr(igeom-2+2*i)
        y(i) = zr(igeom-1+2*i)
        xp(i) = zero
        yp(i) = zero
10  end do
!
    do 20 i = 0, 3
        zr(lcastr+i) = zero
20  end do
    do 22 i = 1, 6
        matine(i) = zero
22  end do
!
!     --- BOUCLE SUR LES POINTS DE GAUSS ---
    volume = zero
    do 100 kp = 1, npg
        k = (kp-1) * nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy)
        if (lteatt(' ','AXIS','OUI')) then
            r = zero
            do 102 i = 1, nno
                r = r + zr(igeom-2+2*i)*zr(ivf+k+i-1)
102          continue
            poids = poids*r
        endif
        volume = volume + poids
        do 104 i = 1, nno
!           --- CDG ---
            zr(lcastr+1) = zr(lcastr+1)+poids*x(i)*zr(ivf+k+i-1)
            zr(lcastr+2) = zr(lcastr+2)+poids*y(i)*zr(ivf+k+i-1)
!           --- INERTIE ---
            xxi = 0.d0
            xyi = 0.d0
            yyi = 0.d0
            do 106 j = 1, nno
                xxi = xxi + x(i)*zr(ivf+k+i-1)*x(j)*zr(ivf+k+j-1)
                xyi = xyi + x(i)*zr(ivf+k+i-1)*y(j)*zr(ivf+k+j-1)
                yyi = yyi + y(i)*zr(ivf+k+i-1)*y(j)*zr(ivf+k+j-1)
106          continue
            matine(1) = matine(1) + poids*yyi
            matine(2) = matine(2) + poids*xyi
            matine(3) = matine(3) + poids*xxi
104      continue
100  end do
!
    if (lteatt(' ','AXIS','OUI')) then
        xg = zero
        yg = zr(lcastr+2) / volume
        zr(lcastr) = depi * volume * rho(1)
        zr(lcastr+3) = yg
        zr(lcastr+1) = zero
        zr(lcastr+2) = zero
!
!        --- ON DONNE LES INERTIES AU CDG ---
        matine(6) = matine(3) * rho(1) * depi
        matine(1) = matine(1) * rho(1) * depi + matine(6)/2.d0 - zr( lcastr)*yg*yg
        matine(2) = zero
        matine(3) = matine(1)
!
    else
        zr(lcastr) = volume * rho(1)
        zr(lcastr+1) = zr(lcastr+1) / volume
        zr(lcastr+2) = zr(lcastr+2) / volume
        zr(lcastr+3) = zero
!
!        --- ON DONNE LES INERTIES AU CDG ---
        xg = zr(lcastr+1)
        yg = zr(lcastr+2)
        matine(1) = matine(1)*rho(1) - zr(lcastr)*yg*yg
        matine(2) = matine(2)*rho(1) - zr(lcastr)*xg*yg
        matine(3) = matine(3)*rho(1) - zr(lcastr)*xg*xg
        matine(6) = matine(1) + matine(3)
    endif
    zr(lcastr+4) = matine(1)
    zr(lcastr+5) = matine(3)
    zr(lcastr+6) = matine(6)
    zr(lcastr+7) = matine(2)
    zr(lcastr+8) = matine(4)
    zr(lcastr+9) = matine(5)
!
    if (option .eq. 'CARA_GEOM') then
!
! --- CALCUL DE IXRP2 = SOMME((Y*(X**2 + Y**2).DS) ET
! --- CALCUL DE IYRP2 = SOMME((X*(X**2 + Y**2).DS) :
!     --------------------------------------------
        do 110 i = 1, nno
            xp(i) = x(i) - xg
            yp(i) = y(i) - yg
110      continue
!
        ixrp2 = zero
        iyrp2 = zero
!
        do 120 kp = 1, npg
            k = (kp-1) * nno
            call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy)
!
            xpg = zero
            ypg = zero
            do 130 i = 1, nno
                xpg = xpg + xp(i)*zr(ivf+k+i-1)
                ypg = ypg + yp(i)*zr(ivf+k+i-1)
130          continue
!
            ixrp2 = ixrp2 + xpg*(xpg*xpg + ypg*ypg)*poids
            iyrp2 = iyrp2 + ypg*(xpg*xpg + ypg*ypg)*poids
!
120      continue
!
        zr(lcastr+10) = ixrp2
        zr(lcastr+11) = iyrp2
!
    endif
!
end subroutine
