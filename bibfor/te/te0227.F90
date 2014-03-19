subroutine te0227(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterc/r8prem.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ......................................................................
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
!
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          COQUE 1D
!                          OPTION : 'MASS_INER       '
!                          ELEMENT: MECXSE3 , METCSE3 , METDSE3
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    character(len=8) :: elrefe, fami, poum
    integer :: icodre(1), kpg, spt
    real(kind=8) :: dfdx(3), r, rm, poids, cour, nx, ny, xg, yg
    real(kind=8) :: rho(1), x(3), y(3), xxi, xyi, yyi
    real(kind=8) :: matine(6), volume, depi, zero
    integer :: nno, nnos, jgano, ndim, ipoids, ivf, idfdk, igeom, imate, icaco
    integer :: kp, npg, i, j, k, lcastr
! ......................................................................
!
    call elref1(elrefe)
    zero = 0.0d0
    depi = r8depi()
!
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
!
!
    call jevech('PGEOMER', 'L', igeom)
    do 10 i = 1, nno
        x(i) = zr(igeom-2+2*i)
        y(i) = zr(igeom-1+2*i)
10  end do
!
    call jevech('PMATERC', 'L', imate)
    call jevech('PCACOQU', 'L', icaco)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre, 1)
    if (rho(1) .le. r8prem()) then
        call utmess('F', 'ELEMENTS5_45')
    endif
    rm = rho(1)*zr(icaco)
!
    call jevech('PMASSINE', 'E', lcastr)
!
    volume = zero
    do 20 i = 1, 6
        matine(i) = zero
20  end do
!
!     --- BOUCLE SUR LES POINTS DE GAUSS ---
!
    do 60 kp = 1, npg
        k = (kp-1)*nno
        call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                    cour, poids, nx, ny)
        if (nomte .eq. 'MECXSE3') then
            r = zero
            do 30 i = 1, nno
                r = r + zr(igeom+2* (i-1))*zr(ivf+k+i-1)
30          continue
            poids = poids*r
        endif
        volume = volume + poids
!
        do 50 i = 1, nno
!           --- CDG ---
            zr(lcastr+1) = zr(lcastr+1) + poids*x(i)*zr(ivf+k+i-1)
            zr(lcastr+2) = zr(lcastr+2) + poids*y(i)*zr(ivf+k+i-1)
!           --- INERTIE ---
            xxi = 0.d0
            xyi = 0.d0
            yyi = 0.d0
            do 40 j = 1, nno
                xxi = xxi + x(i)*zr(ivf+k+i-1)*x(j)*zr(ivf+k+j-1)
                xyi = xyi + x(i)*zr(ivf+k+i-1)*y(j)*zr(ivf+k+j-1)
                yyi = yyi + y(i)*zr(ivf+k+i-1)*y(j)*zr(ivf+k+j-1)
40          continue
            matine(1) = matine(1) + poids*yyi
            matine(2) = matine(2) + poids*xyi
            matine(3) = matine(3) + poids*xxi
50      continue
60  end do
!
    if (nomte .eq. 'MECXSE3') then
        yg = zr(lcastr+2)/volume
        zr(lcastr) = depi*volume*rm
        zr(lcastr+3) = yg
        zr(lcastr+1) = zero
        zr(lcastr+2) = zero
!
!        --- ON DONNE LES INERTIES AU CDG ---
        matine(6) = matine(3)*rm*depi
        matine(1) = matine(1)*rm*depi + matine(6)/2.d0 - zr(lcastr)* yg*yg
        matine(2) = zero
        matine(3) = matine(1)
!
    else
        zr(lcastr) = volume*rm
        zr(lcastr+1) = zr(lcastr+1)/volume
        zr(lcastr+2) = zr(lcastr+2)/volume
        zr(lcastr+3) = zero
!
!        --- ON DONNE LES INERTIES AU CDG ---
        xg = zr(lcastr+1)
        yg = zr(lcastr+2)
        matine(1) = matine(1)*rm - zr(lcastr)*yg*yg
        matine(2) = matine(2)*rm - zr(lcastr)*xg*yg
        matine(3) = matine(3)*rm - zr(lcastr)*xg*xg
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
