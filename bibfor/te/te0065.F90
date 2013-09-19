subroutine te0065(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref2.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
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
!     CALCULE DES TERMES PROPRES A UN STRUCTURE
!     OPTION : 'MASS_INER'              (ELEMENTS ISOPARAMETRIQUES 3D)
!     ------------------------------------------------------------------
    integer :: nbres, nbfamx
!-----------------------------------------------------------------------
    integer :: l, lcastr, ndim, nnos
    real(kind=8) :: rho(1), xxi, yyi, zero, zzi
!-----------------------------------------------------------------------
    parameter   ( nbres = 3, nbfamx = 20 )
!
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), lielrf(nbfamx)
    character(len=16) :: phenom
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, volume
    real(kind=8) :: x(27), y(27), z(27), xg, yg, zg, matine(6)
    real(kind=8) :: rhopou, rhoflu, tpg, valres(nbres), ayz, ycell, rapp, yf
    integer :: ipoids, ivf, idfde, igeom, nbv, lsect, lcorr
    integer :: jgano, nno, kp, npg, i, j, imate, ntrou
!     ------------------------------------------------------------------
    zero = 0.d0
!
    call elref2(nomte, nbfamx, lielrf, ntrou)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
!
    if (lielrf(2)(1:4) .eq. 'POHO') then
!
!        POUR LES ELEMENTS DE LA MODELISATION '3D_FAISCEAU':
!        ===================================================
!
!        - DETERMINATION DU RHO 'POUTRE': RHOPOU
        if (phenom .eq. 'ELAS') then
            nomres(1) = 'RHO'
            nbv = 1
        else
            call utmess('F', 'ELEMENTS3_98')
        endif
        tpg = 0.d0
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', [tpg],&
                    nbv, nomres, valres, icodre, 1)
        rhopou = valres(1)
!
!        - DETERMINATION DU RHO 'FLUIDE': RHOFLU
        call rccoma(zi(imate), 'FLUIDE', 1, phenom, icodre(1))
        if (phenom .eq. 'FLUIDE') then
            nomres(1) = 'RHO'
            nbv = 1
        else
            call utmess('F', 'ELEMENTS3_98')
        endif
        tpg = 0.d0
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', [tpg],&
                    nbv, nomres, valres, icodre, 1)
        rhoflu = valres(1)
!
!        - DETERMINATION DU RHO 'EQUIVALENT' : RHO
!          RHO = ( RHOPOU * AYZ * RAPP ) +  ( RHOFLUI * YF )
!                 RAPP :=  COEF_ECH **2 / A_CELL
!                 YF   :=  A_FLUI  / A_CELL
!                 AYZ  := AIRE_SECTION_POUTRE
        call jevech('PCAGNPO', 'L', lsect)
        ayz = zr(lsect)
        call jevech('PCAPOUF', 'L', lcorr)
        ycell = zr(lcorr+4)
        rapp = zr(lcorr+5)
        rapp = rapp * rapp / ycell
        yf = zr(lcorr+3)/ycell
        rho(1) = ( rhopou * ayz * rapp ) + ( rhoflu * yf )
        call elref4(lielrf(1), 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
!
    else
        call elref4(' ', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
        if (phenom .eq. 'ELAS' .or. phenom .eq. 'ELAS_ISTR' .or. phenom .eq. 'ELAS_ORTH') then
            call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                        ' ', phenom, 0, ' ', [0.d0],&
                        1, 'RHO', rho, icodre, 1)
            if (rho(1) .le. r8prem()) then
                call utmess('F', 'ELEMENTS5_45')
            endif
        else
            call utmess('F', 'ELEMENTS_50')
        endif
    endif
!
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
                    poids)
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
    zr(lcastr) = volume*rho(1)
    zr(lcastr+1) = xg
    zr(lcastr+2) = yg
    zr(lcastr+3) = zg
!
!     ---ON DONNE LES INERTIES EN G ---
    zr(lcastr+4) = matine(1)*rho(1) - zr(lcastr)* (yg*yg+zg*zg)
    zr(lcastr+5) = matine(3)*rho(1) - zr(lcastr)* (xg*xg+zg*zg)
    zr(lcastr+6) = matine(6)*rho(1) - zr(lcastr)* (xg*xg+yg*yg)
    zr(lcastr+7) = matine(2)*rho(1) - zr(lcastr)* (xg*yg)
    zr(lcastr+8) = matine(4)*rho(1) - zr(lcastr)* (xg*zg)
    zr(lcastr+9) = matine(5)*rho(1) - zr(lcastr)* (yg*zg)
!
end subroutine
