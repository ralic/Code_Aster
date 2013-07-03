subroutine te0014(option, nomte)
    implicit none
!
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
#include "jeveux.h"
!
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
    character(len=16) :: option, nomte
!.......................................................................
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_MECA_ROTA_R '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
    integer :: icodre
!
    character(len=16) :: phenom
    real(kind=8) :: amm(81, 81), ft(81), x(27), y(27), z(27)
    real(kind=8) :: xi, xij
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids
    real(kind=8) :: rho, om1, om2, om3, omm, omo, rri
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: jgano, ndl, nno, kp, npg, ii, jj, i, j, ideplm, ideplp
    integer :: ndim, ivectu, irota, l, ic
!
!-----------------------------------------------------------------------
    integer :: iret, nnos
    real(kind=8) :: r8b
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    ndl = 3*nno
    call jevech('PGEOMER', 'L', igeom)
    call tecach('ONN', 'PDEPLMR', 'L', 1, ideplm,&
                iret)
    call tecach('ONN', 'PDEPLPR', 'L', 1, ideplp,&
                iret)
    call jevech('PMATERC', 'L', imate)
    call jevech('PROTATR', 'L', irota)
    call jevech('PVECTUR', 'E', ivectu)
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre)
    call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                ' ', phenom, 0, ' ', r8b,&
                1, 'RHO', rho, icodre, 1)
!
    do 30 i = 1, ndl
        do 20 j = 1, ndl
            amm(i,j) = 0.d+00
20      continue
30  end do
!
    omm = zr(irota)*zr(irota)
    om1 = zr(irota)*zr(irota+1)
    om2 = zr(irota)*zr(irota+2)
    om3 = zr(irota)*zr(irota+3)
    if (ideplm .eq. 0 .or. ideplp .eq. 0) then
        do 40 i = 1, nno
            x(i) = zr(igeom+3* (i-1)) - zr(irota+4)
            y(i) = zr(igeom+3*i-2) - zr(irota+5)
            z(i) = zr(igeom+3*i-1) - zr(irota+6)
40      continue
    else
        do 50 i = 1, nno
            x(i) = zr(igeom+3*i-3) + zr(ideplm+3*i-3) + zr(ideplp+3*i- 3) - zr(irota+4)
            y(i) = zr(igeom+3*i-2) + zr(ideplm+3*i-2) + zr(ideplp+3*i- 2) - zr(irota+5)
            z(i) = zr(igeom+3*i-1) + zr(ideplm+3*i-1) + zr(ideplp+3*i- 1) - zr(irota+6)
50      continue
    endif
    do 60 i = 1, nno
        omo = om1*x(i) + om2*y(i) + om3*z(i)
        ft(3*i-2) = omm*x(i) - omo*om1
        ft(3*i-1) = omm*y(i) - omo*om2
        ft(3*i) = omm*z(i) - omo*om3
60  end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 100 kp = 1, npg
!
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, dfdz, poids)
        do 90 i = 1, nno
            xi = rho*poids*zr(ivf+l+i-1)
            ii = 3* (i-1)
            do 80 j = 1, nno
                xij = xi*zr(ivf+l+j-1)
                jj = 3* (j-1)
                do 70 ic = 1, 3
                    amm(ii+ic,jj+ic) = amm(ii+ic,jj+ic) + xij
70              continue
80          continue
90      continue
100  end do
!
    do 120 i = 1, ndl
        rri = 0.d0
        do 110 j = 1, ndl
            rri = rri + amm(i,j)*ft(j)
110      continue
        amm(i,i) = rri
120  end do
!
    do 130 i = 1, ndl
        zr(ivectu+i-1) = amm(i,i)
130  end do
!
end subroutine
