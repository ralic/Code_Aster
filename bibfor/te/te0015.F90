subroutine te0015(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
!
    character(len=16) :: nomte, option
!.......................................................................
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_MECA_PESA_R '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
    integer :: icodre(1)
    character(len=16) :: phenom
    real(kind=8) :: r8bid, rho(1), coef
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids
    integer :: ipoids, ivf, idfde, igeom
    integer :: jgano, imate, ipesa, ivectu, nnos
    integer :: ndim, nno, npg, ndl, kp, l, i, ii, j
!
!
!
!
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PPESANR', 'L', ipesa)
    call jevech('PVECTUR', 'E', ivectu)
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
    call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                ' ', phenom, 0, ' ', [r8bid],&
                1, 'RHO', rho, icodre(1), 1)
!
    ndl = 3*nno
    do 10 i = 1, ndl
        zr(ivectu+i-1) = 0.0d0
10  end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 40 kp = 1, npg
!
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, dfdz, poids)
!
        coef = rho(1)*poids*zr(ipesa)
!
        do 30 i = 1, nno
            ii = 3* (i-1)
!
            do 20 j = 1, 3
                zr(ivectu+ii+j-1) = zr(ivectu+ii+j-1) + coef*zr(ivf+l+ i-1)*zr(ipesa+j)
20          continue
!
30      continue
!
40  end do
!
!
end subroutine
