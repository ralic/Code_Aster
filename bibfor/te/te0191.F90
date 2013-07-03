subroutine te0191(option, nomte)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/vecma.h"
    character(len=16) :: option, nomte
! ......................................................................
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
!
!    - FONCTION REALISEE:  CALCUL DES MATRICES DE MASSE ELEMENTAIRES
!                          POUR LES ELEMENTS DE FOURIER
!                          OPTION : 'MASS_MECA       '
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    character(len=8) :: fami, poum
    character(len=16) :: phenom
    integer :: icodre
    real(kind=8) :: a(3, 3, 9, 9), dfdx(9), dfdy(9), poids, r, r8b, rho
    real(kind=8) :: matp(27, 27), matv(378)
    integer :: nno, kp, nnos, npg2, i, j, k, l, imatuu, nddl, nvec, iacce, ivect
    integer :: ipoids, ivf, idfde, igeom, imate, ijkl, ik, kpg, spt
    integer :: ndim, jgano
! ......................................................................
!
    call elref4(' ', 'MASS', ndim, nno, nnos,&
                npg2, ipoids, ivf, idfde, jgano)
    nddl = 3 * nno
    nvec = nddl * ( nddl + 1 ) / 2
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre)
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', phenom, 0, ' ', r8b,&
                1, 'RHO', rho, icodre, 1)
!
    do 113 k = 1, 3
        do 113 l = 1, 3
            do 113 i = 1, nno
                do 113 j = 1, i
                    a(k,l,i,j) = 0.0d0
113              continue
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 kp = 1, npg2
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, poids)
!
        r = 0.0d0
        do 102 i = 1, nno
            r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
102      continue
        poids = poids*r*rho
!
        do 106 i = 1, nno
            do 107 j = 1, i
                a(1,1,i,j) = a(1,1,i,j) + poids * zr(ivf+k+i-1) * zr( ivf+k+j-1)
107          continue
106      continue
101  end do
!
    do 108 i = 1, nno
        do 109 j = 1, i
            a(2,2,i,j) = a(1,1,i,j)
            a(3,3,i,j) = a(1,1,i,j)
109      continue
108  end do
!
    if (option .eq. 'MASS_MECA') then
!
        call jevech('PMATUUR', 'E', imatuu)
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
        do 112 k = 1, 3
            do 112 l = 1, 3
                do 112 i = 1, nno
                    ik = ((3*i+k-4) * (3*i+k-3)) / 2
                    do 112 j = 1, i
                        ijkl = ik + 3 * (j-1) + l
                        zr(imatuu+ijkl-1) = a(k,l,i,j)
112                  continue
!
    else if (option .eq. 'M_GAMMA') then
!
        call jevech('PACCELR', 'L', iacce)
        call jevech('PVECTUR', 'E', ivect)
        do 210 k = 1, nvec
            matv(k) = 0.0d0
210      continue
        do 212 k = 1, 3
            do 212 l = 1, 3
                do 214 i = 1, nno
                    ik = ((3*i+k-4) * (3*i+k-3)) / 2
                    do 216 j = 1, i
                        ijkl = ik + 3 * (j-1) + l
                        matv(ijkl) = a(k,l,i,j)
216                  continue
214              continue
212          continue
        call vecma(matv, nvec, matp, nddl)
        call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
!
    else
!C OPTION DE CALCUL INVALIDE
        call assert(.false.)
    endif
!
end subroutine
