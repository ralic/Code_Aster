subroutine te0168(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/biline.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/pmavec.h"
#include "asterfort/rcvalb.h"
#include "asterfort/vecma.h"
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
!    - FONCTION REALISEE:  CALCUL MATRICE DE MASSE MECABLE
!                          OPTION : 'MASS_MECA'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    integer :: icodre(1)
    real(kind=8) :: a, rho(1), coef, jacobi, en(3, 2), r8b
    real(kind=8) :: matp(6, 6), matv(21)
    integer :: nno, npg, k, kp, i, ii, jj, ki, ky, nddl, nvec, imatuu, lsect
    integer :: ipoids, ivf, iyty, igeom, imate, iacce, ivect
    integer :: ndim, nnos, jgano, idfdk
! ......................................................................
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdk, jgano)
    call jevete('&INEL.CABPOU.YTY', 'L', iyty)
    nddl = 3*nno
    nvec = nddl* (nddl+1)/2
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                ' ', 'ELAS', 0, ' ', [r8b],&
                1, 'RHO', rho, icodre, 1)
    call jevech('PCACABL', 'L', lsect)
    a = zr(lsect)
!
    k = 0
    do 20 kp = 1, npg
        do 10 i = 1, nno
            k = k + 1
            en(i,kp) = zr(ivf-1+k)
10      continue
20  end do
!
    do 30 k = 1, nvec
        matv(k) = 0.0d0
30  end do
!
    do 70 kp = 1, npg
        ky = (kp-1)*nddl*nddl
        jacobi = sqrt(biline(nddl,zr(igeom),zr(iyty+ky),zr(igeom)))
        coef = rho(1)*a*jacobi*zr(ipoids-1+kp)
        k = 0
        do 60 ii = 1, nno
            do 50 ki = 1, 3
                k = k + ki - 3
                do 40 jj = 1, ii
                    k = k + 3
                    matv(k) = matv(k) + coef*en(ii,kp)*en(jj,kp)
40              continue
50          continue
60      continue
70  end do
!
    if (option .eq. 'MASS_MECA') then
!
        call jevech('PMATUUR', 'E', imatuu)
!
        do 80 i = 1, nvec
            zr(imatuu+i-1) = matv(i)
80      continue
!
    else if (option.eq.'M_GAMMA') then
!
        call jevech('PACCELR', 'L', iacce)
        call jevech('PVECTUR', 'E', ivect)
!
        call vecma(matv, nvec, matp, nddl)
        call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
!
    else
!C OPTION DE CALCUL INVALIDE
        ASSERT(.false.)
    endif
!
end subroutine
