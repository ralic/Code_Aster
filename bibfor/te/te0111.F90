subroutine te0111(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - BUT :  CALCUL DES MATRICES DE RAIDEUR CENTRIFUGE ELEMENTAIRES
!                          POUR LES ELEMENTS DE FOURIER
!                          OPTION : 'RIGI_MECA_RO    '
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: icodre(1)
    character(len=4) :: fami
    real(kind=8) :: a(3, 3, 9, 9), dfdx(9), dfdy(9), poids, r
    integer :: nno, kp, npg2, i, j, imatuu, nnos, ndim, jgano
    integer :: ipoids, ivf, idfde, igeom, imate
!
!
!-----------------------------------------------------------------------
    integer :: ijkl, ik, irota, k, l
    real(kind=8) :: omega1, omega2, omega3, rho(1), wij
!-----------------------------------------------------------------------
    fami = 'MASS'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg2, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PROTATR', 'L', irota)
    call jevech('PMATUUR', 'E', imatuu)
!
    omega1 = zr(irota+1) * zr(irota)
    omega2 = zr(irota+2) * zr(irota)
    omega3 = zr(irota+3) * zr(irota)
!
    do 113 k = 1, 3
        do 113 l = 1, 3
            do 113 i = 1, nno
                do 113 j = 1, i
                    a(k,l,i,j) = 0.d0
113              continue
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 kp = 1, npg2
!
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, poids)
!
        r = 0.d0
        do 102 i = 1, nno
            r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
102      continue
        poids = poids*r
        call rcvalb(fami, kp, 1, '+', zi(imate),&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, 'RHO', rho, icodre, 1)
!
        do 106 i = 1, nno
!
            do 107 j = 1, i
!
                wij = rho(1) * poids * zr(ivf+l+i-1) * zr(ivf+l+j-1)
!
                a(1,1,i,j) = a(1,1,i,j) - (omega2**2 + omega3**2) * wij
!
                a(2,2,i,j) = a(2,2,i,j) - (omega1**2 + omega3**2) * wij
!
                a(3,3,i,j) = a(3,3,i,j) - (omega1**2 + omega2**2) * wij
!
                a(2,1,i,j) = a(2,1,i,j) + omega1 * omega2 * wij
!
                a(3,1,i,j) = a(3,1,i,j) + omega1 * omega3 * wij
!
                a(3,2,i,j) = a(3,2,i,j) + omega2 * omega3 * wij
!
107          continue
!
106      continue
!
101  end do
!
    do 108 i = 1, nno
        do 109 j = 1, i
            a(1,2,i,j) = a(2,1,i,j)
            a(1,3,i,j) = a(3,1,i,j)
            a(2,3,i,j) = a(3,2,i,j)
109      continue
108  continue
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
112              continue
!
end subroutine
