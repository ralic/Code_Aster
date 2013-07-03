subroutine te0021(option, nomte)
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
!.......................................................................
    implicit none
!
!     BUT: CALCUL DES MATRICES DE RAIDEUR CENTRIFUGE ELEMENTAIRES
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'RIGI_MECA_RO '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
!
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
    integer :: icodre
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: a(3, 3, 27, 27)
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: jgano, nno, kp, i, j, imatuu, kpg, spt
!
!
!-----------------------------------------------------------------------
    integer :: ijkl, ik, irota, k, l, ndim, nnos
    integer :: npg2
    real(kind=8) :: omega1, omega2, omega3, r8b, rho, wij
!-----------------------------------------------------------------------
    call elref4(' ', 'MASS', ndim, nno, nnos,&
                npg2, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PROTATR', 'L', irota)
    call jevech('PMATUUR', 'E', imatuu)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', 0, ' ', r8b,&
                1, 'RHO', rho, icodre, 1)
    omega1 = zr(irota+1)*zr(irota)
    omega2 = zr(irota+2)*zr(irota)
    omega3 = zr(irota+3)*zr(irota)
!
    do 50 k = 1, 3
        do 40 l = 1, 3
            do 30 i = 1, nno
                do 20 j = 1, i
                    a(k,l,i,j) = 0.d0
20              continue
30          continue
40      continue
50  end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 80 kp = 1, npg2
!
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, dfdz, poids)
!
        do 70 i = 1, nno
            do 60 j = 1, i
                wij = rho*poids*zr(ivf+l+i-1)*zr(ivf+l+j-1)
                a(1,1,i,j) = a(1,1,i,j) - (omega2**2+omega3**2)*wij
                a(2,2,i,j) = a(2,2,i,j) - (omega1**2+omega3**2)*wij
                a(3,3,i,j) = a(3,3,i,j) - (omega1**2+omega2**2)*wij
                a(2,1,i,j) = a(2,1,i,j) + omega1*omega2*wij
                a(3,1,i,j) = a(3,1,i,j) + omega1*omega3*wij
                a(3,2,i,j) = a(3,2,i,j) + omega2*omega3*wij
60          continue
70      continue
80  end do
!
    do 100 i = 1, nno
        do 90 j = 1, i
            a(1,2,i,j) = a(2,1,i,j)
            a(1,3,i,j) = a(3,1,i,j)
            a(2,3,i,j) = a(3,2,i,j)
90      continue
100  end do
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
    do 140 k = 1, 3
        do 130 l = 1, 3
            do 120 i = 1, nno
                ik = ((3*i+k-4)* (3*i+k-3))/2
                do 110 j = 1, i
                    ijkl = ik + 3* (j-1) + l
                    zr(imatuu+ijkl-1) = a(k,l,i,j)
110              continue
120          continue
130      continue
140  end do
!
end subroutine
