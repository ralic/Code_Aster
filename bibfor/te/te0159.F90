subroutine te0159(nomopt, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
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
!
!     BUT: CALCUL DES MATRICES DE GYROSCOPIE ELEMENTAIRES
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'MECA_GYRO ' ET 'RIGI_GYRO '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    integer :: icodre(1)
    character(len=8) :: fami, poum
    character(len=16) :: nomte, nomopt
    real(kind=8) :: a(3, 3, 27, 27), poids
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: jgano, nno, kp, i, j, imatuu, kpg, spt
!
!
!-----------------------------------------------------------------------
    integer :: ijkl, ik, jl, irota, k, l, ndim, nnos
    integer :: npg2
    real(kind=8) :: omega1, omega2, omega3, rho(1), wij
!-----------------------------------------------------------------------
    call elref4(' ', 'MASS', ndim, nno, nnos,&
                npg2, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PROTATR', 'L', irota)
    call jevech('PMATUNS', 'E', imatuu)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, 'RHO', rho(1), icodre, 1)
    omega1 = zr(irota+1)*zr(irota)
    omega2 = zr(irota+2)*zr(irota)
    omega3 = zr(irota+3)*zr(irota)
!
    do 50 k = 1, 3
        do 40 l = 1, 3
            do 30 i = 1, nno
                do 20 j = 1, nno
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
                    poids)
!
        do 70 i = 1, nno
            do 60 j = 1, nno
                wij = rho(1)*poids*zr(ivf+l+i-1)*zr(ivf+l+j-1)
                if (nomopt.eq.'MECA_GYRO') then
                   wij = 2*wij
                endif
                a(1,1,i,j) = a(1,1,i,j) + 0.d0
                a(2,2,i,j) = a(2,2,i,j) + 0.d0
                a(3,3,i,j) = a(3,3,i,j) + 0.d0
                a(2,1,i,j) = a(2,1,i,j) + omega3*wij
                a(3,1,i,j) = a(3,1,i,j) - omega2*wij
                a(3,2,i,j) = a(3,2,i,j) + omega1*wij
                a(1,2,i,j) = a(1,2,i,j) - omega3*wij
                a(1,3,i,j) = a(1,3,i,j) + omega2*wij
                a(2,3,i,j) = a(2,3,i,j) - omega1*wij
60          continue
70      continue
80  end do
!
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
    do 140 k = 1, 3
        do 130 l = 1, 3
            do 120 i = 1, nno
                do 110 j = 1, nno
                    ik = 3 * nno * (3 * (i-1) + k - 1)
                    jl = 3 * (j-1) + l - 1
                    ijkl = ik + jl + 1
                    zr(imatuu+ijkl-1) = a(k,l,i,j)
110              continue
120          continue
130      continue
140  end do
!
end subroutine
