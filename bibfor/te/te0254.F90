subroutine te0254(option, nomte)
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
!     BUT: CALCUL DES MATRICES DE MASSE ELEMENTAIRES EN MECANIQUE
!          ELEMENTS  DE FLUIDE ISOPARAMETRIQUES 2D
!
!          OPTION : 'MASS_MECA '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
!
!-----------------------------------------------------------------------
    integer :: nbres
    real(kind=8) :: r
!-----------------------------------------------------------------------
    parameter         ( nbres=2 )
    character(len=8) :: nomres(nbres), fami, poum
    integer :: icodre(nbres), kpg, spt
    character(len=16) :: nomte, option
    real(kind=8) :: valres(nbres), a(2, 2, 9, 9)
    real(kind=8) :: dfdx(9), dfdy(9), poids, rho, celer
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: nno, ndim, kp, npg2, nnos, ik, ijkl, i, j, k, l
    integer :: imatuu, jgano
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg2, ipoids, ivf, idfde, jgano)
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATUUR', 'E', imatuu)
!
    nomres(1)='RHO'
    nomres(2)='CELE_R'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', [0.d0],&
                2, nomres, valres, icodre, 1)
    rho = valres(1)
    celer = valres(2)
!
    do 113 k = 1, 2
        do 113 l = 1, 2
            do 113 i = 1, nno
                do 113 j = 1, i
                    a(k,l,i,j) = 0.d0
113              continue
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 kp = 1, npg2
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy)
!%
        if (lteatt(' ','AXIS','OUI')) then
            r = 0.d0
            do 102 i = 1, nno
                r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
102          continue
            poids = poids*r
        endif
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!    TERME EN -RHO*(GRAD(PHI)**2)          C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        do 106 i = 1, nno
            do 107 j = 1, i
                a(2,2,i,j) = a(2,2,i,j) - poids * (dfdx(i)*dfdx(j) + dfdy(i)*dfdy(j))*rho
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!    TERME EN   (P*PHI)/(CEL**2)       C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                a(1,2,i,j) = a(1,2,i,j) + poids * zr(ivf+k+i-1) * zr( ivf+k+j-1)/ celer / celer
!
107          continue
!
106      continue
!
101  end do
!
    do 108 i = 1, nno
        do 109 j = 1, i
            a(2,1,i,j) = a(1,2,i,j)
109      continue
108  end do
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
    do 112 k = 1, 2
        do 112 l = 1, 2
            do 112 i = 1, nno
                ik = ((2*i+k-3) * (2*i+k-2)) / 2
                do 112 j = 1, i
                    ijkl = ik + 2 * (j-1) + l
                    zr(imatuu+ijkl-1) = a(k,l,i,j)
112              continue
!
end subroutine
