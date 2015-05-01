subroutine te0171(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!          ELEMENTS  DE FLUIDE ISOPARAMETRIQUES 3D
!
!          OPTION : 'MASS_MECA '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
!
!-----------------------------------------------------------------------
    integer :: k, nbres, nnos
!-----------------------------------------------------------------------
    parameter (nbres=2)
    character(len=8) :: fami, poum
    character(len=16) :: nomres(nbres)
!
    integer :: icodre(nbres), kpg, spt
    character(len=16) :: nomte, option
    real(kind=8) :: valres(nbres), a(2, 2, 27, 27)
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, rho, celer
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: jgano, nno, ndim, kp, npg1, ik, ijkl, i, j, l, imatuu
!
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATUUR', 'E', imatuu)
!
    nomres(1) = 'RHO'
    nomres(2) = 'CELE_R'
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
    do 50 k = 1, 2
        do 40 l = 1, 2
            do 30 i = 1, nno
                do 20 j = 1, i
                    a(k,l,i,j) = 0.d0
20              continue
30          continue
40      continue
50  end do
!
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 80 kp = 1, npg1
!
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!    TERME EN -RHO*(GRAD(PHI)**2)          C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        do 70 i = 1, nno
            do 60 j = 1, i
                a(2,2,i,j) = a(2,2,i,j) - poids* (dfdx(i)*dfdx(j)+ dfdy(i)*dfdy(j)+ dfdz(i)*dfdz(&
                             &j))*rho
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!    TERME EN   (P*PHI)/(CEL**2)       C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                a(1,2,i,j) = a(1,2,i,j) + poids*zr(ivf+l+i-1)*zr(ivf+ l+j-1)/ celer/celer
60          continue
!
70      continue
!
80  end do
!
    do 100 i = 1, nno
        do 90 j = 1, i
            a(2,1,i,j) = a(1,2,i,j)
90      continue
100  end do
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
    do 140 k = 1, 2
        do 130 l = 1, 2
            do 120 i = 1, nno
                ik = ((2*i+k-3)* (2*i+k-2))/2
                do 110 j = 1, i
                    ijkl = ik + 2* (j-1) + l
                    zr(imatuu+ijkl-1) = a(k,l,i,j)
110              continue
120          continue
130      continue
140  end do
!
end subroutine
