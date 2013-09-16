subroutine te0371(option, nomte)
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
!     BUT: CALCUL DES MATRICES DE RIGIDITE  ELEMENTAIRES EN MECANIQUE
!          ELEMENTS 2D DE COUPLAGE PESANTEUR-SURFACE LIBRE D'UN FLUIDE
!
!          OPTION : 'MASS_MECA'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
#include "jeveux.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
!
    integer :: icodre(1)
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option
    integer :: igeom, imate, kpg, spt
    integer :: i, j, k, l, ik, ijkl, ldec, kdec, ino, jno
    integer :: ndim, nno, npg2, ipg, nnos, jgano
    integer :: ipoids, ivf, idfrde, imatuu
    real(kind=8) :: a(2, 2, 27, 27), rho(1)
    real(kind=8) :: poids, jac
    real(kind=8) :: dxde, dxdk, dyde, dydk
!
!
!-----------------------------------------------------------------------
    real(kind=8) :: r8b
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg2, ipoids, ivf, idfrde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATUUR', 'E', imatuu)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', [r8b],&
                1, 'RHO', rho, icodre, 1)
!
!     INITIALISATION DE LA MATRICE
!
    do 112 k = 1, 2
        do 112 l = 1, 2
            do 112 i = 1, nno
                do 112 j = 1, i
                    a(k,l,i,j) = 0.d0
112              continue
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 113 ipg = 1, npg2
!
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
        dxde=0.d0
        dxdk=0.d0
        dyde=0.d0
        dydk=0.d0
        do 100 i = 1, nno
            dxde=dxde+zr(igeom+3*(i-1))*zr(idfrde+kdec+(i-1)*ndim)
            dxdk=dxdk+zr(igeom+3*(i-1))*zr(idfrde+kdec+(i-1)*ndim+1)
            dyde=dyde+zr(igeom+3*(i-1)+1)*zr(idfrde+kdec+(i-1)*ndim)
            dydk=dydk+zr(igeom+3*(i-1)+1)*zr(idfrde+kdec+(i-1)*ndim+1)
100      continue
        jac = dxde*dydk-dxdk*dyde
        poids = abs(jac)*zr(ipoids+ipg-1)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALCUL DU TERME RHO * PHI * Z      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        do 130 ino = 1, nno
            do 140 jno = 1, ino
                a(1,2,ino,jno) = a(1,2,ino,jno) + poids * rho(1) * zr(ivf+ldec+ino-1) * &
                                 zr(ivf+ldec+jno-1)
140          continue
130      continue
113  continue
!
    do 151 ino = 1, nno
        do 152 jno = 1, ino
            a(2,1,ino,jno) = a(1,2,ino,jno)
152      continue
151  end do
!
! PASSAGE DU STOCKAGE RECTANGULAIRE AU STOCKAGE TRIANGULAIRE
!
    ijkl = 0
    ik = 0
    do 160 k = 1, 2
        do 160 l = 1, 2
            do 160 i = 1, nno
                ik = ((2*i+k-3) * (2*i+k-2)) / 2
                do 160 j = 1, i
                    ijkl = ik + 2 * (j-1) + l
                    zr(imatuu+ijkl-1) = a(k,l,i,j)
160              continue
!
end subroutine
