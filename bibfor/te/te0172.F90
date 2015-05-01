subroutine te0172(option, nomte)
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
!     BUT: CALCUL DES MATRICES DE RIGIDITE  ELEMENTAIRES EN MECANIQUE
!          ELEMENTS 2D DE COUPLAGE ACOUSTICO-MECANIQUE
!
!          OPTION : 'MASS_MECA '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
!
    integer :: icodre(1), kpg, spt
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: a(4, 4, 27, 27), sx(27, 27), sy(27, 27)
    real(kind=8) :: sz(27, 27), norm(3), rho(1)
    integer :: igeom, imate
    integer :: i, j, k, l, ik, ijkl, idec, jdec, ldec, kdec, kco, ino, jno
    integer :: ndim, nno, ipg, nnos, npg2
    integer :: ipoids, ivf, idfdx, idfdy, imatuu, jgano
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
!
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATUUR', 'E', imatuu)
!
!    CALCUL DES PRODUITS VECTORIELS OMI X OMJ POUR LE CALCUL
!    DE L'ELEMENT DE SURFACE AU POINT DE GAUSS
!
    do 1 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 2 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2)*zr(j+3) - zr(i+3)*zr(j+2)
            sy(ino,jno) = zr(i+3)*zr(j+1) - zr(i+1)*zr(j+3)
            sz(ino,jno) = zr(i+1)*zr(j+2) - zr(i+2)*zr(j+1)
 2      continue
 1  end do
!
!     INITIALISATION DE LA MATRICE
!
    do 112 k = 1, 4
        do 112 l = 1, 4
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
!    CALCUL DE LA NORMALE DE LA SURFACE AU POINT DE GAUSS
!
        do 114 kco = 1, 3
            norm(kco) = 0.d0
114      end do
!
        do 120 i = 1, nno
            idec = (i-1)*ndim
            do 120 j = 1, nno
                jdec =(j-1)*ndim
!
                norm(1) = norm(1) + zr(idfdx+kdec+idec) * zr(idfdy+ kdec+jdec) * sx(i,j)
                norm(2) = norm(2) + zr(idfdx+kdec+idec) * zr(idfdy+ kdec+jdec) * sy(i,j)
                norm(3) = norm(3) + zr(idfdx+kdec+idec) * zr(idfdy+ kdec+jdec) * sz(i,j)
!
120          continue
!
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'FLUIDE', 0, ' ', [0.d0],&
                    1, 'RHO', rho, icodre, 1)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALCUL DU TERME PHI*(U.N DS)       C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        do 130 ino = 1, nno
            do 140 jno = 1, ino
                do 150 kco = 1, 3
!
                    a(kco,4,ino,jno) = a(kco,4,ino,jno) + zr(ipoids+ ipg-1) * norm(kco) * rho(1) *&
                                       zr(ivf+ldec+ino-1) * zr(ivf+ldec+jno-1)
!
150              continue
140          continue
130      continue
113  continue
!
    do 151 ino = 1, nno
        do 152 jno = 1, ino
            do 153 kco = 1, 3
                a(4,kco,ino,jno) = a(kco,4,ino,jno)
153          continue
152      continue
151  end do
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
    ijkl = 0
    ik = 0
    do 160 k = 1, 4
        do 160 l = 1, 4
            do 160 i = 1, nno
                ik = ((4*i+k-5) * (4*i+k-4)) / 2
                do 160 j = 1, i
                    ijkl = ik + 4 * (j-1) + l
                    zr(imatuu+ijkl-1) = a(k,l,i,j)
160              continue
!
end subroutine
