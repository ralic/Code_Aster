subroutine te0173(option, nomte)
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
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_MECA_VNOR '
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
    real(kind=8) :: jac, nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9)
    integer :: ipoids, ivf, idfdx, idfdy, igeom
    integer :: ndim, nno, ipg, npg1, ivectu, imate
    integer :: idec, jdec, kdec, ldec, nnos, jgano
!
!
!-----------------------------------------------------------------------
    integer :: i, ii, ino, ivnor, j, jno, mater
!
    real(kind=8) :: rho(1)
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    idfdy = idfdx + 1
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call jevech('PGEOMER', 'L', igeom)
!
    call jevech('PMATERC', 'L', imate)
    mater = zi(imate)
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'FLUIDE', 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre, 1)
!
    call jevech('PVECTUR', 'E', ivectu)
    call jevech('PSOURCR', 'L', ivnor)
!
    do 11 i = 1, 2*nno
        zr(ivectu+i-1) = 0.0d0
11  end do
!
!     CALCUL DES PRODUITS VECTORIELS OMI X OMJ
!
    do 21 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 22 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
22      continue
21  end do
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 ipg = 1, npg1
        kdec=(ipg-1)*nno*ndim
        ldec=(ipg-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!        CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
!
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
!
102          continue
!
!        CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac = sqrt (nx*nx + ny*ny + nz*nz)
!
!
        do 103 i = 1, nno
            ii = 2*i
            zr(ivectu+ii-1) = zr(ivectu+ii-1) - jac*zr(ipoids+ipg-1) * zr(ivnor+ipg-1) * rho(1) * &
                              zr(ivf+ldec+i-1)
103      continue
!
101  end do
!
end subroutine
