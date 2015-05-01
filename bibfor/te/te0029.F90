subroutine te0029(option, nomte)
    implicit none
!.......................................................................
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
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT FORCE_FACE FONCTION
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_MECA_FF2D3D '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
    character(len=8) :: nompar(4)
    character(len=16) :: nomte, option
    real(kind=8) :: jac, nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9)
    real(kind=8) :: valpar(4)
    integer :: ipoids, ivf, idfdx, idfdy, igeom, itemps
    integer :: ndim, nno, ipg, npg1, iforc, nnos
    integer :: idec, jdec, kdec, ldec, jgano
!
!-----------------------------------------------------------------------
    integer :: i, ier, ii, ino, ires, j, jno
!
    real(kind=8) :: fx, fy, fz, xx, yy, zz
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PFF2D3D', 'L', iforc)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PVECTUR', 'E', ires)
!
    valpar(4) = zr(itemps)
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
    nompar(4) = 'INST'
!
    do 10 i = 1, 3*nno
        zr(ires+i-1) = 0.0d0
10  end do
!
!    CALCUL DES PRODUITS VECTORIELS OMI X OMJ
!
    do 1 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 2 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
 2      continue
 1  end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 ipg = 1, npg1
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
!    CALCUL DE FX,FY,FZ
!
        xx = 0.d0
        yy = 0.d0
        zz = 0.d0
        do 110 i = 1, nno
            xx = xx + zr(igeom+3*(i-1)) * zr(ivf+ldec+i-1)
            yy = yy + zr(igeom+3*i-2) * zr(ivf+ldec+i-1)
            zz = zz + zr(igeom+3*i-1) * zr(ivf+ldec+i-1)
110      continue
        valpar(1) = xx
        valpar(2) = yy
        valpar(3) = zz
        call fointe('FM', zk8(iforc), 4, nompar, valpar,&
                    fx, ier)
        call fointe('FM', zk8(iforc+1), 4, nompar, valpar,&
                    fy, ier)
        call fointe('FM', zk8(iforc+2), 4, nompar, valpar,&
                    fz, ier)
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!   CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
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
!   LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
!
        jac = sqrt (nx*nx + ny*ny + nz*nz)
!
        do 103 i = 1, nno
            ii = 3 * (i-1) -1
!
            zr(ires+ii+1) = zr(ires+ii+1) + zr(ipoids+ipg-1) * fx * zr(ivf+ldec+i-1) * jac
!
            zr(ires+ii+2) = zr(ires+ii+2) + zr(ipoids+ipg-1) * fy * zr(ivf+ldec+i-1) * jac
!
            zr(ires+ii+3) = zr(ires+ii+3) + zr(ipoids+ipg-1) * fz * zr(ivf+ldec+i-1) * jac
!
103      continue
!
101  end do
end subroutine
