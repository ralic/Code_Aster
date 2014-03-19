subroutine te0308(option, nomte)
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
!
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'RESI_THER_COEH_F'
!                          ELEMENTS 3D
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
    character(len=8) :: nompar(4)
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac, tpg, theta
    real(kind=8) :: valpar(4)
    integer :: ipoids, ivf, idfdx, idfdy, igeom
    integer :: ndim, nno, ipg, npg1, iveres, iech
    integer :: idec, jdec, kdec, ldec, nnos, jgano
!-----------------------------------------------------------------------
    integer :: i, ier, ino, itemp, itemps, j, jno
!
    real(kind=8) :: echnp1, xx, yy, zz
!-----------------------------------------------------------------------
    data               nompar/'X','Y','Z','INST'/
! DEB ------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCOEFHF', 'L', iech)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPEI', 'L', itemp)
    call jevech('PRESIDU', 'E', iveres)
!
    theta = zr(itemps+2)
!
!    CALCUL DES PRODUITS VECTORIELS OMI   OMJ
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
    do 101 ipg = 1, npg1
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx = nx + zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sx(i,j)
                ny = ny + zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sy(i,j)
                nz = nz + zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sz(i,j)
102          continue
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
        tpg = 0.d0
        xx = 0.d0
        yy = 0.d0
        zz = 0.d0
        do 104 i = 1, nno
            tpg = tpg + zr(itemp+i-1) * zr(ivf+ldec+i-1)
            xx = xx + zr(igeom+3*i-3) * zr(ivf+ldec+i-1)
            yy = yy + zr(igeom+3*i-2) * zr(ivf+ldec+i-1)
            zz = zz + zr(igeom+3*i-1) * zr(ivf+ldec+i-1)
104      continue
        valpar(1) = xx
        valpar(2) = yy
        valpar(3) = zz
        valpar(4) = zr(itemps)
        call fointe('FM', zk8(iech), 4, nompar, valpar,&
                    echnp1, ier)
!CDIR$ IVDEP
        do 103 i = 1, nno
            zr(iveres+i-1) = zr(iveres+i-1) - jac* theta* zr(ipoids+ ipg-1)* zr(ivf+ldec+i-1)* ec&
                             &hnp1 * tpg
103      continue
!
101  end do
! FIN ------------------------------------------------------------------
end subroutine
