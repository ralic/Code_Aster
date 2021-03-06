subroutine te0058(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: option, nomte
!.......................................................................
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN THERMIQUE
!          CORRESPONDANT AU FLUX IMPOSE (FONCTION)
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_THER_FLUN_F'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    character(len=8) :: nompar(4)
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac, theta
    real(kind=8) :: valpar(4), xx, yy, zz, flux
    integer :: ipoids, ivf, idfdx, idfdy, igeom, itemps
    integer :: ndim, nno, ipg, npg1, ivectt, iflux, idec, jdec, kdec
    integer :: ldec, nnos, jgano
!
!-----------------------------------------------------------------------
    integer :: i, ier, ino, j, jno
    real(kind=8) :: flun, flunp1
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg1,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfdx, jgano=jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PFLUXNF', 'L', iflux)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PVECTTR', 'E', ivectt)
!
    theta = zr(itemps+2)
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
    nompar(4) = 'INST'
!
    do i = 1, nno
        zr(ivectt+i-1) = 0.0d0
    end do
!
!    CALCUL DES PRODUITS VECTORIELS OMI X OMJ
!
    do ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
        end do
    end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do ipg = 1, npg1
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
!    CALCUL DE FLUN
!
        xx = 0.d0
        yy = 0.d0
        zz = 0.d0
        do i = 1, nno
            xx = xx + zr(igeom+3*i-3) * zr(ivf+ldec+i-1)
            yy = yy + zr(igeom+3*i-2) * zr(ivf+ldec+i-1)
            zz = zz + zr(igeom+3*i-1) * zr(ivf+ldec+i-1)
        end do
        valpar(1) = xx
        valpar(2) = yy
        valpar(3) = zz
        valpar(4) = zr(itemps)
        call fointe('FM', zk8(iflux), 4, nompar, valpar,&
                    flunp1, ier)
        if (theta .ne. 1.0d0) then
            valpar(4) = zr(itemps)-zr(itemps+1)
            call fointe('FM', zk8(iflux), 4, nompar, valpar,&
                        flun, ier)
        else
            flun = 0.0d0
        endif
        flux = theta*flunp1 + (1.0d0-theta)*flun
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!   CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
!
        do i = 1, nno
            idec = (i-1)*ndim
            do j = 1, nno
                jdec = (j-1)*ndim
                nx = nx + zr(idfdx+kdec+idec) *zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) *zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) *zr(idfdy+kdec+jdec) * sz(i,j)
            end do
        end do
!
!   CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
        do i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + jac * zr(ipoids+ipg-1) * flux * zr(ivf+ldec+i-1)
        end do
!
    end do
end subroutine
