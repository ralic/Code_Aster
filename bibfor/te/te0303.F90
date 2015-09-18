subroutine te0303(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: option, nomte
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
!     BUT: CALCUL DES MATRICES ELEMENTAIRES EN THERMIQUE
!          CORRESPONDANT AU TERME D'ECHANGE ENTRE 2 PAROIS (FACE)
!          D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'RIGI_THER_PARO_R'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
    integer :: ipoids, ivf, idfdx, idfdy, igeom, i, j
    integer :: ndim, nno, ipg, npg1, ivectt, ihechp, ino, jno, nnos, jgano
    integer :: idec, jdec, kdec, ldec, itemps, itemp
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9)
    real(kind=8) :: jac, tem, theta, h
!     ------------------------------------------------------------------
!
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos,&
                     npg=npg1, jpoids=ipoids, jvf=ivf, jdfde=idfdx, jgano=jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PHECHPR', 'L', ihechp)
    h = zr(ihechp)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PVECTTR', 'E', ivectt)
!
    theta = zr(itemps+2)
    do i = 1, 2*nno
        zr(ivectt+i-1) = 0.0d0
    end do
!
!    CALCUL DES PRODUITS VECTORIELS OMI * OMJ
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
!
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
!
            end do
        end do
!
! --- CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
        tem = 0.d0
        do i = 1, nno
            ldec = (ipg-1)*nno
            tem = tem + (zr(itemp+nno+i-1)- zr(itemp+i-1) ) * zr(ivf+ ldec+i-1)
        end do
        do i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + jac * h * zr(ipoids+ipg- 1) * zr(ivf+ldec+i-1) * (1&
                             &.0d0-theta)*tem
            zr(ivectt+nno+i-1) = zr(ivectt+nno+i-1) - jac * h * zr(ipoids+ipg-1) * zr(ivf+ldec+i-&
                                 &1) * (1.0d0-theta)*tem
        end do
    end do
end subroutine
