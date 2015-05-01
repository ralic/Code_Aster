subroutine te0130(option, nomte)
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
!     BUT: CALCUL DES MATRICES TANGENTES ELEMENTAIRES EN THERMIQUE
!          CORRESPONDANT AU TERME D'ECHANGE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'MTAN_THER_COEF_R'
!          OPTION : 'MTAN_THER_RAYO_R'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterc/r8t0.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: nomte, option
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac, theta
    integer :: ipoids, ivf, idfdx, idfdy, igeom
    integer :: ndim, nno, ndi, ipg, npg2, imattt, iech, iray
    integer :: itemp, idec, jdec, kdec, ldec, nnos
    real(kind=8) :: hech, sigma, epsil, tpg, tz0
!
!-----------------------------------------------------------------------
    integer :: i, ij, ino, itemps, j, jgano, jno
!
!-----------------------------------------------------------------------
    tz0 = r8t0()
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg2,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfdx, jgano=jgano)
    idfdy = idfdx + 1
    ndi = nno*(nno+1)/2
!
    if (option(11:14) .eq. 'COEF') then
        call jevech('PCOEFHR', 'L', iech)
        hech = zr(iech)
    else if (option(11:14).eq.'RAYO') then
        call jevech('PRAYONR', 'L', iray)
        call jevech('PTEMPEI', 'L', itemp)
        sigma = zr(iray)
        epsil = zr(iray+1)
    endif
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PMATTTR', 'E', imattt)
!
    theta = zr(itemps+2)
!
    do i = 1, ndi
        zr(imattt+i-1) = 0.0d0
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
    do ipg = 1, npg2
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
!   CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
        if (option(11:14) .eq. 'COEF') then
            do i = 1, nno
                do j = 1, i
                    ij = (i-1)*i/2 + j
!
                    zr(imattt+ij-1) = zr(imattt+ij-1) + jac * theta * zr(ipoids+ipg-1) * hech * z&
                                      &r(ivf+ldec+i-1) * zr( ivf+ldec+j-1)
!
                end do
            end do
        else if (option(11:14).eq.'RAYO') then
            tpg = 0.d0
            do i = 1, nno
                tpg = tpg + zr(itemp+i-1) * zr(ivf+ldec+i-1)
            end do
            do i = 1, nno
                do j = 1, i
                    ij = (i-1)*i/2 + j
!
                    zr(imattt+ij-1) = zr(imattt+ij-1) + jac * theta * zr(ivf+ldec+i-1) * zr(ivf+l&
                                      &dec+j-1) * zr(ipoids+ ipg-1) * 4.d0 * sigma * epsil * (tpg&
                                      &+tz0)**3
!
                end do
            end do
        endif
!
    end do
end subroutine
