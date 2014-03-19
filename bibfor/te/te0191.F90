subroutine te0191(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/vecma.h"
!
    character(len=16) :: option, nomte
! ......................................................................
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    - FONCTION REALISEE:  CALCUL DES MATRICES DE MASSE ELEMENTAIRES
!                          POUR LES ELEMENTS DE FOURIER
!                          OPTION : 'MASS_MECA       '
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    character(len=8) :: fami, poum
    character(len=16) :: phenom
    integer :: icodre(1)
    real(kind=8) :: a(3, 3, 9, 9), poids, r, rho(1)
    real(kind=8) :: matp(27, 27), matv(378)
    integer :: nno, kp, nnos, npg2, i, j, k, l, imatuu, nddl, nvec, iacce, ivect
    integer :: ipoids, ivf, idfde, igeom, imate, ijkl, ik, kpg, spt
    integer :: ndim, jgano
! ......................................................................
!
    call elrefe_info(fami='MASS',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    nddl = 3 * nno
    nvec = nddl * ( nddl + 1 ) / 2
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', phenom, 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre(1), 1)
!
    do k = 1, 3
        do l = 1, 3
            do i = 1, nno
                do j = 1, i
                    a(k,l,i,j) = 0.0d0
                end do
            end do
        end do
    end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do kp = 1, npg2
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids)
!
        r = 0.0d0
        do i = 1, nno
            r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
        end do
        poids = poids*r*rho(1)
!
        do i = 1, nno
            do j = 1, i
                a(1,1,i,j) = a(1,1,i,j) + poids * zr(ivf+k+i-1) * zr( ivf+k+j-1)
            end do
        end do
    end do
!
    do i = 1, nno
        do j = 1, i
            a(2,2,i,j) = a(1,1,i,j)
            a(3,3,i,j) = a(1,1,i,j)
        end do
    end do
!
    if (option .eq. 'MASS_MECA') then
!
        call jevech('PMATUUR', 'E', imatuu)
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
        do k = 1, 3
            do l = 1, 3
                do i = 1, nno
                    ik = ((3*i+k-4) * (3*i+k-3)) / 2
                    do j = 1, i
                        ijkl = ik + 3 * (j-1) + l
                        zr(imatuu+ijkl-1) = a(k,l,i,j)
                    end do
                end do
            end do
        end do
!
    else if (option .eq. 'M_GAMMA') then
!
        call jevech('PACCELR', 'L', iacce)
        call jevech('PVECTUR', 'E', ivect)
        do k = 1, nvec
            matv(k) = 0.0d0
        end do
        do k = 1, 3
            do l = 1, 3
                do i = 1, nno
                    ik = ((3*i+k-4) * (3*i+k-3)) / 2
                    do j = 1, i
                        ijkl = ik + 3 * (j-1) + l
                        matv(ijkl) = a(k,l,i,j)
                    end do
                end do
            end do
        end do
        call vecma(matv, nvec, matp, nddl)
        call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
!
    else
!C OPTION DE CALCUL INVALIDE
        ASSERT(.false.)
    endif
!
end subroutine
