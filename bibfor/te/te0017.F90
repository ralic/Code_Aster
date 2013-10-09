subroutine te0017(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: option, nomte
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
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_MECA_FORC_F '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
    integer :: ipoids, ivf, idfde, igeom, itemps, iforc, ier
    integer :: jgano, nno, ndl, kp, npg1, ii, i, l, ivectu, ndim, nnos
    real(kind=8) :: poids, fx, fy, fz
    real(kind=8) :: xx, yy, zz, valpar(4)
    character(len=8) :: nompar(4)
!     ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PFF3D3D', 'L', iforc)
!
    valpar(4) = zr(itemps)
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
    nompar(4) = 'INST'
!
    ndl = 3*nno
    do i = 1, ndl
        zr(ivectu+i-1) = 0.0d0
    end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do kp = 1, npg1
!
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids)
!
        xx = 0.d0
        yy = 0.d0
        zz = 0.d0
        do i = 1, nno
            xx = xx + zr(igeom+3*i-3)*zr(ivf+l+i-1)
            yy = yy + zr(igeom+3*i-2)*zr(ivf+l+i-1)
            zz = zz + zr(igeom+3*i-1)*zr(ivf+l+i-1)
        end do
        valpar(1) = xx
        valpar(2) = yy
        valpar(3) = zz
        call fointe('FM', zk8(iforc ), 4, nompar, valpar,&
                    fx, ier)
        call fointe('FM', zk8(iforc+1), 4, nompar, valpar,&
                    fy, ier)
        call fointe('FM', zk8(iforc+2), 4, nompar, valpar,&
                    fz, ier)
!
        do i = 1, nno
            ii = 3* (i-1)
            zr(ivectu+ii ) = zr(ivectu+ii ) + poids*zr(ivf+l+i-1)*fx
            zr(ivectu+ii+1) = zr(ivectu+ii+1) + poids*zr(ivf+l+i-1)* fy
            zr(ivectu+ii+2) = zr(ivectu+ii+2) + poids*zr(ivf+l+i-1)* fz
!
        end do
!
    end do
!
end subroutine
