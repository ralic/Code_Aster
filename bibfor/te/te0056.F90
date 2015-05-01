subroutine te0056(option, nomte)
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
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: option, nomte
!
!     BUT: CALCUL DU SECOND MEMBRE ELEMENTAIRE EN THERMIQUE CORRESPON-
!          DANT A UNE SOURCE VOLUMIQUE FONCTION
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_THER_SOUR_F'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    character(len=8) :: nompar(4)
    real(kind=8) :: poids, sourc, theta
    real(kind=8) :: valpar(4), xx, yy, zz
    integer :: ipoids, ivf, idfde, igeom
    integer :: jgano, nno, kp, npg1, i, ivectt, isour, itemps
!
!
!-----------------------------------------------------------------------
    integer :: ier, l, ndim, nnos
    real(kind=8) :: soun, sounp1
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PSOURCF', 'L', isour)
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
        zr(ivectt-1+i) = 0.0d0
    end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do kp = 1, npg1
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids)
!
!    CALCUL DE SOURC
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
        valpar(4) = zr(itemps)
        call fointe('FM', zk8(isour), 4, nompar, valpar,&
                    sounp1, ier)
        if (theta .ne. 1.0d0) then
            valpar(4) = zr(itemps) - zr(itemps+1)
            call fointe('FM', zk8(isour), 4, nompar, valpar,&
                        soun, ier)
        else
            soun = 0.d0
        endif
        sourc = theta*sounp1 + (1.0d0-theta)*soun
!
        do i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + poids*sourc*zr(ivf+l+i- 1)
        end do
!
    end do
!
end subroutine
