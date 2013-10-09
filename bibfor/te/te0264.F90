subroutine te0264(option, nomte)
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
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'CHAR_THER_SOUR_F'
!                          ELEMENTS FOURIER
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: icode, nbres
    real(kind=8) :: soun, sounp1, theta
!-----------------------------------------------------------------------
    parameter         ( nbres=3 )
    character(len=8) :: nompar(nbres)
    real(kind=8) :: valpar(nbres)
    real(kind=8) :: poids, r, z, sour
    integer :: nno, kp, npg1, i, k, itemps, ivectt, isour, nnos, jgano
    integer :: ipoids, ivf, idfde, igeom, ndim
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PSOURCF', 'L', isour)
    call jevech('PVECTTR', 'E', ivectt)
    theta = zr(itemps+2)
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'INST'
!
    do kp = 1, npg1
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids)
        r = 0.d0
        z = 0.d0
        do i = 1, nno
            r = r + zr(igeom+2*(i-1) )*zr(ivf+k+i-1)
            z = z + zr(igeom+2*(i-1)+1)*zr(ivf+k+i-1)
        end do
        poids = poids*r
        valpar(1) = r
        valpar(2) = z
        valpar(3) = zr(itemps)
        call fointe('FM', zk8(isour), 3, nompar, valpar,&
                    sounp1, icode)
        valpar(3) = zr(itemps)-zr(itemps+1)
        call fointe('FM', zk8(isour), 3, nompar, valpar,&
                    soun, icode)
        sour = theta*sounp1 + (1.0d0-theta)*soun
!CDIR$ IVDEP
        do i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + poids * zr(ivf+k+i-1) * sour
        end do
    end do
end subroutine
