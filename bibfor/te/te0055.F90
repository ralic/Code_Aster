subroutine te0055(option, nomte)
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
!     BUT: CALCUL DU SECOND MEMBRE ELEMENTAIRE EN THERMIQUE CORRESPON-
!          DANT A UNE SOURCE VOLUMIQUE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_THER_SOUR_R '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: nomte, option
    real(kind=8) :: poids
    integer :: ipoids, ivf, idfde, igeom
    integer :: jgano, nno, kp, npg1, i, ivectt, isour
!
!
!
!-----------------------------------------------------------------------
    integer :: l, ndim, nnos
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PSOURCR', 'L', isour)
    call jevech('PVECTTR', 'E', ivectt)
!
    do i = 1, nno
        zr(ivectt-1+i) = 0.0d0
    end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do kp = 1, npg1
!
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids)
        do i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + poids*zr(isour-1+kp)*zr( ivf+l+i-1)
        end do
!
    end do
!
end subroutine
