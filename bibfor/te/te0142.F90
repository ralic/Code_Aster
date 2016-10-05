subroutine te0142(option, nomte)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
!
    integer :: nbcomp
    parameter (nbcomp=3)
    integer :: igeom, i, j
    integer :: ndim, npg1, kpg, spt
    integer :: imate, ival
    integer :: mater, nnos
    real(kind=8) :: valres(nbcomp)
    integer :: icodre(nbcomp), ndim2
    character(len=8) :: fami, poum
    character(len=16) :: nomres(nbcomp)
    character(len=8) :: nompar(3)
    real(kind=8) :: valpar(3)
!     ------------------------------------------------------------------
!
    if (option .eq. 'MATE_ELGA')then
        fami = 'RIGI'
    elseif (option .eq. 'MATE_ELEM')then
        fami = 'FPG1'
    else
        ASSERT(.false.)
    endif
    call elrefe_info(fami=fami,ndim=ndim,nnos=nnos, npg=npg1)
!
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATERR', 'E', ival)
!
    mater=zi(imate)
    nomres(1)='E'
    nomres(2)='NU'
    nomres(3) = 'RHO'
    spt=1
    poum='+'

    if (lteatt('ABSO','OUI')) then
        fami='FPG1'
        kpg=1
!
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        ndim2 = ndim + 1
!   coordonnées du barycentre de l'élément
        call jevech('PGEOMER', 'L', igeom)
        valpar(:) = 0.d0
        do i = 1, nnos
            do j = 1, ndim2
                valpar(j) = valpar(j) + zr(igeom-1+(i-1)*ndim2+j)/nnos
            enddo
        enddo
!
        call rcvalb(fami, kpg, spt, poum, mater,&
                        ' ', 'ELAS', ndim2, nompar, valpar,&
                        3, nomres, valres, icodre, 1)
        do kpg = 1, npg1
            zr(ival-1+(kpg-1)*nbcomp+1) = valres(1)
            zr(ival-1+(kpg-1)*nbcomp+2) = valres(2)
            zr(ival-1+(kpg-1)*nbcomp+3) = valres(3)
        enddo
    else
        do kpg = 1, npg1
            call rcvalb(fami, kpg, spt, poum, mater,&
                        ' ', 'ELAS', 0, ' ', [0.d0],&
                        3, nomres, valres, icodre, 1)
            zr(ival-1+(kpg-1)*nbcomp+1) = valres(1)
            zr(ival-1+(kpg-1)*nbcomp+2) = valres(2)
            zr(ival-1+(kpg-1)*nbcomp+3) = valres(3)
        enddo
    endif
!
end subroutine
