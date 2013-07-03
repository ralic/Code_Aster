subroutine te0328(option, nomte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.......................................................................
    implicit none
!          ELEMENTS ISOPARAMETRIQUES 3D ET 2D
!    FONCTION REALISEE:
!            OPTION : 'VERIF_JACOBIEN'
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
!
#include "asterfort/dfdm2j.h"
#include "asterfort/dfdm3j.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/tecael.h"
#include "asterfort/u2mesk.h"
    character(len=16) :: nomte, option
!
    logical :: posi, nega
    character(len=24) :: valk(2)
    real(kind=8) :: poids
    integer :: igeom, ipoids, ivf, idfde, ndim, npg, nno, jgano, nnos
    integer :: kp, icodr
    integer :: iadzi, iazk24, codret
!
    codret=0
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCODRET', 'E', icodr)
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    posi=.false.
    nega=.false.
    do 1 kp = 1, npg
        if (ndim .eq. 3) then
            call dfdm3j(nno, kp, idfde, zr(igeom), poids)
        else if (ndim.eq.2) then
            call dfdm2j(nno, kp, idfde, zr(igeom), poids)
        else
            goto 9999
        endif
        if (poids .lt. 0.d0) nega=.true.
        if (poids .gt. 0.d0) posi=.true.
 1  end do
!
!
    if (posi .and. nega) then
        call tecael(iadzi, iazk24)
        nno=zi(iadzi-1+2)
        valk(1)=zk24(iazk24-1+3)
        valk(2)=zk24(iazk24-1+3+nno+1)
        call u2mesk('A', 'CALCULEL_7', 2, valk)
        codret=1
    endif
!
9999  continue
    zi(icodr-1+1)=codret
!
end subroutine
