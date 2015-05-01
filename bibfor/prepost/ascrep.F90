subroutine ascrep(mailla, ltp1)
    implicit   none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: mailla
    real(kind=8) :: ltp1
!-----------------------------------------------------------------------
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
!     OPERATEUR: "MODI_MAILLAGE" , MOTCLE FACTEUR "PLAQ_TUBE"
!
!     CHANGEMENT DE REPERE : PLAQUE --> UTILISATEUR
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: nbno, icoor, idime, ino, ndim
    real(kind=8) :: xp, yp, zp
    character(len=24) :: coord, dime
!     ------------------------------------------------------------------
!
    call jemarq()
!
    coord = mailla//'.COORDO    .VALE'
    dime = mailla//'.DIME           '
!
    call jeveuo(coord, 'E', icoor)
    call jeveuo(dime, 'E', idime)
    nbno = zi(idime)
    ndim = zi(idime+5)
!
    do 100 ino = 1, nbno
        xp = zr(icoor+ndim*(ino-1))
        yp = zr(icoor+ndim*(ino-1)+1)
        zp = zr(icoor+ndim*(ino-1)+2)
        zr(icoor+ndim*(ino-1)) = - yp
        zr(icoor+ndim*(ino-1)+1) = - xp
        zr(icoor+ndim*(ino-1)+2) = - (zp + ltp1)
100  end do
!
    call jedema()
!
end subroutine
