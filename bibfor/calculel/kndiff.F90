subroutine kndiff(long, lk1, l1, lk2, l2,&
                  lk3, l3)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/kndif2.h"
#include "asterfort/knindi.h"
    integer :: long, l1, l2, l3
    character(len=*) :: lk1(l1), lk2(l2), lk3(l3)
! ---------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!
! BUT: DIFFERENCE ENTRE 2 LISTES   LK3 = LK1 - LK2
! ---------------------------------------------------------------------
!     ARGUMENTS:
! LONG   IN   I     : 8,16 OU 24 : LONGUEUR DES CHAINES DE LK1 ET LK2
! LK1    IN   V(K*) : LISTE DE K*
! L1     IN   I     : LONGUEUR DE LA LISTE LK1
! LK2    IN   V(K*) : LISTE DES K*
! L2     IN   I     : LONGUEUR DE LA LISTE LK2
! LK3    OUT  V(K*) : LISTE DES K* QUI DOIT CONTENIR LK1 - LK2
! L3     IN   I     : DIMENSION DU TABLEAU LK3
! L3     OUT  I     : LONGUEUR DE LA LISTE LK3
!----------------------------------------------------------------------
    integer :: k1, k2, nbk3
! DEB
!
    call assert((long.eq.8).or.(long.eq.16).or.(long.eq.24))
!
    nbk3=l3
    l3 = 0
    if (l1 .eq. 0) goto 9999
!
    if (l2 .eq. 0) then
        l3=l1
        do 20, k1 = 1 , l1
        lk3(k1)=lk1(k1)
20      continue
        goto 9999
    endif
!
!
!     -- SI LES LISTES SONT LONGUES, ON UTILISE KNDIF2 :
!     --------------------------------------------------
    if (dble(l1*l2) .gt. 1.d4) then
        call kndif2(long, lk1, l1, lk2, l2,&
                    lk3, nbk3)
        l3=nbk3
        goto 9999
    endif
!
!
    do 10, k1 = 1 , l1
!          -- ON VERIFIE QUE LK1(K1) SE TROUVE DANS LK2 :
    k2 = knindi ( long, lk1(k1), lk2, l2 )
    if (k2 .eq. 0) then
        l3 = l3 + 1
        call assert(l3.le.nbk3)
        lk3(l3) = lk1(k1)
    endif
!
    10 end do
9999  continue
end subroutine
