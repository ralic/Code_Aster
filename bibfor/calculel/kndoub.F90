subroutine kndoub(long, lkn, nbkn, iret)
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
! A_UTIL
    implicit none
#include "asterfort/assert.h"
#include "asterfort/knindi.h"
    integer :: long, iret, nbkn
    character(len=*) :: lkn(nbkn)
! ---------------------------------------------------------------------
! BUT: VERIFIER QU'IL N'Y A PAS DE DOUBLONS DANS UNE LISTE DE KN
! ---------------------------------------------------------------------
!     ARGUMENTS:
! LONG   IN   I     : 8/16/24 : LONGUEUR DES CHAINES DE LKN
! LKN    IN   V(K*) : LISTE DES KN A VERIFIER
! NBKN   IN   I     : LONGUEUR DE LA LISTE LKN
! IRET   OUT  I     : CODE RETOUR :
!                     /0 : IL N'Y A PAS DE DOUBLONS
!                     /I1>0 : NUMERO DU 1ER DOUBLON DANS LKN
!----------------------------------------------------------------------
    integer :: k1, k2
! DEB
!
    ASSERT((long.eq.8).or.(long.eq.16).or.(long.eq.24))
!
    iret = 0
!
    do 10,k1 = 1,nbkn - 1
    k2 = knindi(long,lkn(k1),lkn(k1+1),nbkn-k1)
    if (k2 .gt. 0) then
        iret = k1
        goto 20
    endif
    10 end do
!
!
!
20  continue
!
!
end subroutine
