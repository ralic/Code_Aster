subroutine optdep(option, lisopt, nopout)
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
    implicit none
!     --- ARGUMENTS IN ---
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/ccliop.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
    character(len=16) :: option
!     --- ARGUMENTS OUT ---
    integer :: nopout
    character(len=24) :: lisopt(*)
!
    character(len=24) :: noliop
    character(len=8) :: temp
    integer :: i, jlisop
!
    temp = '&&OPTDEP'
    call ccliop('CHAMP', option, temp, noliop, nopout)
    if (nopout .eq. 0) goto 9999
    call assert(nopout.le.100)
!
    call jeveuo(noliop, 'L', jlisop)
    do 10 i = 1, nopout
        lisopt(i) = zk24(jlisop-1+i)
10  end do
!
9999  continue
    call jedetr(temp//'.LISOPT')
    call jedetr(temp//'.LISORI')
    call jedetr(temp//'.LISDEP')
    call jedetr(temp//'.LNOINS')
    call jedetr(temp//'.ISODEP')
end subroutine
