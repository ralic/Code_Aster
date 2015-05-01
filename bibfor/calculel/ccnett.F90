subroutine ccnett(nobase, nopout)
    implicit none
!     --- ARGUMENTS ---
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
    integer :: nopout
    character(len=8) :: nobase
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!  CALC_CHAMP - NETTOYAGE
!  -    -       ----
! ----------------------------------------------------------------------
!
! IN  :
!   NOBASE  K8   BASE DU NOM A PARTIR DE LAQUELLE LE NOM DES OBJETS DE
!                CCLIOP ONT ETE CONSTRUITS
!   NOPOUT  I    TAILLE DE LA LISTE OUT
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
!
    integer :: iop
!
    character(len=5) :: numopt
    character(len=11) :: nobaop
    character(len=24) :: noliop, nolori, noldep, noliin, lisins, nolisd
!
    call jemarq()
!
    noliop = nobase//'.LISOPT'
    nolori = nobase//'.LISORI'
    noldep = nobase//'.LISDEP'
    noliin = nobase//'.LNOINS'
    nolisd = nobase//'.ISODEP'
!
    nobaop = nobase//'.OP'
!
    do 30 iop = 1, nopout
        call codent(iop, 'D0', numopt)
        lisins = nobaop//numopt
        call jedetr(lisins)
30  end do
!
    call jedetr(noliop)
    call jedetr(nolori)
    call jedetr(noldep)
    call jedetr(noliin)
    call jedetr(nolisd)
!
    call jedema()
!
end subroutine
