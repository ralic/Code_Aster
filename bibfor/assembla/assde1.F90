subroutine assde1(tych, champ)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/chlici.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/assert.h"
    character(len=*), intent(in) :: champ
    character(len=*), intent(in) :: tych
! ----------------------------------------------------------------------
!     IN:
!       NOMU   : NOM D'UN CONCEPT DE TYPE
!                CHAMP / CHAM_NO, CHAM_ELEM, CARTE ou RESUELEM (K19)
!
!     RESULTAT:
!     ON DETRUIT TOUS LES OBJETS JEVEUX CORRESPONDANT A CE CONCEPT.
! ----------------------------------------------------------------------
!
!
    character(len=19) :: champ2
    aster_logical :: dbg
! -DEB------------------------------------------------------------------
    champ2 = champ
!
    dbg=.true.
    dbg=.false.
    if (dbg) call chlici(champ2, 19)
!
!
!
!   -- POUR LES CARTE, CHAM_NO, CHAM_ELEM, ET RESU_ELEM :
    if (tych .eq. 'CHAM_ELEM') then
        call jedetr(champ2//'.CELD')
        call jedetr(champ2//'.CELV')
        call jedetr(champ2//'.CELK')
!
    else if (tych.eq.'CHAM_NO') then
        call jedetr(champ2//'.VALE')
        call jedetr(champ2//'.REFE')
        call jedetr(champ2//'.DESC')
!
    else if (tych.eq.'CARTE') then
        call jedetr(champ2//'.DESC')
        call jedetr(champ2//'.NOMA')
        call jedetr(champ2//'.VALE')
        call jedetr(champ2//'.NOLI')
        call jedetr(champ2//'.LIMA')
        call jedetr(champ2//'.VALV')
        call jedetr(champ2//'.NCMP')
        call jedetr(champ2//'.PTMA')
        call jedetr(champ2//'.PTMS')
!
    else if (tych.eq.'RESUELEM') then
        call jedetr(champ2//'.NOLI')
        call jedetr(champ2//'.DESC')
        call jedetr(champ2//'.RESL')
        call jedetr(champ2//'.RSVI')
!
    else if (tych.eq.'CHAMP') then
        call jedetr(champ2//'.CELD')
        call jedetr(champ2//'.CELK')
        call jedetr(champ2//'.CELV')
        call jedetr(champ2//'.DESC')
        call jedetr(champ2//'.LIMA')
        call jedetr(champ2//'.NCMP')
        call jedetr(champ2//'.NOLI')
        call jedetr(champ2//'.NOMA')
        call jedetr(champ2//'.PTMA')
        call jedetr(champ2//'.PTMS')
        call jedetr(champ2//'.REFE')
        call jedetr(champ2//'.RESL')
        call jedetr(champ2//'.RSVI')
        call jedetr(champ2//'.VALE')
        call jedetr(champ2//'.VALV')
    else
        ASSERT(.false.)
    endif
!
!
!
end subroutine
