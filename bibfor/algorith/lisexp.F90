subroutine lisexp(list_char)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lisnch.h"
#include "asterfort/utmess.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: list_char
!
! --------------------------------------------------------------------------------------------------
!
! LISTE_CHARGES
!
! Exclude some loads with PILOTAGE
!
! --------------------------------------------------------------------------------------------------
!
!
! In  list_char : name of <LISTE_CHARGES> datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_excl_char
    parameter    (nb_excl_char=6)
    character(len=6) :: ligr_excl_char(nb_excl_char)
!
    character(len=24) :: lich_char, lich_info
    integer :: j_lich_char, j_lich_info
    integer :: ichar, iexcl, nb_char, iret
    character(len=19) :: lchin
    character(len=8) :: char_name
!
    data ligr_excl_char  /'.ROTAT', '.FL1', '.FELEC', '.EPSIN',&
     &                    '.ONDPL', '.SIINT'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Access
!
    lich_char = list_char(1:19)//'.LCHA'
    lich_info = list_char(1:19)//'.INFC'
    call jeveuo(lich_char, 'L', j_lich_char)
    call jeveuo(lich_info, 'L', j_lich_info)
!
! - Number of loads
!
    call lisnch(list_char, nb_char)
!
! - Loop on loads
!
    do ichar = 1, nb_char
        char_name = zk24(j_lich_char-1+ichar)(1:8)
        if (zi(j_lich_info+nb_char+ichar) .eq. 5) then
            do iexcl = 1, nb_excl_char
                lchin = char_name(1:8)//'.CHME.LIGRE'//ligr_excl_char(iexcl)
                call jeexin(lchin, iret)
                if (iret .ne. 0) then
                    call utmess('F', 'CHARGES_26', sk=char_name)
                endif
            enddo
        endif
    end do
!
    call jedema()
end subroutine
