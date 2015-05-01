subroutine cbprca(phenom_, load)
!
implicit none
!
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/dismoi.h"
#include "asterfort/lisnnl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: phenom_
    character(len=8), intent(in) :: load
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Keyword = 'EVOL_CHAR'
!
! --------------------------------------------------------------------------------------------------
!
! In  phenom       : phenomenon (MECANIQUE/THERMIQUE/ACOUSTIQUE)
! In  load         : name of load
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: object
    character(len=8), pointer :: p_object(:) => null()
    character(len=8) :: evol_char
    integer :: nb_occ, nb_cham
    character(len=16) :: type_sd
    character(len=13) :: obje_pref
!
! --------------------------------------------------------------------------------------------------
!
    call getvid(' ', 'EVOL_CHAR', scal=evol_char, nbret=nb_occ)
    call lisnnl(phenom_, load, obje_pref)
!
    if (nb_occ .ne. 0) then
!
! ----- Check
!
        ASSERT(nb_occ.eq.1)
        call dismoi('NB_CHAMP_UTI', evol_char, 'RESULTAT', repi=nb_cham)
        if (nb_cham .le. 0) then
            call utmess('F', 'CHARGES3_1', sk=evol_char)
        endif
        call gettco(evol_char, type_sd)
        ASSERT(type_sd .eq. 'EVOL_CHAR')
!
! ----- Save
!
        object = obje_pref(1:13)//'.EVOL.CHAR'
        call wkvect(object, 'G V K8', 1, vk8 = p_object)
        p_object(1) = evol_char

    endif

end subroutine
