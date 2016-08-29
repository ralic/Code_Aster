subroutine rslesd(result     , nume   ,&
                  model_     , materi_, cara_elem_,&
                  list_load_ , iexcit_)
!
implicit none
!
#include "asterfort/utmess.h"
#include "asterfort/rs_get_mate.h"
#include "asterfort/rs_get_model.h"
#include "asterfort/rs_get_caraelem.h"
#include "asterfort/rs_get_listload.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: result
    integer, intent(in) :: nume
    character(len=8), optional, intent(out) :: model_
    character(len=8), optional, intent(out) :: materi_
    character(len=8), optional, intent(out) :: cara_elem_
    character(len=19), optional, intent(out) :: list_load_
    integer, optional, intent(out) :: iexcit_
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Get parameters at index stored in results datastructure or from command file
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! In  nume             : index to find in results datastructure
! Out model            : name of model
! Out materi           : name of material characteristics (field)
! Out cara_elem        : name of elementary characteristics (field)
! Out list_load        : name of datastructure for list of loads when came from results
! Out iexcit           : where loads are coming from ?
!                      0 : From results datastructure
!                      1 : From command file
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iexcit, codret
    character(len=19) :: list_load
    character(len=8) :: model, materi, cara_elem
!
! --------------------------------------------------------------------------------------------------
!
    model     = ' '
    cara_elem = ' '
    materi    = ' '
    list_load = ' '
    iexcit    = 0
!
! - Get model at index stored in results datastructure or from command file
!
    if (present(model_)) then
        call rs_get_model(result, nume, model, codret)
        if (codret .eq. 4) then
            call utmess('A', 'RESULT1_37')
        endif
        model_ = model
    endif
!
! - Get elem. characteristics field at index stored in results datastructure or from command file
!
    if (present(cara_elem_)) then
        call rs_get_caraelem(result, nume, cara_elem, codret)
        if (codret .eq. 4) then
            call utmess('A', 'RESULT1_38')
        endif
        cara_elem_ = cara_elem
    endif
!
! - Get material field at index stored in results datastructure or from command file
!
    if (present(materi_)) then
        call rs_get_mate(result, nume, materi, codret)
        if (codret .eq. 4) then
            call utmess('A', 'RESULT1_39')
        endif
        materi_ = materi
    endif
!
! - Get list of loads at index stored in results datastructure or from command file
!
    if (present(list_load_)) then
        call rs_get_listload(result, nume, list_load, iexcit)
        iexcit_    = iexcit
        list_load_ = list_load
    endif
!
end subroutine
