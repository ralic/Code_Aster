subroutine ntdoth(model     , mate   , cara_elem, list_load, result, &
                  nume_store, matcst_, coecst_  )
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ntdoch.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/rslesd.h"
!
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
!
    character(len=24), intent(out) :: model
    character(len=24), intent(out) :: cara_elem
    character(len=24), intent(out) :: mate
    character(len=19), intent(inout) :: list_load
    character(len=8), optional, intent(in) :: result
    integer, optional, intent(in) :: nume_store
    aster_logical, optional, intent(out) :: matcst_
    aster_logical, optional, intent(out) :: coecst_
!
! --------------------------------------------------------------------------------------------------
!
! Thermics - Read parameters
!
! --------------------------------------------------------------------------------------------------
!
! Out model            : name of model
! Out list_load        : list of loads
! Out mate             : name of material characteristics (field)
! Out cara_elem        : name of datastructure for elementary parameters (CARTE)
! In  result           : name of datastructure for results
! In  nume_store       : index to store in results
! Out matcst           : .true. if constant material parameters
! Out coecst           : .true. if constant rigidity matrix
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: k16bid, nomcmd
    character(len=19) :: list_load_resu
    integer :: i_load, nb_load, iexcit
    character(len=8) :: k8bid, repk, materi
    aster_logical :: l_load_user
    aster_logical :: matcst, coecst
    character(len=24) :: lload_info
    integer, pointer :: v_load_info(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    materi      = ' '
    mate        = ' '
    cara_elem   = ' '
    if (list_load.eq.' ') then
        list_load   = '&&NTDOTH.LISCHA'
    endif
    model       = ' '
    l_load_user = .true.
    coecst      = .true.
    matcst      = .true.
!
    call getres(k8bid, k16bid, nomcmd)
!
    if ((nomcmd.eq.'CALC_CHAMP') .or. (nomcmd.eq.'POST_ELEM')) then
        call rslesd(result        , nume_store, model, materi, cara_elem,&
                    list_load_resu, iexcit)
        l_load_user = iexcit.eq.1
    else
        call getvid(' ', 'MODELE'    , scal=model)
        call getvid(' ', 'CHAM_MATER', scal=materi)
        call getvid(' ', 'CARA_ELEM' , scal=cara_elem)
    endif
!
! - Coding material parameters
!
    if (materi .ne. ' ') then
        call rcmfmc(materi, mate)
    endif
!
! - Get loads information and create datastructure
!
    call ntdoch(list_load, l_load_user, list_load_resu)
!
! - Detect non-constant rigidity matrix
!
    lload_info = list_load(1:19)//'.INFC'
    call jeveuo(lload_info, 'L', vi   = v_load_info)
    nb_load = v_load_info(1)
    coecst  = .true.
    do i_load = 1, nb_load
        if (v_load_info(nb_load+i_load+1).eq.3) then
            coecst = .false.
        endif
    end do
!
! - Detect non-constant material parameters
!
    call dismoi('THER_F_INST', mate, 'CHAM_MATER', repk=repk)
    matcst = repk.eq.'NON'
!
    if (present(coecst_)) then
        coecst_ = coecst
    endif
    if (present(matcst_)) then
        matcst_ = matcst
    endif
!
end subroutine
