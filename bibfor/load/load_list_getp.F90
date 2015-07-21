subroutine load_list_getp(phenom      , l_load_user, v_llresu_info, v_llresu_name, v_list_dble,&
                          l_apply_user, i_load     , nb_load      , i_excit      , load_name  ,&
                          load_type   , ligrch     , load_apply   )
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=4), intent(in) :: phenom
    aster_logical, intent(in) :: l_load_user
    character(len=8), pointer, intent(in) :: v_list_dble(:)
    integer, intent(in), pointer :: v_llresu_info(:)
    character(len=24), intent(in), pointer :: v_llresu_name(:)
    integer, intent(in) :: i_load
    integer, intent(in) :: nb_load
    aster_logical, intent(in) :: l_apply_user
    integer, intent(inout) :: i_excit
    character(len=8), intent(out) :: load_name
    character(len=8), intent(out) :: load_type
    character(len=19), intent(out) :: ligrch
    character(len=8), optional, intent(out) :: load_apply
!
! --------------------------------------------------------------------------------------------------
!
! List of loads - Utility
!
! Get parameters for construct list of loads
!
! --------------------------------------------------------------------------------------------------
!
! In  phenom         : phenomenon (MECA/THER/ACOU)
! In  l_load_user    : .true. if loads come from user (EXCIT)
! In  v_llresu_info  : pointer for loads infos for list of loads from result datastructure
! In  v_llresu_name  : pointer for loads names for list of loads from result datastructure
! In  v_list_dble    : pointer to list of loads (to avoid same loads)
! In  l_apply_user   : .true. if applying load option by user
! In  i_load         : index in list
! In  nb_load        : number of loads for list of loads
! IO  i_excit        : index in EXCIT factor keyword
! Out load_name      : name of load
! Out load_type      : type of load
! Out load_apply     : how to apply load
! Out ligrch         : LIGREL (list of elements) where apply load
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywf, load_pheno
    integer :: i_load_dble, nocc
!
! --------------------------------------------------------------------------------------------------
!
    load_name = ' '
    keywf     = 'EXCIT'
!
    if (l_load_user) then
        i_excit = i_excit + 1
30      continue
        call getvid(keywf, 'CHARGE', iocc=i_excit, nbval=0, nbret=nocc)
        if (nocc .eq. 0) then
            i_excit = i_excit + 1
            goto 30
        else
            call getvid(keywf, 'CHARGE', iocc=i_excit, scal=load_name)
            do i_load_dble = 1, nb_load
                if (load_name .eq. v_list_dble(i_load_dble)) then
                    call utmess('F', 'CHARGES_1', sk=load_name)
                endif
            end do
        endif
    else
        load_name = v_llresu_name(i_load)(1:8)
    endif
!
! - Save load name
!
    v_list_dble(i_load) = load_name
!
! - Get LIGREL
!
    if (phenom.eq.'MECA') then
        ligrch = load_name//'.CHME.LIGRE'
    elseif (phenom.eq.'THER') then
        ligrch = load_name//'.CHTH.LIGRE'
    else
        ASSERT(.false.)
    endif
!
! - Type of load
!
    call dismoi('TYPE_CHARGE', load_name, 'CHARGE', repk=load_type)
!
! - Check phenomenon
!
    call dismoi('PHENOMENE'  , load_name, 'CHARGE', repk=load_pheno)
    if (phenom.eq.'MECA') then
        if (load_pheno.ne.'MECANIQUE') then
            call utmess('F', 'CHARGES_22', sk=load_name)
        endif
    elseif (phenom.eq.'THER') then        
        if (load_pheno.ne.'THERMIQUE') then
            call utmess('F', 'CHARGES_21', sk=load_name)
        endif
    else
        ASSERT(.false.)
    endif
!
! - Type of applying load
!
    if (phenom.eq.'MECA') then
        ASSERT(present(load_apply))
        if (l_load_user) then
            if (l_apply_user) then
                call getvtx(keywf, 'TYPE_CHARGE', iocc=i_excit, scal=load_apply)
            else
                load_apply = 'FIXE'
            endif
        else
            if (l_apply_user) then
                load_apply = 'FIXE_CST'
            else
                if (v_llresu_info(i_load+1) .eq. 4 .or.&
                    v_llresu_info(1+nb_load+i_load) .eq. 4) then
                    load_apply = 'SUIV'
                elseif (v_llresu_info(i_load+1).eq.5 .or.&
                        v_llresu_info(1+nb_load+i_load).eq.5) then
                    load_apply = 'FIXE_PIL'
                else if (v_llresu_info(1+3*nb_load+2+i_load).eq.1) then
                    load_apply = 'DIDI'
                endif
            endif
        endif
    elseif (phenom.eq.'THER') then
        ASSERT(.not.present(load_apply))        
    else
        ASSERT(.false.)
    endif
!
end subroutine
