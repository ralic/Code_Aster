subroutine ntdoch(list_load, l_load_user_, list_load_resu)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/focste.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/liscad.h"
#include "asterfort/lisccr.h"
#include "asterfort/jeexin.h"
#include "asterfort/load_list_getp.h"
#include "asterfort/jeveuo.h"
#include "asterfort/load_neut_iden.h"
#include "asterfort/load_neut_data.h"
#include "asterfort/utmess.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
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
    character(len=19), intent(in) :: list_load
    aster_logical, optional, intent(in) :: l_load_user_
    character(len=19), optional, intent(in) :: list_load_resu
!
! --------------------------------------------------------------------------------------------------
!
! Thermics - Read parameters
!
! Get loads information and create datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  list_load_resu : name of datastructure for list of loads from result datastructure
! In  l_load_user    : .true. if loads come from user (EXCIT)
! In  list_load      : name of datastructure for list of loads
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_info_maxi
    parameter   (nb_info_maxi=99)
    character(len=24) :: list_info_type(nb_info_maxi)
!
    integer :: nb_type_neum
    parameter   (nb_type_neum = 10)
    aster_logical :: list_load_keyw(nb_type_neum)
!
    aster_logical :: l_func_mult, l_load_user, l_apply_user
    integer :: nb_info_type
    character(len=24) :: info_type
    integer :: nb_load, n1, i_load, i_type_neum, iret, i_excit
    character(len=24) :: ligrch, const_func
    character(len=10) :: load_obje(2)
    character(len=19) :: cart_name
    character(len=8) :: load_name
    character(len=24) :: load_type, load_para, load_func, load_keyw
    real (kind=8) :: rcoef
    character(len=16) :: load_opti_f
    integer, pointer :: v_llresu_info(:) => null()
    character(len=24), pointer :: v_llresu_name(:) => null()
    character(len=24), pointer :: v_llresu_func(:) => null()
    character(len=8), pointer :: v_list_dble(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_load      = 0
    i_excit      = 0
    const_func   = '&&NTDOCH'
    l_load_user  = .true.
    l_apply_user = .true. 
    if (present(l_load_user_)) then
        l_load_user = l_load_user_
    endif
!
! - Number of loads
!
    if (l_load_user) then
        call getfac('EXCIT', nb_load)
    else
        call jeveuo(list_load_resu//'.INFC', 'L', vi   = v_llresu_info)
        nb_load = v_llresu_info(1)
    endif
!
    if (nb_load .ne. 0) then
!
! ----- Create list of loads 
!
        call lisccr('THER', list_load, nb_load, 'V')
!
! ----- List of loads to avoid same loads
!
        AS_ALLOCATE(vk8 = v_list_dble, size = nb_load)
!
! ----- Access to saved list of loads datastructure
!
        if (.not.l_load_user) then
            call jeveuo(list_load_resu(1:19)//'.INFC', 'L', vi   = v_llresu_info)
            call jeveuo(list_load_resu(1:19)//'.LCHA', 'L', vk24 = v_llresu_name)
            call jeveuo(list_load_resu(1:19)//'.FCHA', 'L', vk24 = v_llresu_func)
        endif
!
! ----- Loop on loads
!
        do i_load = 1 , nb_load
!
! --------- Get parameters for construct list of loads
!
            call load_list_getp('THER'      , l_load_user , v_llresu_info, v_llresu_name,&
                                v_list_dble , l_apply_user, i_load       , nb_load      ,&
                                i_excit     , load_name   , load_type    , ligrch)
!
! --------- Dirichlet loads (AFFE_CHAR_CINE)
!
            nb_info_type = 0
            info_type    = 'RIEN'
            if (load_type(1:5) .eq. 'CITH_') then
                if (load_type(5:7) .eq. '_FT') then
                    info_type = 'CINE_FT'
                else if (load_type(5:7).eq.'_FO') then
                    info_type = 'CINE_FO'
                else
                    info_type = 'CINE_CSTE'
                endif
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! --------- Dirichlet loads (AFFE_CHAR_THER)
!
            info_type = 'RIEN'
            cart_name = ligrch(1:13)//'.CIMPO'
            call jeexin(cart_name//'.DESC', iret)
            if (iret .ne. 0) then
                if (load_type(5:7) .eq. '_FO') then
                    info_type = 'DIRI_FO'
                    call dismoi('PARA_INST', cart_name, 'CARTE', repk=load_para)
                    if (load_para(1:3) .eq. 'OUI') then
                        info_type = 'DIRI_FT'
                    endif
                else
                    info_type = 'DIRI_CSTE'
                endif
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! --------- Multiplicative function
!
            l_func_mult = .false.
            load_func   = const_func
            if (l_load_user) then
                call getvid('EXCIT', 'FONC_MULT', iocc=i_load, scal=load_func, nbret=n1)
                l_func_mult = n1.gt.0
                if (n1 .eq. 0) then
                    rcoef = 1.d0
                    call focste(const_func, 'TOUTRESU', rcoef, 'V')
                endif
            else
                call jeveuo(list_load_resu//'.FCHA', 'L', vk24=v_llresu_func)
                load_func = v_llresu_func(i_load)
                if (load_func(1:2) .ne. '&&') then
                    l_func_mult = .true.
                endif
            endif
!
! --------- Identify type of Neumann loads 
!
            call load_neut_iden(nb_type_neum, load_name, list_load_keyw)
!
! --------- Add Neuman loads
!
            do i_type_neum = 1 ,nb_type_neum
                info_type = 'RIEN'
                if (list_load_keyw(i_type_neum)) then
                    call load_neut_data(i_type_neum, nb_type_neum, '2MBR',&
                                        load_opti_f_ = load_opti_f,&
                                        load_obje_   = load_obje,&
                                        load_keyw_   = load_keyw)
                    cart_name  = load_name(1:8)//'.CHTH'//load_obje(1)
                    if ((load_opti_f.eq.'No_load') .and. l_func_mult) then
                        call utmess('F', 'CHARGES_20', sk=load_name)
                    endif
                    if (load_keyw.eq.'ECHANGE') then
                        if (l_func_mult) then
                            call utmess('F', 'CHARGES_32', sk=load_name)
                        endif
                    endif
                    if (load_keyw.eq.'EVOL_CHAR') then
                        ASSERT (load_type(5:7) .ne. '_FO')
                        info_type = 'NEUM_CSTE'
                    else
                        if (load_type(5:7) .eq. '_FO') then
                            info_type = 'NEUM_FO'
                            call dismoi('PARA_INST', cart_name, 'CARTE', repk=load_para)
                            if (load_para(1:3) .eq. 'OUI') then
                                info_type = 'NEUM_FT'
                            endif
                        else
                           info_type = 'NEUM_CSTE'
                        endif
                    endif
                endif
                if (info_type .ne. 'RIEN') then
                    nb_info_type = nb_info_type + 1
                    ASSERT(nb_info_type.lt.nb_info_maxi)
                    list_info_type(nb_info_type) = info_type
                endif
            end do
!
! --------- Add new load(s) in list
!
            if (nb_info_type .gt. 0) then
                call liscad('THER'      , list_load     , i_load, load_name, load_func, &
                            nb_info_type, list_info_type)
            endif
        end do
    endif
!
    AS_DEALLOCATE(vk8 = v_list_dble)

end subroutine
