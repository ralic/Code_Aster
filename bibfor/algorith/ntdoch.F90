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
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
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
    integer :: nb_type_maxi
    parameter   (nb_type_maxi = 10)
    character(len=6) :: object(nb_type_maxi)
!
    aster_logical :: l_func_mult, l_load_user
    integer :: nb_info_type
    character(len=24) :: info_type
    integer :: nb_load, n1, i_load, i_type, iret
    character(len=24) :: ligrch, lchin, const_func
    character(len=24) :: load_name, load_type, load_para, load_func
    real (kind=8) :: rcoef
    character(len=16) :: pheno
    character(len=24) :: lloadr_name, lloadr_func
    integer, pointer :: v_loadr_info(:) => null()
    character(len=24), pointer :: v_loadr_name(:) => null()
    character(len=24), pointer :: v_loadr_func(:) => null()
!
    data object/&
          '.SOURE'  ,'.FLURE'  ,'.FLUR2'  ,&
          '.T_EXT'  ,'.COEFH'  ,'.HECHP'  ,&
          '.GRAIN'  ,'.FLUNL'  ,'.SOUNL'  ,&
          '.RAYO'   /
!
! --------------------------------------------------------------------------------------------------
!
    nb_load     = 0
    const_func  = '&&NTDOCH'
    l_load_user = .true.
    if (present(l_load_user_)) then
        l_load_user = l_load_user_
    endif
!
! - Number of loads
!
    if (l_load_user) then
        call getfac('EXCIT', nb_load)
    else
        call jeveuo(list_load_resu//'.INFC', 'L', vi   = v_loadr_info)
        nb_load = v_loadr_info(1)
    endif
!
    if (nb_load .ne. 0) then
!
! ----- Create list of loads 
!
        call lisccr('THER', list_load, nb_load, 'V')
!
! ----- Datastructure access
!
        lloadr_name = list_load_resu(1:19)//'.LCHA'
        lloadr_func = list_load_resu(1:19)//'.FCHA'
        if (.not.l_load_user) then
            call jeveuo(lloadr_name, 'L', vk24 = v_loadr_name)
            call jeveuo(lloadr_func, 'L', vk24 = v_loadr_func)
        endif
!
! ----- Loop on loads
!
        do i_load = 1 , nb_load
!
! --------- Name of load
!
            if (l_load_user) then
                call getvid('EXCIT', 'CHARGE', iocc=i_load, scal=load_name)
            else
                call jeveuo(list_load_resu//'.LCHA', 'L', vk24=v_loadr_name)
                load_name = v_loadr_name(i_load)
            endif
!
! --------- Only thermics loads
!
            call dismoi('TYPE_CHARGE', load_name, 'CHARGE', repk=load_type)
            call dismoi('PHENOMENE'  , load_name, 'CHARGE', repk=pheno)
            if (pheno.ne.'THERMIQUE') then
                call utmess('F', 'CHARGES_21', sk=load_name)
            endif
!
! --------- LIGREL of load
!
            ligrch = load_name(1:8)//'.CHTH.LIGRE'
!
! --------- Dirichlet loads (AFFE_CHAR_CINE)
!
            nb_info_type = 0
            info_type    = 'RIEN'
            if (load_type(1:5) .eq. 'CITH_') then
                call jeexin(load_name(1:19)//'.AFCK', iret)
                ASSERT(iret.ne.0)
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
            lchin = ligrch(1:13)//'.CIMPO.DESC'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (load_type(5:7) .eq. '_FO') then
                    info_type = 'DIRI_FO'
                    call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=load_para)
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
                call jeveuo(list_load_resu//'.FCHA', 'L', vk24=v_loadr_func)
                load_func = v_loadr_func(i_load)
                if (load_func(1:2) .ne. '&&') then
                    l_func_mult = .true.
                endif
            endif
!
! --------- Other loads (AFFE_CHAR_THER)
!
            do i_type = 1 ,nb_type_maxi
                info_type = 'RIEN'
                lchin = ligrch(1:13)//object(i_type)//'.DESC'
                call exisd('CHAMP_GD', lchin, iret)
                if (iret .ne. 0) then
                    if ((i_type.ge.6) .and. l_func_mult) then
                        call utmess('F', 'CHARGES_20', sk=load_name(1:8))
                    endif
                    if (load_type(5:7) .eq. '_FO') then
                        info_type = 'NEUM_FO'
                        call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=load_para)
                        if (load_para(1:3) .eq. 'OUI') then
                            info_type = 'NEUM_FT'
                        endif
                    else
                       info_type = 'NEUM_CSTE'
                    endif
                endif
                if (info_type .ne. 'RIEN') then
                    nb_info_type = nb_info_type + 1
                    ASSERT(nb_info_type.lt.nb_info_maxi)
                    list_info_type(nb_info_type) = info_type
                endif
            end do
!
! --------- Add load
!
            if (nb_info_type .gt. 0) then
                call liscad('THER'      , list_load     , i_load, load_name, load_func, &
                            nb_info_type, list_info_type)
            endif
        end do
    endif

end subroutine
