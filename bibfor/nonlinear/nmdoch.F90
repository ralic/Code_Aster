subroutine nmdoch(list_load, l_load_user, list_load_resu)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/nmdoch_nbload.h"
#include "asterfort/load_list_getp.h"
#include "asterfort/load_unde_diri.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liscad.h"
#include "asterfort/lisccr.h"
#include "asterfort/lisexp.h"
#include "asterfort/lislfc.h"
#include "asterfort/utmess.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
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
    aster_logical, intent(in) :: l_load_user
    character(len=19), intent(in) :: list_load
    character(len=19), intent(in) :: list_load_resu
!
! --------------------------------------------------------------------------------------------------
!
! Mechanics - Read parameters
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
! --- NOMBRE MAXIMUM DE TYPE_INFO
    integer :: nb_info_maxi
    parameter   (nb_info_maxi=99)
    character(len=24) :: list_info_type(nb_info_maxi)
! --- NOMBRE MAXIMUM DE RESUELEM POUR LES FORCES DE LAPLACE : NBCHMX
    integer :: nbchmx
    parameter   (nbchmx=99)
! --- NOMBRE MAXIMUM DE TYPESD DE CHARGE                    : NBTYCH
    integer :: nbtych
    parameter    (nbtych=18)
    character(len=6) :: nomlig(nbtych)
!
    integer :: itych
    integer :: n1
    integer :: npilo, nb_excit, nb_load
    integer :: infmax, i_excit, i_load, iret, infc, j, i_load_new
    character(len=5) :: suffix
    character(len=8) :: k8bid, load_type, parcha, load_apply
    character(len=8) :: fctcsr
    character(len=16) :: nomcmd, typesd
    character(len=8) :: load_name, nomfct
    character(len=24) :: info_type
    character(len=19) :: lisdbl
    character(len=24) :: ligrch, lchin
    integer :: i_neum_lapl, i_diri_suiv
    aster_logical :: lfcplx, lacce, l_zero_allowed, l_apply_user
    integer :: nb_info_type
    integer, pointer :: v_ll_infc(:) => null()
    integer, pointer :: v_llresu_info(:) => null()
    character(len=24), pointer :: v_llresu_name(:) => null()
    character(len=8), pointer :: v_list_dble(:) => null()
!
    data nomlig  /'.FORNO','.F3D3D','.F2D3D','.F1D3D',&
                  '.F2D2D','.F1D2D','.F1D1D','.PESAN',&
                  '.ROTAT','.PRESS','.FELEC','.FCO3D',&
                  '.FCO2D','.EPSIN','.FLUX' ,'.VEASS',&
                  '.ONDPL','.SIINT'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call getres(k8bid, typesd, nomcmd)
!
! - Initializations
!
    nb_load        = 0
    infmax         = 0
    fctcsr         = '&&NMDOME'
    lisdbl         = '&&NMDOME.LISDBL'
    lfcplx         = .false.
    lacce          = .false.
    info_type      = 'RIEN'
    npilo          = 0
    i_excit        = 0
    nb_excit       = 0
    i_load_new     = 0
    i_diri_suiv    = 0
!
! - Is applying load option by user ?
!
    l_apply_user   = .false.
    if (l_load_user) then
        if (nomcmd.eq.'DYNA_LINE_TRAN' .or. nomcmd.eq.'DYNA_LINE_HARM') then
            l_apply_user   = .false.
        else
            l_apply_user   = .true.
        endif
    else
        if (nomcmd.eq.'CALC_CHAMP') then
            l_apply_user   = .false.
        else
            l_apply_user   = .true.
        endif
    endif
!
! - Can we create "zero-load" list of loads datastructure ?
!
    l_zero_allowed = .false.
    if (l_load_user) then
        l_zero_allowed = nomcmd.eq.'DYNA_NON_LINE'.or.&
                         nomcmd.eq.'CALC_FORC_NONL'.or.&
                         nomcmd.eq.'CALC_CHAMP'.or.&
                         nomcmd.eq.'POST_ELEM'.or.&
                         nomcmd.eq.'LIRE_RESU'.or.&
                         nomcmd.eq.'CREA_RESU'
    else
        l_zero_allowed = .true.
    endif
!
! - Get number of loads for loads datastructure
!
    call nmdoch_nbload(l_load_user, list_load_resu, l_zero_allowed, nb_load,&
                       nb_excit)
!
! - Create "zero-load" list of loads datastructure
!
    if (nb_load.eq.0) then
        call lisccr('MECA', list_load, 1, 'V')
        call jeveuo(list_load(1:19)//'.INFC', 'E', vi=v_ll_infc)
        v_ll_infc(1) = nb_load
    endif
!
! - Access to saved list of loads datastructure
!
    if (.not.l_load_user) then
        call jeveuo(list_load_resu(1:19)//'.INFC', 'L', vi   = v_llresu_info)
        call jeveuo(list_load_resu(1:19)//'.LCHA', 'L', vk24 = v_llresu_name)
    endif
!
    if (nb_load .ne. 0) then
!
! ----- Create list of loads
!
        call lisccr('MECA', list_load, nb_load, 'V')
!
! ----- List of loads to avoid same loads
!
        AS_ALLOCATE(vk8 = v_list_dble, size = nb_load)
!
! ----- Loop on loads
!
        do i_load = 1, nb_load
!
! --------- Get parameters for construct list of loads
!
            call load_list_getp('MECA'      , l_load_user , v_llresu_info, v_llresu_name,&
                                v_list_dble , l_apply_user, i_load       , nb_load      ,&
                                i_excit     , load_name   , load_type    , ligrch       ,&
                                load_apply)
!
! --------- Number of loads "PILOTAGE"
!
            if (load_apply .eq. 'FIXE_PIL') then
                npilo = npilo + 1
            endif
!
! --------- FONCTIONS MULTIPLICATIVES DES CHARGES
!
            lfcplx = (&
                     nomcmd .eq. 'DYNA_LINE_HARM' .or.&
                     ( nomcmd.eq.'LIRE_RESU' .and. typesd.eq.'DYNA_HARMO' )&
                     )
            lacce = (nomcmd.eq.'DYNA_NON_LINE'.or. nomcmd.eq.'LIRE_RESU')
            call lislfc(list_load_resu, i_load, i_excit, l_load_user, nb_excit,&
                        lfcplx, lacce, fctcsr, nomfct)
            if (nomfct .ne. fctcsr) then
                if (load_apply .eq. 'FIXE_PIL') then
                    call utmess('F', 'CHARGES_38', sk=load_name)
                endif
            endif
!
! ------- CHARGE DE TYPE DIRICHLET PROVENANT D'UN AFFE_CHAR_CINE
!
            nb_info_type = 0
            info_type = 'RIEN'
            if (load_type(1:5) .eq. 'CIME_') then
                if (load_apply(1:4) .eq. 'SUIV') then
                    call utmess('F', 'CHARGES_23', sk=load_name)
                else if (load_apply.eq.'FIXE_PIL') then
                    call utmess('F', 'CHARGES_27', sk=load_name)
                else if (load_apply(1:4).eq.'DIDI') then
                    call utmess('F', 'CHARGES_24', sk=load_name)
                else
                    if (load_type(5:7) .eq. '_FT') then
                        info_type = 'CINE_FT'
                    else if (load_type(5:7).eq.'_FO') then
                        info_type = 'CINE_FO'
                    else
                        info_type = 'CINE_CSTE'
                    endif
                endif
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! -------- CHARGE DE TYPE DIRICHLET PROVENANT DE AFFE_CHAR_MECA
!
            info_type = 'RIEN'
            lchin = ligrch(1:13)//'.CIMPO.DESC'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (load_apply(1:4) .eq. 'SUIV') then
                    info_type    = 'DIRI_SUIV'
                    if (i_diri_suiv.ne.0) then
                        call utmess('F', 'CHARGES_29')
                    else
                        i_diri_suiv  = i_load
                    endif
                else if (load_apply.eq.'FIXE_PIL') then
                    call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=parcha)
                    if (parcha(1:3) .eq. 'OUI') then
                        call utmess('F', 'CHARGES_28', sk=load_name)
                    endif
                    if (load_type(5:7) .eq. '_FT') then
                        call utmess('F', 'CHARGES_28', sk=load_name)
                    else if (load_type(5:7).eq.'_FO') then
                        info_type = 'DIRI_PILO_F'
                    else
                        info_type = 'DIRI_PILO'
                    endif
                else
                    if (load_type(5:7) .eq. '_FO') then
                        info_type = 'DIRI_FO'
                        call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=parcha)
                        if (parcha(1:3) .eq. 'OUI') then
                            info_type = 'DIRI_FT'
                        endif
                    else
                        info_type = 'DIRI_CSTE'
                    endif
                    if (load_apply(1:4) .eq. 'DIDI') then
                        info_type = info_type(1:9)//'_DIDI'
                    endif
                endif
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! ------- CHARGE DE TYPE NEUMANN
!
            do itych = 1, nbtych
                if (nomlig(itych) .eq. '.VEASS') then
                    suffix = '     '
                else
                    suffix = '.DESC'
                endif
                lchin = ligrch(1:13)//nomlig(itych)//suffix
                call jeexin(lchin, iret)
                info_type = 'RIEN'
                if (iret .ne. 0) then
                    if (nomlig(itych) .eq. '.ONDPL') then
                        info_type = 'NEUM_ONDE'
                    else if (nomlig(itych).eq.'.SIINT') then
                        info_type = 'NEUM_SIGM_INT'
                    else if (load_apply.eq.'FIXE_PIL') then
                        info_type = 'NEUM_PILO'
                        if (nomlig(itych) .ne. '.VEASS') then
                            call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=parcha)
                            if (parcha(1:3) .eq. 'OUI') then
                                call utmess('F', 'CHARGES_28')
                            endif
                        endif
                    else if (load_apply(1:4).eq.'SUIV') then
                        info_type = 'NEUM_SUIV'
                    else if (load_type(5:7).eq.'_FO') then
                        call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=parcha)
                        if (parcha(1:3) .eq. 'OUI') then
                            info_type = 'NEUM_FT'
                        else
                            info_type = 'NEUM_FO'
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
! ------- CHARGE DE TYPE EVOL_CHAR
!
            info_type = 'RIEN'
            lchin = ligrch(1:13)//'.EVOL.CHAR'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (load_apply(1:4) .eq. 'SUIV') then
                    info_type = 'NEUM_SUIV'
                else
                    info_type = 'NEUM_CSTE'
                endif
                if (load_apply .eq. 'FIXE_PIL') then
                    call utmess('F', 'CHARGES_34', sk=load_name)
                endif
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! ------- CHARGE DE TYPE EXCIT_SOL
!
            info_type = 'RIEN'
            lchin = ligrch(1:13)//'.VEISS'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (nomcmd .eq. 'STAT_NON_LINE') then
                    call utmess('F', 'CHARGES_50', sk=load_name)
                endif
                if (load_apply .eq. 'SUIV') then
                    call utmess('F', 'CHARGES_51', sk=load_name)
                endif
                if (load_apply .eq. 'DIDI') then
                    call utmess('F', 'CHARGES_52', sk=load_name)
                endif
                if (load_type(5:6) .eq. '_F') then
                    call utmess('F', 'CHARGES_53', sk=load_name)
                endif
                if (nomfct .ne. fctcsr) then
                    call utmess('F', 'CHARGES_54', sk=load_name)
                endif
                info_type = 'EXCIT_SOL'
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! -------- CHARGES DE TYPE FORCE DE LAPLACE
!
            infc = 0
            info_type = 'RIEN'
            do j = 1, nbchmx
                lchin(1:17) = ligrch(1:13)//'.FL1'
                call codent(j, 'D0', lchin(18:19))
                lchin = lchin(1:19)//'.DESC'
                call jeexin(lchin, iret)
                if (iret .ne. 0) then
                    infc = infc + 1
                else
                    goto 90
                endif
            end do
 90         continue
            if (infc .ne. 0) then
                i_neum_lapl = max(infmax,infc)
                info_type = 'NEUM_LAPL'
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! --------- Only undead load for Dirichlet in one load of list_load datastructure
!
            if (i_diri_suiv.ne.0) then
                if (nb_info_type.ne.1) then
                    call utmess('F', 'CHARGES_25', sk=load_name)
                endif
            endif
!
! --------- Add new load(s) in list
!
            if (nb_info_type .gt. 0) then
                i_load_new = i_load_new+1
                call liscad('MECA'      , list_load     , i_load_new, load_name, nomfct, &
                            nb_info_type, list_info_type, i_neum_laplz = i_neum_lapl)
            endif
!
        end do
!
! ---- PILOTAGE POSSIBLE SI IL YA DES CHARGES PILOTEES !
!
        if (nomcmd .ne. 'LIRE_RESU') then
            if (nomcmd .eq. 'STAT_NON_LINE') then
                call getvtx('PILOTAGE', 'TYPE', iocc=1, nbret=n1)
                if (n1 .ne. 0 .and. npilo .eq. 0) then
                    call utmess('F', 'CHARGES_39')
                endif
                if (npilo .gt. 1) then
                    call utmess('F', 'CHARGES_40')
                endif
            endif
        endif
!
! ----- Some loads are prohibited with PILOTAGE
!
        if (npilo .ge. 1) then
            call lisexp(list_load)
        endif
    endif
!
! - Modify list for undead Dirichlet load
!
    if (i_diri_suiv.ne.0) then
        call load_unde_diri(list_load, i_diri_suiv)
    endif  
    
    AS_DEALLOCATE(vk8 = v_list_dble)
    call jedema()
end subroutine
