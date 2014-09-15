subroutine nmetcr(model      , compor     , list_func_acti, sddyna   , sdpost,&
                  sdcont_defi, sdcont_algo, sd_inout      , cara_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/nmetac.h"
#include "asterfort/nmetc0.h"
#include "asterfort/nmetcc.h"
#include "asterfort/rscrsd.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: model
    integer, intent(in) :: list_func_acti(*)
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_algo
    character(len=19), intent(in) :: compor
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sdpost
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(out) :: sd_inout
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Init
!
! Create input/output datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  cara_elem        : name of datastructure for elementary parameters (CARTE)
! In  compor           : name of <CARTE> COMPOR
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_algo      : name of contact algorithm datastructure
! In  list_func_acti   : list of active functionnalities
! In  sddyna           : name of dynamic parameters datastructure
! In  sdpost           : name of post-treatment for stability analysis parameters datastructure
! Out sd_inout         : datastructure for input/output parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: zioch, nb_field_maxi
    parameter    (zioch = 10, nb_field_maxi=20 )
!
    integer :: nb_field, nb_field_in, nb_field_out
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: i_field, i_field_maxi
    aster_logical :: list_field_acti(nb_field_maxi), l_find
    character(len=19) :: result
    integer :: i_field_resu, nb_field_resu
    character(len=24) :: field_resu, field_read, field_save, field_state
    character(len=24) :: field_type, field_name_algo, field_name_init
!
    character(len=24) :: field_name_resu(nb_field_maxi), keyw_obsv(nb_field_maxi)
    character(len=24) :: field_gran(nb_field_maxi), keyw_etat_init(nb_field_maxi)
    character(len=24) :: field_disc(nb_field_maxi)
    character(len=24) :: flag_arch(nb_field_maxi), flag_etat_init(nb_field_maxi)
! - Name of field (type) in results datastructure (add one -> modify rscrsd subroutine)
    data field_name_resu  /'DEPL'        ,'SIEF_ELGA'   ,'VARI_ELGA'   ,&
                           'COMPORTEMENT','VITE'        ,'ACCE'        ,&
                           'INDC_ELEM'   ,'SECO_ELEM'   ,'COHE_ELEM'   ,&
                           'VALE_CONT'   ,'MODE_FLAMB'  ,'DEPL_VIBR'   ,&
                           'DEPL_ABSOLU' ,'VITE_ABSOLU' ,'ACCE_ABSOLU' ,&
                           'FORC_NODA'   ,'STRX_ELGA'   ,'MODE_STAB'   ,&
                           'FORC_AMOR'   ,'FORC_LIAI'/
! - Type of GRANDEUR for field
    data field_gran       /'DEPL_R','SIEF_R','VARI_R',&
                           'COMPOR','DEPL_R','DEPL_R',&
                           'NEUT_I','NEUT_R','NEUT_R',&
                           'DEPL_R','DEPL_R','DEPL_R',&
                           'DEPL_R','DEPL_R','DEPL_R',&
                           'DEPL_R','STRX_R','DEPL_R',&
                           'DEPL_R','DEPL_R'/
! - Keyword for initial state (ETAT_INIT)
    data keyw_etat_init   /'DEPL','SIGM','VARI',&
                           ' '   ,'VITE','ACCE',&
                           ' '   ,' '   ,' '   ,&
                           ' '   ,' '   ,' '   ,&
                           ' '   ,' '   ,' '   ,&
                           ' '   ,'STRX',' '   ,&
                           ' '   ,' '/
! - Spatial discretization of field
    data field_disc       /'NOEU','ELGA','ELGA',&
                           'ELGA','NOEU','NOEU',&
                           'ELEM','ELEM','ELEM',&
                           'NOEU','NOEU','NOEU',&
                           'NOEU','NOEU','NOEU',&
                           'NOEU','ELGA','NOEU',&
                           'NOEU','NOEU'/
! - 'OUI' if field can been read for initial state (ETAT_INIT)
    data flag_etat_init   /'OUI','OUI','OUI',&
                           'NON','OUI','OUI',&
                           'OUI','OUI','OUI',&
                           'NON','NON','NON',&
                           'OUI','OUI','OUI',&
                           'NON','OUI','NON',&
                           'OUI','OUI'/
! - 'OUI' if field can been store (ARCHIVAGE)
    data flag_arch        /'OUI','OUI','OUI',&
                           'OUI','OUI','OUI',&
                           'OUI','OUI','OUI',&
                           'OUI','OUI','OUI',&
                           'OUI','OUI','OUI',&
                           'NON','OUI','OUI',&
                           'OUI','OUI'/
! - Keyword for OBSERVATION
    data keyw_obsv        /'DEPL'        ,'SIEF_ELGA'   ,'VARI_ELGA'   ,&
                           ' '           ,'VITE'        ,'ACCE'        ,&
                           ' '           ,' '           ,' '           ,&
                           'VALE_CONT'   ,' '           ,' '           ,&
                           'DEPL_ABSOLU' ,'VITE_ABSOLU' ,'ACCE_ABSOLU' ,&
                           'FORC_NODA'   ,'STRX_ELGA'   ,' '           ,&
                           ' '           ,' '/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    sd_inout     = '&&NMETCR.INOUT'
    nb_field     = 0
    nb_field_in  = 0
    nb_field_out = 0
    list_field_acti(1:nb_field_maxi) = .false.
    result = '&&NMETCR'
!
! - Select fields depending on active functionnalities
!
    call nmetac(list_func_acti, sddyna, sdcont_defi, nb_field_maxi, list_field_acti)
!
! - Count active fields (input/output)
!
    do i_field = 1, nb_field_maxi
        if (list_field_acti(i_field)) then
            nb_field = nb_field + 1
            if (flag_etat_init(i_field).eq.'OUI') nb_field_in  = nb_field_in + 1
            if (flag_arch(i_field).eq.'OUI')      nb_field_out = nb_field_out + 1
        endif
    end do
!
! - Create datastructure
!
    io_lcha = sd_inout(1:19)//'.LCHA'
    io_info = sd_inout(1:19)//'.INFO'
    call wkvect(io_lcha, 'V V K24', zioch*nb_field, vk24 = v_io_para)
    call wkvect(io_info, 'V V I'  , 4             , vi   = v_io_info)
!
! - Save informations
!
    v_io_info(1) = nb_field
    v_io_info(2) = nb_field_in
    v_io_info(3) = nb_field_out
    v_io_info(4) = zioch
!
! - Add fields
!
    i_field     = 0
    field_state = ' '
    do i_field_maxi = 1, nb_field_maxi
        if (list_field_acti(i_field_maxi)) then
            i_field    = i_field + 1
            field_type = field_name_resu(i_field_maxi)
            call nmetcc(field_type     , field_name_algo, field_name_init, &
                        compor         , sddyna         , sdpost         , sdcont_algo)
            v_io_para(zioch*(i_field-1)+1 ) = field_type
            v_io_para(zioch*(i_field-1)+2 ) = field_name_init
            v_io_para(zioch*(i_field-1)+3 ) = keyw_etat_init(i_field_maxi)
            v_io_para(zioch*(i_field-1)+4 ) = field_state
            v_io_para(zioch*(i_field-1)+5 ) = field_disc(i_field_maxi)
            v_io_para(zioch*(i_field-1)+6 ) = field_name_algo
            v_io_para(zioch*(i_field-1)+7 ) = field_gran(i_field_maxi)
            v_io_para(zioch*(i_field-1)+8 ) = flag_etat_init(i_field_maxi)
            v_io_para(zioch*(i_field-1)+9 ) = flag_arch(i_field_maxi)
            v_io_para(zioch*(i_field-1)+10) = keyw_obsv(i_field_maxi)
        endif
    end do
    ASSERT(i_field.eq.nb_field)
!
! - Create initial state fields
!
    call nmetc0(model, cara_elem, compor, sd_inout)
!
! - Check !
!
    call rscrsd('V', result, 'EVOL_NOLI', 1) 
    call jelira(result(1:8)//'           .DESC', 'NOMMAX', nb_field_resu)
    do i_field = 1, nb_field
        field_type      = v_io_para(zioch*(i_field-1)+1)
        field_name_init = v_io_para(zioch*(i_field-1)+2)
        field_read      = v_io_para(zioch*(i_field-1)+8)
        field_save      = v_io_para(zioch*(i_field-1)+9)
        if (field_save .eq. 'OUI') then
            l_find = .false.
            do i_field_resu = 1, nb_field_resu
                call jenuno(jexnum(result(1:8)//'           .DESC', i_field_resu), field_resu)
                if (field_resu .eq. field_type) l_find = .true.
            end do
! --------- No field in results => change rscrsd subroutine !
            ASSERT(l_find)
        endif
    end do
    call detrsd('RESULTAT', result)
!
    call jedema()
end subroutine
