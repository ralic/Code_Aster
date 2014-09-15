subroutine ntetcr(nume_ddl, l_temp_nonl, sd_inout,&
                  comporz , hydrz      , hydr_initz)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nthydr.h"
#include "asterfort/nmetcc.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
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
    character(len=24), intent(in) :: nume_ddl
    aster_logical, intent(in) :: l_temp_nonl
    character(len=24), intent(out) :: sd_inout
    character(len=*), optional, intent(in) :: comporz
    character(len=*), optional, intent(in) :: hydrz
    character(len=*), optional, intent(in) :: hydr_initz
!
! --------------------------------------------------------------------------------------------------
!
! THER_* - Init
!
! Create input/output datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_ddl         : name of nume_ddl object (numbering equation)
! In  compor           : name of <CARTE> COMPOR
! In  l_temp_nonl      : .true. if THER_NON_LINE
! In  hydr             : name of field for hydratation
! In  hydr_init        : name of field for initialhydratation
! Out sd_inout         : datastructure for input/output parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: zioch, nb_field_maxi
    parameter    (zioch = 10,nb_field_maxi=3 )
!
    integer :: nb_field, nb_field_in, nb_field_out
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: i_field, i_field_maxi, neq
    aster_logical :: l_hydr
    character(len=24) :: temp_init
    aster_logical :: list_field_acti(nb_field_maxi)
    character(len=24) :: field_type, field_name_algo, field_name_init, field_state
    character(len=19) :: compor
    character(len=24) :: hydr, hydr_init
!
    character(len=24) :: field_name_resu(nb_field_maxi), keyw_obsv(nb_field_maxi)
    character(len=24) :: field_gran(nb_field_maxi), keyw_etat_init(nb_field_maxi)
    character(len=24) :: field_disc(nb_field_maxi)
    character(len=24) :: flag_arch(nb_field_maxi), flag_etat_init(nb_field_maxi)
! - Name of field (type) in results datastructure (add one -> modify rscrsd subroutine)
    data field_name_resu  /'TEMP'  ,'HYDR_ELNO'   ,'COMPORTHER'  /
! - Type of GRANDEUR for field
    data field_gran       /'TEMP_R','HYDR_R','COMPOR'/
! - Keyword for initial state (ETAT_INIT)
    data keyw_etat_init   /'CHAM_NO',' ',' '/
! - Spatial discretization of field
    data field_disc       /'NOEU','ELNO','ELGA'/
! - 'OUI' if field can been read for initial state (ETAT_INIT)
    data flag_etat_init   /'OUI', 'OUI', 'NON'/
! - 'OUI' if field can been store (ARCHIVAGE)
    data flag_arch        /'OUI','OUI','OUI'/
! - Keyword for OBSERVATION
    data keyw_obsv        /'TEMP'        ,' '   ,' '/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    compor       = ' '
    hydr         = ' '
    hydr_init    = ' '
    if (present(comporz))    compor = comporz
    if (present(hydrz))      hydr = hydrz
    if (present(hydr_initz)) hydr_init = hydr_initz
    sd_inout     = '&&NTETCR.INOUT'
    nb_field     = 0
    nb_field_in  = 0
    nb_field_out = 0
    list_field_acti(1:nb_field_maxi) = .false.
    temp_init = '&&NTETCR.TEMP0'
!
! - Active functionnalities
!
    l_hydr = .false.
    if (l_temp_nonl) then
        call nthydr(l_hydr)
    endif
!
! - Select fields depending on active functionnalities
!
    list_field_acti(1) = .true.
    if (l_temp_nonl) then
        list_field_acti(3) = .true.
    endif
    if (l_hydr) then
        list_field_acti(2) = .true.
    endif
!
! - Count active fields (input/output)
!
    do i_field_maxi = 1, nb_field_maxi
        if (list_field_acti(i_field_maxi)) then
            nb_field = nb_field + 1
            if (flag_etat_init(i_field_maxi).eq.'OUI') nb_field_in  = nb_field_in + 1
            if (flag_arch(i_field_maxi).eq.'OUI')      nb_field_out = nb_field_out + 1
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
            call nmetcc(field_type     , field_name_algo      , field_name_init, &
                        compor =compor ,&
                        hydr = hydr    , temp_init = temp_init, hydr_init = hydr_init )
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
    call vtcreb(temp_init, nume_ddl, 'V', 'R', neq)
!
    call jedema()
end subroutine
