subroutine load_neum_spec(load_name    , load_nume  , load_type  , ligrel_calc, i_type_neum,&
                          nb_type_neumz, nb_in_maxi , nb_in_prep , lchin      , lpain      ,&
                          nb_in_add    , load_ligrel, load_option, matr_type  , iden_direct,&
                          name_inputz)
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: load_name
    integer, intent(in) :: load_nume
    character(len=19), intent(in) :: ligrel_calc
    character(len=4), intent(in) :: load_type
    integer, intent(in) :: i_type_neum
    integer, intent(in) :: nb_type_neumz
    integer, intent(in) :: nb_in_maxi
    integer, intent(in) :: nb_in_prep
    character(len=*), intent(inout) :: lpain(nb_in_maxi)
    character(len=*), intent(inout) :: lchin(nb_in_maxi)
    integer, intent(out) :: nb_in_add
    character(len=19), intent(out) :: load_ligrel
    character(len=16), intent(out) :: load_option
    character(len=8), optional, intent(out) :: matr_type
    character(len=*), optional, intent(in) :: iden_direct
    character(len=*), optional, intent(in) :: name_inputz
!
! --------------------------------------------------------------------------------------------------
!
! Neumann loads computation
!
! Get information about load (Neumann)
!
! --------------------------------------------------------------------------------------------------
!
! In  load_name      : name of current load
! In  load_nume      : identification of load type
! In  load_type      : load type to compute
!                        'Dead' - Dead loads (not dependent on displacements)
!                        'Pilo' - Loads for continuation (not dependent on displacements)
!                        'Suiv' - Undead loads (dependent on displacements)
! In  ligrel_calc    : LIGREL to compute
! In  i_type_neum    : index for Neumann load type
! In  nb_type_neumz  : maximum number of Neumann load type
! In  nb_in_maxi     : maximum number of input fields
! In  nb_in_prep     : number of input fields before specific ones
! IO  lpain          : list of input parameters
! IO  lchin          : list of input fields
! Out nb_in_add      : number of input fields which been added
! Out load_ligrel    : name of LIGREL for current load
! Out load_option    : name of option for current load
! Out matr_type      : matrix type for undead load
! In  iden_direct    : direct identification of type
! In  name_inputz    : direct name of input field
!
! Exception: if type VECT_ASSE -> option ='Copy_Load' and VECT_ASSE name in load_ligrel
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_type_neum
    parameter (nb_type_neum=18)
    character(len=6) :: object(nb_type_neum)
    character(len=7) :: para_r(nb_type_neum), para_f(nb_type_neum)
    character(len=16) :: option_f(nb_type_neum), option_r(nb_type_neum)
    character(len=16) :: optsui_f(nb_type_neum), optsui_r(nb_type_neum)
    character(len=16) :: optmat_f(nb_type_neum), optmat_r(nb_type_neum)
    logical :: l_pilo(nb_type_neum)
    logical :: l_suiv(nb_type_neum)
    character(len=7) :: para_matr(nb_type_neum)
!
    integer :: iret, i_field_in
    character(len=24) :: identify
    character(len=19) :: ligrel_load, name_input
    character(len=8) :: affcha
    logical :: l_constant, l_fonct_0, l_fonct_t, l_sigm_int
    character(len=8), pointer :: p_vale_sigm(:) => null()
    character(len=8), pointer :: p_vect_asse(:) => null()
!
! - Object name construct in AFFE_CHAR_MECA
!
    data object   /'.FORNO','.F3D3D','.F2D3D','.F1D3D',&
                   '.F2D2D','.F1D2D','.F1D1D','.PESAN',&
                   '.ROTAT','.PRESS','.FELEC','.FCO3D',&
                   '.FCO2D','.EPSIN','.FLUX' ,'.VEASS',&
                   '.SIINT','.EFOND'/
!
! - Name of input parameter field (real coefficient)
!
    data para_r   /'PFORNOR','PFR3D3D','PFR2D3D','PFR1D3D',&
                   'PFR2D2D','PFR1D2D','PFR1D1D','PPESANR',&
                   'PROTATR','PPRESSR','PFRELEC','PFRCO3D',&
                   'PFRCO2D','PEPSINR','PFLUXR' ,'       ',&
                   '       ','PEFOND'/
!
! - Name of option for dead load (real coefficient)
!
    data option_r /'CHAR_MECA_FORC_R','CHAR_MECA_FR3D3D','CHAR_MECA_FR2D3D','CHAR_MECA_FR1D3D',&
                   'CHAR_MECA_FR2D2D','CHAR_MECA_FR1D2D','CHAR_MECA_FR1D1D','CHAR_MECA_PESA_R',&
                   'CHAR_MECA_ROTA_R','CHAR_MECA_PRES_R','CHAR_MECA_FRELEC','CHAR_MECA_FRCO3D',&
                   'CHAR_MECA_FRCO2D','CHAR_MECA_EPSI_R','CHAR_MECA_FLUX_R','Copy_Load'       ,&
                   'FORC_NODA       ','CHAR_MECA_EFON_R'/
!
! - Name of option for undead load (real coefficient)
!
    data optsui_r /'No_Load         ','No_Load         ','No_Load         ','No_Load         ',&
                   'No_Load         ','No_Load         ','CHAR_MECA_SR1D1D','CHAR_MECA_PESA_R',&
                   'CHAR_MECA_ROTA_R','CHAR_MECA_PRSU_R','No_Load         ','CHAR_MECA_SRCO3D',&
                   'No_Load         ','No_Load         ','No_Load         ','No_Load         ',&
                   'No_Load         ','CHAR_MECA_EFON_R'/
!
! - Name of option for undead load matrix (real coefficient)
!
    data optmat_r /'No_Load         ','No_Load         ','No_Load         ','No_Load         ',&
                   'No_Load         ','No_Load         ','No_Load         ','No_Load         ',&
                   'RIGI_MECA_RO    ','RIGI_MECA_PRSU_R','No_Load         ','RIGI_MECA_SRCO3D',&
                   'No_Load         ','No_Load         ','No_Load         ','No_Load         ',&
                   'No_Load         ','RIGI_MECA_EFON_R'/
!
! - Name of input parameter field (function coefficient)
!
    data para_f   /'PFORNOF','PFF3D3D','PFF2D3D','PFF1D3D',&
                   'PFF2D2D','PFF1D2D','PFF1D1D','PPESANR',&
                   'PROTATR','PPRESSF','PFRELEC','PFFCO3D',&
                   'PFFCO2D','PEPSINF','PFLUXF' ,'       ',&
                   '       ','PEFOND'/
!
! - Name of option for dead load (function coefficient)
!
    data option_f /'CHAR_MECA_FORC_F','CHAR_MECA_FF3D3D','CHAR_MECA_FF2D3D','CHAR_MECA_FF1D3D',&
                   'CHAR_MECA_FF2D2D','CHAR_MECA_FF1D2D','CHAR_MECA_FF1D1D','CHAR_MECA_PESA_R',&
                   'CHAR_MECA_ROTA_R','CHAR_MECA_PRES_F','CHAR_MECA_FRELEC','CHAR_MECA_FFCO3D',&
                   'CHAR_MECA_FFCO2D','CHAR_MECA_EPSI_F','CHAR_MECA_FLUX_F','Copy_Load',&
                   'FORC_NODA       ','CHAR_MECA_EFON_F'/
!
! - Name of option for undead load (function coefficient)
!
    data optsui_f /'No_Load         ','No_Load         ','No_Load         ','No_Load         ',&
                   'No_Load         ','No_Load         ','CHAR_MECA_SF1D1D','No_Load         ',&
                   'No_Load         ','CHAR_MECA_PRSU_F','No_Load         ','CHAR_MECA_SFCO3D',&
                   'No_Load         ','No_Load         ','No_Load         ','No_Load         ',&
                   'No_Load         ','CHAR_MECA_EFON_F'/
!
! - Name of option for undead load matrix (function coefficient)
!
    data optmat_f /'No_Load         ','No_Load         ','No_Load         ','No_Load         ',&
                   'No_Load         ','No_Load         ','No_Load         ','No_Load         ',&
                   'No_Load         ','RIGI_MECA_PRSU_F','No_Load         ','RIGI_MECA_SFCO3D',&
                   'No_Load         ','No_Load         ','No_Load         ','No_Load         ',&
                   'No_Load         ','RIGI_MECA_EFON_F'/
!
! - Flag if load can been undead load type
!
    data l_suiv   /.false.,.false.,.false.,.false.,&
                   .false.,.false.,.true. ,.true. ,&
                   .true. ,.true. ,.false.,.true. ,&
                   .false.,.false.,.false.,.false.,&
                   .false.,.true. /
!
! - Flag if load can been used for continuation methods
!
    data l_pilo   /.true. ,.true. ,.true. ,.true. ,&
                   .true. ,.true. ,.true. ,.true. ,&
                   .false.,.true. ,.false.,.true. ,&
                   .true. ,.false.,.true. ,.true. ,&
                   .false.,.true./
!
! - Type of matrix for undead load
!
    data para_matr/'       ','       ','       ','       ',&
                   '       ','       ','       ','       ',&
                   'PMATUUR','PMATUNS','       ','PMATUNS',&
                   '       ','       ','       ','       ',&
                   '       ','PMATUUR'/
!
! --------------------------------------------------------------------------------------------------
!
    ligrel_load  = load_name(1:8)//'.CHME.LIGRE'
    load_ligrel  = ' '
    load_option  = 'No_Load'
    l_constant   = .false.
    l_fonct_0    = .false.
    l_fonct_t    = .false.
    l_sigm_int   = .false.
    i_field_in   = nb_in_prep
    ASSERT(i_type_neum.le.nb_type_neum)
    ASSERT(nb_type_neumz.eq.nb_type_neum)
!
! - Identify current load
!
    iret = 0
    if (present(iden_direct)) then
        if (iden_direct.eq.object(i_type_neum)) then
            iret = 1
        endif
        name_input = name_inputz
    else
        if (object(i_type_neum) .eq. '.VEASS') then
            identify = load_name(1:8)//'.CHME'//object(i_type_neum)
        else
            identify = load_name(1:8)//'.CHME'//object(i_type_neum)//'.DESC'
        endif
        name_input = load_name(1:8)//'.CHME'//object(i_type_neum)
        call jeexin(identify, iret)
    endif
!
    if (iret .ne. 0) then
!
! ----- Value type
!
        if (load_nume .eq. 1) then
            l_constant = .true.
        elseif (load_nume .eq. 5) then
            l_constant = .true.
        else if (load_nume .eq. 2) then
            l_fonct_0  = .true.
        else if (load_nume .eq. 3) then
            l_fonct_t  = .true.               
        else if (load_nume .eq. 55) then
            l_sigm_int = .true.
        endif
!
! ----- Special for undeads loads
!
        if (load_nume .eq. 4) then
            l_constant = .true.
            call dismoi('TYPE_CHARGE', load_name, 'CHARGE', repk=affcha)
            if (affcha(5:7) .eq. '_FO') then
                l_constant = .false.
                l_fonct_0  = .true.
            endif
        endif
!
! ----- Name of option
!
        if (l_constant) then
            if (load_type.eq.'Suiv') then
                if (present(matr_type)) then
                    load_option = optmat_r(i_type_neum)
                else
                    load_option = optsui_r(i_type_neum)
                endif
            else
                load_option = option_r(i_type_neum)
            endif
        else if (l_fonct_0.or.l_fonct_t) then
            if (load_type.eq.'Suiv') then
                if (present(matr_type)) then
                    load_option = optmat_f(i_type_neum)
                else
                    load_option = optsui_f(i_type_neum)
                endif
            else
                load_option = option_f(i_type_neum)
            endif
        else if (l_sigm_int) then
            load_option = option_r(i_type_neum)
            ASSERT(load_option.eq.'FORC_NODA')
        else
            ASSERT(.false.)
        endif
!
! ----- Name of input fields
!
        if (l_constant) then
            i_field_in = i_field_in+1
            lpain(i_field_in) = para_r(i_type_neum)
            lchin(i_field_in) = name_input(1:19)
            if (load_option .eq. 'CHAR_MECA_EFON_R') then
                i_field_in = i_field_in+1
                lpain(i_field_in) = 'PPREFFR'
                lchin(i_field_in) = load_name//'.CHME.PREFF'
            endif
        else if (l_fonct_0.or.l_fonct_t) then
            i_field_in = i_field_in+1
            lpain(i_field_in) = para_f(i_type_neum)
            lchin(i_field_in) = name_input(1:19)
            if (load_option .eq. 'CHAR_MECA_EFON_F') then
                i_field_in = i_field_in+1
                lpain(i_field_in) = 'PPREFFF'
                lchin(i_field_in) = load_name//'.CHME.PREFF'
            endif
        else if (l_sigm_int) then
            call jeveuo(ligrel_load(1:13)//'.SIINT.VALE', 'L', vk8=p_vale_sigm)
            i_field_in = i_field_in+1
            lpain(i_field_in) = 'PCONTMR'
            lchin(i_field_in) = p_vale_sigm(1)
        else
            ASSERT(.false.)
        endif
!
! ----- Parameter name of output field for matrix (undead loads)
!
        if (load_type.eq.'Suiv') then
            if (present(matr_type)) then
                matr_type = para_matr(i_type_neum)
            endif
        endif
!
! ----- Select LIGREL
!
        if (object(i_type_neum) .eq. '.FORNO') then
            load_ligrel = ligrel_load
        else
            load_ligrel = ligrel_calc
        endif
        if (load_option .eq. 'Copy_Load') then
            ASSERT((load_nume.ge.1.and.load_nume.le.3).or.load_nume.eq.5)
            call jeveuo(lchin(i_field_in), 'L', vk8 = p_vect_asse)
            load_ligrel = p_vect_asse(1)
        endif
!
! ----- Checking for undead loads
!
        if (load_type.eq.'Suiv') then
            if (.not.l_suiv(i_type_neum)) then
                call utmess('F', 'CHARGES_23', sk=load_name)
            endif
            if ((load_option.eq.'No_Load').and.(.not.present(matr_type))) then
                call utmess('F', 'CHARGES_23', sk=load_name)
            endif
        endif
!
! ----- Checking for continuation type loads
!
        if (load_type.eq.'Pilo') then
            if (.not.l_pilo(i_type_neum)) then
                call utmess('F', 'CHARGES_26', sk=load_name)
            endif
            if (l_fonct_t) then
                call utmess('F', 'CHARGES_28', sk=load_name)
            endif
        endif
!
! ----- Number of input fields which been added
!
        nb_in_add = i_field_in - nb_in_prep
!
        ASSERT(i_field_in.le.nb_in_maxi)
    endif
!
end subroutine
