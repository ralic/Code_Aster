subroutine load_neut_spec(type_ther  , model        , time       , load_name , load_nume,&
                          i_type_neum, nb_type_neumz, nb_in_maxi , nb_in_prep, lchin    ,&
                          lpain      , nb_in_add    , load_ligrel,&
                          load_option,&
                          time_move_ )
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/exixfe.h"
#include "asterfort/jeexin.h"
#include "asterfort/xajcin.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=4), intent(in) :: type_ther
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: time
    character(len=8), intent(in) :: load_name
    integer, intent(in) :: load_nume
    integer, intent(in) :: i_type_neum
    integer, intent(in) :: nb_type_neumz
    integer, intent(in) :: nb_in_maxi
    integer, intent(in) :: nb_in_prep
    character(len=*), intent(inout) :: lpain(nb_in_maxi)
    character(len=*), intent(inout) :: lchin(nb_in_maxi)
    integer, intent(out) :: nb_in_add
    character(len=19), intent(out) :: load_ligrel
    character(len=16), intent(out) :: load_option
    character(len=24), optional, intent(in) :: time_move_
!
! --------------------------------------------------------------------------------------------------
!
! Neumann loads computation - Thermic
!
! Get information about load (Neumann)
!
! --------------------------------------------------------------------------------------------------
!
! In  type_ther        : type of thermics
!                        'MOVE' for moving sources
!                        'STAT' if not
! In  model            : name of the model
! In  time             : time (<CARTE>)
! In  time_move        : modified time (<CARTE>) for THER_NON_LINE_MO
! In  load_name        : name of current load
! In  load_nume        : identification of load type
! In  i_type_neum      : index for Neumann load type
! In  nb_type_neumz    : maximum number of Neumann load type
! In  nb_in_maxi       : maximum number of input fields
! In  nb_in_prep       : number of input fields before specific ones
! IO  lpain            : list of input parameters
! IO  lchin            : list of input fields
! Out nb_in_add        : number of input fields which been added
! Out load_ligrel      : name of LIGREL for current load
! Out load_option      : name of option for current load
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_type_neum
    parameter (nb_type_neum=6)
    character(len=6) :: object(nb_type_neum)
    character(len=6) :: ligrel(nb_type_neum)
    character(len=7) :: para_r(nb_type_neum), para_f(nb_type_neum)
    character(len=16) :: option_f(nb_type_neum), option_r(nb_type_neum)
    character(len=24) :: keyw(nb_type_neum)   
!
    integer :: i_field_in
    character(len=19) :: ligrel_load, ligrel_model, name_input, name_input_comp
    logical :: l_constant, l_fonct_0, l_fonct_t
    integer :: ier, iret, ireth
    logical :: l_xfem
!
! - Keyword in AFFE_CHAR_THER
!
    data keyw     /'ECHANGE'      ,'FLUX_REP_XYZ' ,'FLUX_REP_NORM','SOURCE',&
                   'ECHANGE_PAROI','PRE_GRAD_TEMP'/
!
! - Object name construct in AFFE_CHAR_THER
!
    data object   /'.T_EXT','.FLURE','.FLUR2','.SOURE',&
                   '.HECHP','.GRAIN'/
!
! - Type of LIGREL to compute
!
    data ligrel   /'Model','Model','Model','Model',&
                   'Load','Model'/
!
! - Name of option (real coefficient)
!
    data option_r /'CHAR_THER_TEXT_R','CHAR_THER_FLUN_R','CHAR_THER_FLUX_R','CHAR_THER_SOUR_R',&
                   'CHAR_THER_PARO_R','CHAR_THER_GRAI_R'/
!
! - Name of option (function coefficient)
!
    data option_f /'CHAR_THER_TEXT_F','CHAR_THER_FLUN_F','CHAR_THER_FLUX_F','CHAR_THER_SOUR_F',&
                   'CHAR_THER_PARO_F','CHAR_THER_GRAI_F'/
!
! - Name of input parameter field (real coefficient)
!
    data para_r   /'PT_EXTR','PFLUXNR','PFLUXVR','PSOURCR',&
                   'PHECHPR','PGRAINR'/
!
! - Name of input parameter field (function coefficient)
!
    data para_f   /'PT_EXTF','PFLUXNF','PFLUXVF','PSOURCF',&
                   'PHECHPF','PGRAINF'/
!
! --------------------------------------------------------------------------------------------------
!
    ligrel_model = model(1:8)//'.MODELE'
    ligrel_load  = load_name(1:8)//'.CHTH.LIGRE'
    call exixfe(model, ier)
    l_xfem       = ier.ne.0
    load_ligrel  = ' '
    load_option  = 'No_Load'
    l_constant   = .false.
    l_fonct_0    = .false.
    l_fonct_t    = .false.
    i_field_in   = nb_in_prep
    ASSERT(i_type_neum.le.nb_type_neum)
    ASSERT(nb_type_neumz.eq.nb_type_neum)
!
! - Identify current load
!
    iret = 0
    name_input = load_name(1:8)//'.CHTH'//object(i_type_neum)
    call exisd('CHAMP_GD', name_input, iret)
!
    if (iret .ne. 0) then
!
! ----- Value type
!
        if (load_nume .eq. 1) then
            l_constant = .true.
        else if (load_nume .eq. 2) then
            l_fonct_0  = .true.
        else if (load_nume .eq. 3) then
            l_fonct_t  = .true.               
        else
            ASSERT(.false.)
        endif
!
! ----- Name of option
!
        if (l_constant) then
            load_option = option_r(i_type_neum)
        else if (l_fonct_0.or.l_fonct_t) then
            load_option = option_f(i_type_neum)
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
        else if (l_fonct_0.or.l_fonct_t) then
            i_field_in = i_field_in+1
            lpain(i_field_in) = para_f(i_type_neum)
            lchin(i_field_in) = name_input(1:19)
        else
            ASSERT(.false.)
        endif
!
! ----- For ECHANGE
!
        if (keyw(i_type_neum) .eq. 'ECHANGE') then
            name_input_comp = load_name(1:8)//'.CHTH.COEFH'
            call exisd('CHAMP_GD', name_input_comp, ireth)
            ASSERT(ireth.ne.0)
            i_field_in = i_field_in+1
            if (l_constant) then
                lpain(i_field_in) = 'PCOEFHR'
            else if (l_fonct_0.or.l_fonct_t) then
                lpain(i_field_in) = 'PCOEFHF'
            else
                ASSERT(.false.)
            endif
            lchin(i_field_in) = name_input_comp(1:19)
        endif   
!
! ----- For PAROI
!
        i_field_in = i_field_in+1
        lpain(i_field_in) = 'PTEMPSR'
        lchin(i_field_in) = time
        if (keyw(i_type_neum) .eq. 'ECHANGE_PAROI') then
            if (type_ther.eq.'MOVE') then
                lchin(i_field_in) = time_move_
            endif
        endif
!
! ----- XFEM fields
!
        if (l_xfem) then
            call xajcin(model     , load_option, nb_in_maxi, lchin, lpain,&
                        i_field_in)
        endif
!
! ----- Select LIGREL
!
        if (l_xfem) then
            load_ligrel = ligrel_model
        else
            if (ligrel(i_type_neum).eq.'Load') then
                load_ligrel = ligrel_load
            elseif (ligrel(i_type_neum).eq.'Model') then
                load_ligrel = ligrel_model
            else
                ASSERT(.false.)
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
