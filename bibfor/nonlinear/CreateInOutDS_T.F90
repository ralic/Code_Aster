subroutine CreateInOutDS_T(ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
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
    type(NL_DS_InOut), intent(inout) :: ds_inout
!
! --------------------------------------------------------------------------------------------------
!
! THER_* - Input/output management
!
! Create input/output datastructure
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer, parameter :: nb_field_defi = 3
    integer :: i_field
! - Name of field (type) in results datastructure (add one -> don't forget to modify rscrsd.F90)
    character(len=16), parameter :: field_type(nb_field_defi) = &
            (/'TEMP            ','HYDR_ELNO       ','COMPORTHER      '/)
! - Type of GRANDEUR for field
    character(len=8), parameter :: gran_name(nb_field_defi) = &
            (/'TEMP_R  ','HYDR_R  ','COMPOR  '/)
! - Keyword for initial state (ETAT_INIT)
    character(len=8), parameter :: init_keyw(nb_field_defi) = &
            (/'CHAM_NO ','        ','        '/)
! - Spatial discretization of field
    character(len=4), parameter :: disc_type(nb_field_defi) = &
            (/'NOEU','ELNO','ELGA'/)
! - TRUE if field can been read for initial state (ETAT_INIT)
    aster_logical, parameter :: l_read_init(nb_field_defi) = &
            (/.true._1 ,.true._1 ,.false._1 /)
! - TRUE if field can been store (ARCHIVAGE)
    aster_logical, parameter :: l_store  (nb_field_defi) = &
            (/.true._1 ,.true._1 ,.true._1  /)
 ! - TRUE if field can been followed (OBSERVATION/SUIVI_DDL)
    aster_logical, parameter :: l_obsv  (nb_field_defi) = &
            (/.true._1 ,.false._1,.false._1 /)
! - Keyword for OBSERVATION
    character(len=16), parameter :: obsv_keyw(nb_field_defi) = &
            (/'TEMP            ','                ','                '/)
! - Variable (JEVEUX name) for field (#H# for hat variable)
    character(len=24), parameter :: algo_name(nb_field_defi) = &
            (/'#H#VALINC#TEMP  ','XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX'/)
! - Variable (JEVEUX name) for init field 
    character(len=24), parameter :: init_name(nb_field_defi) = &
            (/'&&NTETCR.TEMP0  ','XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX'/)
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<THERMIC> . Create input/output management datastructure'
    endif
!
! - Check
!
    ds_inout%nb_field = nb_field_defi
    ASSERT(ds_inout%nb_field.le.ds_inout%nb_field_maxi)
!
! - Set list of fields
!                                                        
    do i_field = 1, nb_field_defi
        ds_inout%field(i_field)%type            = field_type(i_field)
        ds_inout%field(i_field)%field_read      = ' '
        ds_inout%field(i_field)%gran_name       = gran_name(i_field)
        ds_inout%field(i_field)%obsv_keyw       = obsv_keyw(i_field)
        ds_inout%field(i_field)%init_keyw       = init_keyw(i_field)
        ds_inout%field(i_field)%disc_type       = disc_type(i_field)
        ds_inout%field(i_field)%l_read_init     = l_read_init(i_field)
        ds_inout%field(i_field)%l_store         = l_store(i_field)
        ds_inout%field(i_field)%l_obsv          = l_obsv(i_field)
        ds_inout%field(i_field)%algo_name       = algo_name(i_field)
        ds_inout%field(i_field)%init_name       = init_name(i_field)
        ds_inout%field(i_field)%init_type       = ' '
        ds_inout%l_field_read(i_field)          = .false._1
        ds_inout%l_field_acti(i_field)          = .false._1
    end do
!
end subroutine
