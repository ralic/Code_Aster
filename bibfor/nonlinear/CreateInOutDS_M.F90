subroutine CreateInOutDS_M(ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
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
! MECA_NON_LINE - Input/output management
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
    integer, parameter :: nb_field_defi = 21
    integer :: i_field
! - Name of field (type) in results datastructure (add one -> don't forget to modify rscrsd.F90)
    character(len=16), parameter :: field_type(nb_field_defi) = &
            (/'DEPL            ','SIEF_ELGA       ','VARI_ELGA       ',&
              'COMPORTEMENT    ','VITE            ','ACCE            ',&
              'INDC_ELEM       ','SECO_ELEM       ','COHE_ELEM       ',&
              'CONT_NOEU       ','MODE_FLAMB      ','DEPL_VIBR       ',&
              'DEPL_ABSOLU     ','VITE_ABSOLU     ','ACCE_ABSOLU     ',&
              'FORC_NODA       ','STRX_ELGA       ','MODE_STAB       ',&
              'FORC_AMOR       ','FORC_LIAI       ','EPSI_ELGA       '/)
! - Type of GRANDEUR for field
    character(len=8), parameter :: gran_name(nb_field_defi) = &
            (/'DEPL_R  ','SIEF_R  ','VARI_R  ',&
              'COMPOR  ','DEPL_R  ','DEPL_R  ',&
              'NEUT_I  ','NEUT_R  ','NEUT_R  ',&
              'DEPL_R  ','DEPL_R  ','DEPL_R  ',&
              'DEPL_R  ','DEPL_R  ','DEPL_R  ',&
              'DEPL_R  ','STRX_R  ','DEPL_R  ',&
              'DEPL_R  ','DEPL_R  ','EPSI_R  '/)
! - Keyword for initial state (ETAT_INIT)
    character(len=8), parameter :: init_keyw(nb_field_defi) = &
            (/'DEPL    ','SIGM    ','VARI    ',&
              '        ','VITE    ','ACCE    ',&
              '        ','        ','COHE    ',&
              '        ','        ','        ',&
              '        ','        ','        ',&
              '        ','STRX    ','        ',&
              '        ','        ','        '/)
! - Spatial discretization of field
    character(len=4), parameter :: disc_type(nb_field_defi) = &
            (/'NOEU','ELGA','ELGA',&
              'ELGA','NOEU','NOEU',&
              'ELEM','ELEM','XXXX',&
              'NOEU','NOEU','NOEU',&
              'NOEU','NOEU','NOEU',&
              'NOEU','ELGA','NOEU',&
              'NOEU','NOEU','ELGA'/)
! - TRUE if field can been read for initial state (ETAT_INIT)
    aster_logical, parameter :: l_read_init(nb_field_defi) = &
                                                (/.true._1,.true._1 ,.true._1 ,&
                                                 .false._1,.true._1 ,.true._1 ,&
                                                 .true._1 ,.true._1 ,.true._1 ,&
                                                 .false._1,.false._1,.false._1,&
                                                 .true._1 ,.true._1 ,.true._1 ,&
                                                 .false._1,.true._1 ,.false._1,&
                                                 .true._1 ,.true._1 ,.false._1/)
! - TRUE if field can been store (ARCHIVAGE)
    aster_logical, parameter :: l_store  (nb_field_defi) = &
                                               (/.true._1 ,.true._1,.true._1 ,&
                                                 .true._1 ,.true._1,.true._1 ,&
                                                 .true._1 ,.true._1,.true._1 ,&
                                                 .true._1 ,.true._1,.true._1 ,&
                                                 .true._1 ,.true._1,.true._1 ,&
                                                 .false._1,.true._1,.true._1 ,&
                                                 .true._1 ,.true._1,.false._1/)
 ! - TRUE if field can been followed (OBSERVATION/SUIVI_DDL)
    aster_logical, parameter :: l_obsv  (nb_field_defi) = &
                                               (/.true._1 ,.true._1 ,.true._1 ,&
                                                 .false._1,.true._1 ,.true._1 ,&
                                                 .false._1,.false._1,.false._1,&
                                                 .true._1 ,.false._1,.false._1,&
                                                 .true._1 ,.true._1 ,.true._1 ,&
                                                 .true._1 ,.true._1 ,.false._1,&
                                                 .false._1,.false._1,.true._1/)
! - Keyword for OBSERVATION
    character(len=16), parameter :: obsv_keyw(nb_field_defi) = &
            (/'DEPL            ','SIEF_ELGA       ','VARI_ELGA       ',&
              '                ','VITE            ','ACCE            ',&
              '                ','                ','                ',&
              'CONT_NOEU       ','                ','                ',&
              'DEPL_ABSOLU     ','VITE_ABSOLU     ','ACCE_ABSOLU     ',&
              'FORC_NODA       ','STRX_ELGA       ','                ',&
              '                ','                ','EPSI_ELGA       '/)
! - Variable (JEVEUX name) for field (#H# for hat variable)
    character(len=24), parameter :: algo_name(nb_field_defi) = &
            (/'#H#VALINC#DEPMOI','#H#VALINC#SIGMOI','#H#VALINC#VARMOI',&
              'XXXXXXXXXXXXXXXX','#H#VALINC#VITMOI','#H#VALINC#ACCMOI',&
              'XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX',&
              'XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX',&
              'XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX',&
              '#H#VEASSE#CNFINT','#H#VALINC#STRMOI','XXXXXXXXXXXXXXXX',&
              '#H#VALINC#FAMMOI','#H#VALINC#FLIMOI','&&NMETCR.EPSI   '/)
! - Variable (JEVEUX name) for init field 
    character(len=24), parameter :: init_name(nb_field_defi) = &
            (/'&&CNPART.ZERO   ','&&NMETCR.SIGMO0 ','&&NMETCR.VARMO0 ',&
              'XXXXXXXXXXXXXXXX','&&CNPART.ZERO   ','&&CNPART.ZERO   ',&
              'XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX',&
              'XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX','XXXXXXXXXXXXXXXX',&
              '&&CNPART.ZERO   ','&&CNPART.ZERO   ','&&CNPART.ZERO   ',&
              '&&CNPART.ZERO   ','&&NMETCR.STRMO0 ','XXXXXXXXXXXXXXXX',&
              '&&CNPART.ZERO   ','&&CNPART.ZERO   ','&&NMETCR.EPSI   '/)
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Create input/output management datastructure'
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

