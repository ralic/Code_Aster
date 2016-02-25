subroutine nmini0(list_func_acti, eta      , nume_inst  , matass  , zmeelm    ,&
                  zmeass        , zveelm   , zveass     , zsolal  , zvalin    ,&
                  ds_print      , ds_conv  , ds_algopara, ds_inout, ds_contact,&
                  ds_measure    , ds_energy)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/nmchai.h"
#include "asterfort/infdbg.h"
#include "asterfort/CreateConvDS.h"
#include "asterfort/CreatePrintDS.h"
#include "asterfort/CreateAlgoParaDS.h"
#include "asterfort/CreateInOutDS.h"
#include "asterfort/CreateContactDS.h"
#include "asterfort/CreateMeasureDS.h"
#include "asterfort/CreateEnergyDS.h"
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
    integer, intent(out) :: list_func_acti(*)
    character(len=19), intent(out) :: matass
    integer, intent(out) :: nume_inst
    real(kind=8), intent(out) :: eta
    integer, intent(in) :: zmeelm
    integer, intent(in) :: zmeass
    integer, intent(in) :: zveelm
    integer, intent(in) :: zveass
    integer, intent(in) :: zsolal
    integer, intent(in) :: zvalin
    type(NL_DS_Print), intent(out) :: ds_print
    type(NL_DS_Conv), intent(out) :: ds_conv
    type(NL_DS_AlgoPara), intent(out) :: ds_algopara
    type(NL_DS_InOut), intent(out) :: ds_inout
    type(NL_DS_Contact), intent(out) :: ds_contact
    type(NL_DS_Measure), intent(out) :: ds_measure
    type(NL_DS_Energy), intent(out) :: ds_energy
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Initializations
!
! Creation of datastructures
!
! --------------------------------------------------------------------------------------------------
!
! Out list_func_acti   : list of active functionnalities
! Out nume_inst        : index of current time step
! Out ds_print         : datastructure for printing parameters
! Out ds_conv          : datastructure for convergence management
! Out ds_algopara      : datastructure for algorithm parameters
! Out ds_inout         : datastructure for input/output management
! Out ds_contact       : datastructure for contact management
! Out ds_measure       : datastructure for measure and statistics management
! Out ds_energy        : datastructure for energy management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    real(kind=8), parameter :: zero = 0.d0
    integer :: long
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> Create datastructures'
    endif
!
! - Create printing management datastructure
!
    call CreatePrintDS(ds_print)
!
! - Create convergence management datastructure
!
    call CreateConvDS(ds_conv)
!
! - Create algorithm parameters datastructure
!
    call CreateAlgoParaDS(ds_algopara)
!
! - Create input/output management datastructure
!
    call CreateInOutDS('MECA', ds_inout)
!
! - Create contact management datastructure
!
    call CreateContactDS(ds_contact)
!
! - Create measure and statistics management datastructure
!
    call CreateMeasureDS(ds_measure)
!
! - Create energy management datastructure
!
    call CreateEnergyDS(ds_energy)
!
! --- FONCTIONNALITES ACTIVEES               (NMFONC/ISFONC)
!
    list_func_acti(1:100) = 0
!
! --- INITIALISATION BOUCLE EN TEMPS
!
    nume_inst = 0
    eta    = zero
    matass = '&&OP0070.MATASS'
!
! --- VERIF. LONGUEURS VARIABLES CHAPEAUX (SYNCHRO OP0070/NMCHAI)
!
    call nmchai('MEELEM', 'LONMAX', long)
    ASSERT(long.eq.zmeelm)
    call nmchai('MEASSE', 'LONMAX', long)
    ASSERT(long.eq.zmeass)
    call nmchai('VEELEM', 'LONMAX', long)
    ASSERT(long.eq.zveelm)
    call nmchai('VEASSE', 'LONMAX', long)
    ASSERT(long.eq.zveass)
    call nmchai('SOLALG', 'LONMAX', long)
    ASSERT(long.eq.zsolal)
    call nmchai('VALINC', 'LONMAX', long)
    ASSERT(long.eq.zvalin)
!
end subroutine
