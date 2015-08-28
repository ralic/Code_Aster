subroutine nmini0(list_func_acti, eta    , nume_inst  , matass, zmeelm,&
                  zmeass        , zveelm , zveass     , zsolal, zvalin,&
                  ds_print      , ds_conv, ds_algopara)
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
