subroutine nmini0(zpmet   , zpcri, zconv, zpcon, znmeth,&
                  fonact  , parmet, parcri, conv, parcon,&
                  method  , eta, numins, matass, zmeelm,&
                  zmeass  , zveelm, zveass, zsolal, zvalin,&
                  ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/nmchai.h"
#include "asterfort/CreatePrintDS.h"
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
! aslint: disable=W1504
!
    integer :: zpmet, zpcri, zconv
    integer :: zpcon, znmeth
    integer :: fonact(*)
    real(kind=8) :: parmet(zpmet), parcri(zpcri), conv(zconv)
    real(kind=8) :: parcon(zpcon)
    character(len=16) :: method(znmeth)
    character(len=19) :: matass
    integer :: numins
    real(kind=8) :: eta
    integer :: zmeelm, zmeass, zveelm, zveass, zsolal, zvalin
    type(NL_DS_Print), intent(out) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Initializations
!
! Creation of datastructures
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    real(kind=8), parameter :: zero = 0.d0
    integer :: long
!
! ----------------------------------------------------------------------
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
! --- FONCTIONNALITES ACTIVEES               (NMFONC/ISFONC)
!
    fonact(1:100) = 0
!
! --- PARAMETRES DES METHODES DE RESOLUTION  (NMDOMT)
!
    parmet(1:zpmet) = zero
!
! --- PARAMETRES DES CRITERES DE CONVERGENCE (NMLECT)
!
    parcri(1:zpcri) = zero
!
! --- INFORMATIONS SUR LA CONVERGENCE DU CALCUL
!
    conv(1) = -1
    conv(2:zconv) = r8vide()
!
! --- PARAMETRES DU CRITERE DE CONVERGENCE EN CONTRAINTE (NMLECT)
!
    parcon(1:zpcon) = zero
!
! --- METHODES DE RESOLUTION
!
    method(1:znmeth) = ' '
!
! --- INITIALISATION BOUCLE EN TEMPS
!
    numins = 0
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
