subroutine nmnpas(modele    , noma  , mate  , carele    , fonact    ,&
                  ds_print  , sddisc, sdsuiv, sddyna    , sdnume    ,&
                  ds_measure, numedd, numins, ds_contact, ds_algorom,&
                  valinc    , solalg, solveu, ds_conv   , lischa    )
!
use NonLin_Datastructure_type
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/copisd.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/initia.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/cldual_maj.h"
#include "asterfort/cont_init.h"
#include "asterfort/ndnpas.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmimin.h"
#include "asterfort/nmnkft.h"
#include "asterfort/nmvcle.h"
#include "asterfort/SetResi.h"
#include "asterfort/romAlgoNLReduCoorInit.h"
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
    integer :: fonact(*)
    character(len=8) :: noma
    character(len=19) :: sddyna, sdnume, sddisc, solveu
    character(len=24) :: modele, mate, carele
    integer :: numins
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=24) :: sdsuiv
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: numedd
    type(NL_DS_Contact), intent(inout) :: ds_contact
    type(ROM_DS_AlgoPara), intent(in) :: ds_algorom
    character(len=19) :: solalg(*), valinc(*)
    type(NL_DS_Conv), intent(inout) :: ds_conv
    character(len=19), intent(in) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! INITIALISATIONS POUR LE NOUVEAU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
! IN  MODELE : NOM DU MODELE
! IN  NOMA   : NOM DU MAILLAGE
! IN  MATE   : CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  NUMEDD : NUME_DDL
! IN  NUMINS : NUMERO INSTANT COURANT
! IO  ds_print         : datastructure for printing parameters
! IO  ds_measure       : datastructure for measure and statistics management
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDSUIV : SD SUIVI_DDL
! IN  SDNUME : NOM DE LA SD NUMEROTATION
! IO  ds_contact       : datastructure for contact management
! In  ds_algorom       : datastructure for ROM parameters
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IO  ds_conv          : datastructure for convergence management
!
! ----------------------------------------------------------------------
!
    aster_logical :: lgrot, ldyna, lnkry
    aster_logical :: l_cont, l_cont_cont, l_diri_undead, scotch
    integer :: neq
    character(len=19) :: depmoi, varmoi
    character(len=19) :: depplu, varplu
    character(len=19) :: complu, depdel
    real(kind=8) :: instan
    integer :: jdepde
    integer :: indro
    integer :: iterat
    real(kind=8), pointer :: depp(:) => null()
!
! ----------------------------------------------------------------------
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    scotch = .false.
!
! - Active functionnalites
!
    ldyna         = ndynlo(sddyna,'DYNAMIQUE')
    l_cont        = isfonc(fonact,'CONTACT')
    lgrot         = isfonc(fonact,'GD_ROTA')
    lnkry         = isfonc(fonact,'NEWTON_KRYLOV')
    l_cont_cont   = isfonc(fonact,'CONT_CONTINU')
    l_diri_undead = isfonc(fonact,'DIRI_UNDEAD')
!
! --- INSTANT COURANT
!
    instan = diinst(sddisc,numins)
!
! - Print management - Initializations for new step time
!
    call nmimin(fonact, sddisc, sdsuiv, numins, ds_print)
!
! --- POUTRES EN GRANDES ROTATIONS
!
    if (lgrot) then
        call jeveuo(sdnume(1:19)//'.NDRO', 'L', indro)
    else
        indro = isnnem()
    endif
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VARMOI', varmoi)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VARPLU', varplu)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
!
! --- TRAITEMENT DES VARIABLES DE COMMANDE
!
    call nmvcle(modele, mate, carele, instan, complu)
!
! --- ESTIMATIONS INITIALES DES VARIABLES INTERNES
!
    call copisd('CHAMP_GD', 'V', varmoi, varplu)
!
! --- INITIALISATION DES DEPLACEMENTS
!
    call copisd('CHAMP_GD', 'V', depmoi, depplu)
!
! - Initializations of residuals
!
    call SetResi(ds_conv, vale_calc_ = r8vide())
!
! --- INITIALISATION DE L'INCREMENT DE DEPLACEMENT DEPDEL
!
    call jeveuo(depdel//'.VALE', 'E', jdepde)
    call jeveuo(depplu//'.VALE', 'L', vr=depp)
    call initia(neq, lgrot, zi(indro), depp, zr(jdepde))
!
! - Update dualized relations for non-linear Dirichlet boundary conditions (undead)
!
    if (l_diri_undead) then
        call cldual_maj(lischa, depmoi)
    endif
!
! --- INITIALISATIONS EN DYNAMIQUE
!
    if (ldyna) then
        if (l_cont_cont) then
            scotch = ds_contact%l_getoff
        else
            scotch = .false._1
        endif
        call ndnpas(fonact, numedd, numins, sddisc, sddyna,&
                    scotch, valinc, solalg)
    endif
!
! --- NEWTON-KRYLOV : COPIE DANS LA SD SOLVEUR DE LA PRECISION DE LA
!                     RESOLUTION POUR LA PREDICTION (FORCING-TERM)
    if (lnkry) then
        iterat=-1
        call nmnkft(solveu, sddisc, iterat)
    endif
!
! - Initializations of contact for current time step
!
    if (l_cont) then
        call cont_init(noma  , modele, ds_contact, numins, ds_measure,&
                       sddyna, valinc, sdnume    , fonact)
    endif
!
! - Initializations of reduced coordinates (ROM)
!
    if (ds_algorom%l_rom) then
        call romAlgoNLReduCoorInit(ds_algorom)
    endif
!
end subroutine
