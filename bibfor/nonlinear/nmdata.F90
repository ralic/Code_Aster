subroutine nmdata(result, model , mesh  , mate    , carele, &
                  compor, lischa, solveu, method  , parmet, &
                  parcri, parcon, carcri, sddyna  , sdpost, &
                  sderro, sdener, sdcriq, ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/eninit.h"
#include "asterfort/infdbg.h"
#include "asterfort/getvid.h"
#include "asterfort/ndcrdy.h"
#include "asterfort/ndlect.h"
#include "asterfort/nmcrer.h"
#include "asterfort/nmcrga.h"
#include "asterfort/nmdocn.h"
#include "asterfort/ReadPrint.h"
#include "asterfort/nmdomt.h"
#include "asterfort/nmdopo.h"
#include "asterfort/nmdorc.h"
#include "asterfort/nmetdo.h"
#include "asterfort/nmlect.h"
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
    character(len=8) :: result
    character(len=19) :: lischa, solveu, sddyna, sdpost, sdener
    character(len=24) :: mate, carele, compor
    character(len=24) :: carcri, sderro, sdcriq
    character(len=16) :: method(*)
    real(kind=8) :: parmet(*), parcri(*), parcon(*)
    character(len=*), intent(out) :: model
    character(len=*), intent(out) :: mesh
    type(NL_DS_Print), intent(inout) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE
!
! Read parameters
!
! --------------------------------------------------------------------------------------------------
!
! OUT RESULT : NOM UTILISATEUR DU RESULTAT DE MECA_NON_LINE
! Out mesh             : name of mesh
! Out model            : name of model
! OUT MATE   : NOM DU CHAMP DE MATERIAU
! OUT CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! OUT COMPOR : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
! OUT LISCHA : LISTE DES CHARGES
! OUT METHOD : DESCRIPTION DE LA METHODE DE RESOLUTION
! OUT SOLVEU : NOM DU SOLVEUR
! OUT PARMET : PARAMETRES DE LA METHODE DE RESOLUTION
! OUT PARCRI : PARAMETRES DES CRITERES DE CONVERGENCE
! OUT CARCRI : CARTE DES CRITERES DE CONVERGENCE LOCAUX
! OUT PARCON : PARAMETRES DU CRITERE DE CONVERGENCE EN CONTRAINTE
! IN  SDDYNA : SD DYNAMIQUE
! OUT SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! OUT SDERRO : SD ERREUR
! OUT SDCRIQ : SD CRITERE QUALITE
! OUT SDENER : SD ENERGIES
! IO  ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: n1, n2
    character(len=8) :: k8bid
    character(len=16) :: k16bid, nomcmd
    aster_logical :: l_etat_init
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> LECTURE DES DONNEES'
    endif
!
! --- COMMANDE APPELANTE
!
    call getres(k8bid, k16bid, nomcmd)
!
! - Initial state
!
    call getvid('ETAT_INIT', 'EVOL_NOLI', iocc=1, nbret=n1)
    call getvid('ETAT_INIT', 'SIGM', iocc=1, nbret=n2)
    l_etat_init = ((n1.ne.0).or.(n2.ne.0))
!
! --- LECTURE DONNEES GENERALES
!
    call nmlect(result, model, mate, carele, compor,&
                lischa, solveu)
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
!
! --- RELATION DE COMPORTEMENT ET CRITERES DE CONVERGENCE LOCAL
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... LECTURE DONNEES COMPORTEMENT'
    endif
    call nmdorc(model(1:8), mate, l_etat_init, compor, carcri)
!
! --- CRITERES DE CONVERGENCE GLOBAL
!
    call nmdocn(parcri, parcon)
!
! --- NOM ET PARAMETRES DE LA METHODE DE RESOLUTION
!
    call nmdomt(method, parmet)
!
! --- CREATION SD DYNAMIQUE
!
    call ndcrdy(result, sddyna)
!
! --- LECTURE DES OPERANDES DYNAMIQUES
!
    call ndlect(model, mate, carele, lischa, sddyna)
!
! --- LECTURE INFOS POST-TRAITEMENT (CRIT_STAB ET MODE_VIBR)
!
    call nmdopo(sddyna, method, sdpost)
!
! --- LECTURE INFOS ENERGIE
!
    call eninit(sdener)
!
! --- LECTURE DES DONNEES GESTION ALGORITHME
!
    call nmcrga(sderro)
!
! --- LECTURE DES DONNEES CRITERE QUALITE
!
    if (nomcmd .eq. 'STAT_NON_LINE') then
        call nmcrer(carcri, sdcriq)
        call nmetdo(sdcriq)
    endif
!
! - Read parameters for printing
!
    call ReadPrint(ds_print)
!
end subroutine
