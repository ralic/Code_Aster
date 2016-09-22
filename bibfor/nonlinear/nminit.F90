subroutine nminit(mesh      , model     , mate       , cara_elem      , list_load ,&
                  numedd    , numfix    , ds_algopara, ds_constitutive, maprec    ,&
                  solver    , numins    , sddisc     , sdnume         , sdcrit    ,&
                  varc_refe , fonact    , sdpilo     , sddyna         , ds_print  ,&
                  sd_suiv   , sd_obsv   , sderro     , sdpost         , ds_inout  ,&
                  ds_energy , ds_conv   , sdcriq     , valinc         , solalg    ,&
                  measse    , veelem    , meelem     , veasse         , ds_contact,&
                  ds_measure, ds_algorom)
!
use NonLin_Datastructure_type
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/accel0.h"
#include "asterfort/assert.h"
#include "asterfort/cetule.h"
#include "asterfort/cfmxsd.h"
#include "asterfort/cucrsd.h"
#include "asterfort/diinit.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/exfonc.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liscpy.h"
#include "asterfort/lobs.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchap.h"
#include "asterfort/nmchar.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcrch.h"
#include "asterfort/nmcrcv.h"
#include "asterfort/nmcrob.h"
#include "asterfort/nmcrdd.h"
#include "asterfort/nmcrti.h"
#include "asterfort/nmdidi.h"
#include "asterfort/nmdoct.h"
#include "asterfort/nmdoet.h"
#include "asterfort/nmdopi.h"
#include "asterfort/nmetcr.h"
#include "asterfort/nmetpl.h"
#include "asterfort/nmexso.h"
#include "asterfort/nmfonc.h"
#include "asterfort/nmihht.h"
#include "asterfort/InitAlgoPara.h"
#include "asterfort/InitContact.h"
#include "asterfort/InitConv.h"
#include "asterfort/InitEnergy.h"
#include "asterfort/InitPrint.h"
#include "asterfort/romAlgoNLInit.h"
#include "asterfort/nonlinDSConstitutiveInit.h"
#include "asterfort/nmrefe.h"
#include "asterfort/nminma.h"
#include "asterfort/nminmc.h"
#include "asterfort/nminvc.h"
#include "asterfort/nmlssv.h"
#include "asterfort/nmnoli.h"
#include "asterfort/nmnume.h"
#include "asterfort/nmobsv.h"
#include "asterfort/nmpro2.h"
#include "asterfort/nmrini.h"
#include "asterfort/nmvcle.h"
#include "asterfort/nmvcre.h"
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
! aslint: disable=W1504
!
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=19), intent(in) :: list_load
    character(len=24) :: numedd
    character(len=24) :: numfix
    type(NL_DS_AlgoPara), intent(inout) :: ds_algopara
    type(NL_DS_Constitutive), intent(inout) :: ds_constitutive
    character(len=19) :: maprec
    character(len=19), intent(in) :: solver
    integer :: numins
    character(len=19) :: sddisc
    character(len=19) :: sdnume
    character(len=19) :: sdcrit
    character(len=24) :: varc_refe
    integer :: fonact(*)
    character(len=19) :: sdpilo
    character(len=19) :: sddyna
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=24), intent(out) :: sd_suiv
    character(len=19), intent(out) :: sd_obsv
    character(len=24) :: sderro
    character(len=19) :: sdpost
    type(NL_DS_InOut), intent(inout) :: ds_inout
    type(NL_DS_Energy), intent(inout) :: ds_energy
    type(NL_DS_Conv), intent(inout) :: ds_conv
    character(len=24) :: sdcriq
    character(len=19) :: valinc(*)
    character(len=19) :: solalg(*)
    character(len=19) :: measse(*)
    character(len=19) :: veelem(*)
    character(len=19) :: meelem(*)
    character(len=19) :: veasse(*)
    type(NL_DS_Contact), intent(inout) :: ds_contact
    type(NL_DS_Measure), intent(inout) :: ds_measure
    type(ROM_DS_AlgoPara), intent(inout) :: ds_algorom
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Initializations
!
! Initializations of datastructures
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  list_load        : name of datastructure for list of loads
! IO  ds_algopara      : datastructure for algorithm parameters
! IO  ds_constitutive  : datastructure for constitutive laws management
! In  solver           : name of datastructure for solver
! IO  ds_print         : datastructure for printing parameters
! Out sd_suiv          : datastructure for dof monitoring parameters
! Out sd_obsv          : datastructure for observation parameters
! IO  ds_inout         : datastructure for input/output management
! IO  ds_energy        : datastructure for energy management
! IO  ds_inout         : datastructure for input/output management
! Out sd_obsv          : datastructure for observation parameters
! Out sd_suiv          : datastructure for dof monitoring parameters
! IO  ds_print         : datastructure for printing parameters
! IO  ds_conv          : datastructure for convergence management
! IO  ds_algopara      : datastructure for algorithm parameters
! IO  ds_contact       : datastructure for contact management
! IO  ds_measure       : datastructure for measure and statistics management
! IO  ds_algorom       : datastructure for ROM parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, ibid
    real(kind=8) :: r8bid3(3)
    real(kind=8) :: instin
    character(len=19) :: varc_prev, disp_prev, strx_prev
    aster_logical :: lacc0, lpilo, lmpas, lsstf, lerrt, lviss, lrefe, ldidi, l_obsv
!
! --------------------------------------------------------------------------------------------------
!
    lacc0 = .false.
!
! - Initializations for contact parameters
!
    call InitContact(mesh, model, ds_contact)
!
! - Initializations for constitutive laws
!
    call nonlinDSConstitutiveInit(model, cara_elem, ds_constitutive)
!
! - Prepare list of loads (and late elements) for contact
!
    call nmdoct(list_load, ds_contact)
!
! - Create information about numbering
!
    call nmnume(model     , mesh  , ds_inout%result, ds_constitutive%compor, list_load,&
                ds_contact, numedd, sdnume)
!
! --- CREATION DE VARIABLES "CHAPEAU" POUR STOCKER LES NOMS
!
    call nmchap(valinc, solalg, meelem, veelem, veasse,&
                measse)
!
! - Prepare active functionnalities information
!
    call nmfonc(ds_conv  , ds_algopara    , solver   , model     , ds_contact,&
                list_load, sdnume         , sddyna   , sdcriq    , mate      ,&
                ds_inout , ds_constitutive, ds_energy, ds_algorom, fonact)
!
! - Check compatibility of some functionnalities
!
    call exfonc(fonact, ds_algopara, solver, ds_contact, sddyna, mate)
    lpilo = isfonc(fonact,'PILOTAGE' )
    lmpas = ndynlo(sddyna,'MULTI_PAS' )
    lsstf = isfonc(fonact,'SOUS_STRUC')
    lerrt = isfonc(fonact,'ERRE_TEMPS_THM')
    lviss = ndynlo(sddyna,'VECT_ISS' )
    lrefe = isfonc(fonact,'RESI_REFE')
    ldidi = isfonc(fonact,'DIDI')
!
! - Initialization for reduced method
!
    if (ds_algorom%l_rom) then
        call romAlgoNLInit('MECA', mesh, numedd, ds_inout%result, ds_algorom)
    endif
!
! - Prepare contact solving datastructure
!
    if (ds_contact%l_meca_cont) then
        call cfmxsd(mesh      , model, numedd, fonact, sddyna,&
                    ds_contact)
    endif
!
! --- CREATION DE LA STRUCTURE DE LIAISON_UNILATERALE
!
    if (ds_contact%l_meca_unil) then
        call cucrsd(mesh, numedd, ds_contact)
    endif
!
! - Initializations for measure and statistic management
!
    call nmcrti(fonact, ds_inout%result, ds_contact, ds_measure)
!
! - Initializations for algorithm parameters
!
    call InitAlgoPara(fonact, ds_algopara)
!
! - Initializations for convergence management
!
    call InitConv(ds_conv, fonact, ds_contact)
!
! - Initializations for energy management
!
    call InitEnergy(ds_inout%result, ds_energy)
!
! --- CREATION DES VECTEURS D'INCONNUS
!
    call nmcrch(numedd, fonact, sddyna, ds_contact, valinc,&
                solalg, veasse)
!
! --- CONSTRUCTION DU CHAM_NO ASSOCIE AU PILOTAGE
!
    if (lpilo) then
        call nmdopi(model, numedd, ds_algopara, sdpilo)
    endif
!
! --- DUPLICATION NUME_DDL POUR CREER UN DUME_DDL FIXE
!
    call nmpro2(fonact, numedd, numfix)
!
! - Create input/output datastructure
!
    call nmetcr(ds_inout, model     , ds_constitutive%compor, fonact   , sddyna   ,&
                sdpost  , ds_contact, cara_elem, list_load)
!
! - Read initial state
!
    call nmdoet(model , ds_constitutive%compor, fonact, numedd, sdpilo  ,&
                sddyna, sdcriq, solalg, lacc0 , ds_inout)
!
! - Create time discretization and storing datastructures
!
    call diinit(mesh      , model , ds_inout, mate       , cara_elem,&
                fonact    , sddyna, ds_conv , ds_algopara, solver,&
                ds_contact, sddisc)
!
! --- CREATION DU CHAMP DES VARIABLES DE COMMANDE DE REFERENCE
!
    call nmvcre(model, mate, cara_elem, varc_refe)
!
! --- PRE-CALCUL DES MATR_ELEM CONSTANTES AU COURS DU CALCUL
!
    call nminmc(fonact   , list_load, sddyna    , model      , ds_constitutive,&
                numedd   , numfix   , ds_contact, ds_algopara, solalg         ,&
                valinc   , mate     , cara_elem    , sddisc     , ds_measure     ,&
                varc_refe, meelem   , measse    , veelem)
!
! --- INSTANT INITIAL
!
    numins = 0
    instin = diinst(sddisc,numins)
!
! --- EXTRACTION VARIABLES DE COMMANDES AU TEMPS T-
!
    call nmchex(valinc, 'VALINC', 'COMMOI', varc_prev)
    call nmvcle(model , mate, cara_elem, instin, varc_prev)
!
! --- CALCUL ET ASSEMBLAGE DES VECT_ELEM CONSTANTS AU COURS DU CALCUL
!
    call nminvc(model    , mate  , cara_elem  , ds_constitutive, ds_measure,&
                sddisc   , sddyna, valinc  , solalg         , list_load ,&
                varc_refe, numedd, ds_inout, veelem         , veasse    ,&
                measse)
!
! - Compute reference vector for RESI_REFE_RELA
!
    if (lrefe) then
        call nmrefe(model  , ds_constitutive%compor, mate  , cara_elem, numedd,&
                    ds_conv, valinc                , veelem, veasse)
    endif
!
! - Compute vector for DIDI loads
!
    if (ldidi) then
        call nmdidi(ds_inout, model , list_load, numedd, valinc,&
                    veelem  , veasse)
    endif 
!
! --- CREATION DE LA SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
!
    call nmcrcv(sdcrit)
!
! --- INITIALISATION CALCUL PAR SOUS-STRUCTURATION
!
    if (lsstf) then
        call nmlssv('INIT', list_load, ibid)
    endif
!
! --- CREATION DE LA SD EXCIT_SOL
!
    if (lviss) then
        call nmexso(mesh, ds_inout, sddyna, numedd)
    endif
!
! --- CALCUL DE L'ACCELERATION INITIALE
!
    if (lacc0) then
        call nmchar('ACCI'  , ' '   , model    , numedd, mate     ,&
                    cara_elem  , ds_constitutive, list_load, numins, ds_measure   ,&
                    sddisc  , fonact, varc_refe,&
                    ds_inout, valinc, solalg   , veelem, measse   ,&
                    veasse  , sddyna)
        call accel0(model     , numedd, numfix     , fonact, list_load,&
                    ds_contact, maprec, solver     , valinc, sddyna   ,&
                    ds_measure, ds_algopara, meelem, measse   ,&
                    veelem    , veasse, solalg)
    endif
!
! - Extract variables
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', disp_prev)
    call nmchex(valinc, 'VALINC', 'STRMOI', strx_prev)
!
! - Create observation datastructure
!
    call nmcrob(mesh     , model          , sddisc   , ds_inout , cara_elem   ,&
                mate     , ds_constitutive, disp_prev, strx_prev, varc_prev,&
                varc_refe, instin         , sd_obsv  )
!
! - Create dof monitoring datastructure
!
    call nmcrdd(mesh           , model    , ds_inout , cara_elem   , mate     ,&
                ds_constitutive, disp_prev, strx_prev, varc_prev, varc_refe,&
                instin, sd_suiv)
!
! - Initializations for printing
!
    call InitPrint(sd_suiv, ds_print)
!
! --- PRE-CALCUL DES MATR_ASSE CONSTANTES AU COURS DU CALCUL
!
    call nminma(fonact, list_load, sddyna, numedd, ds_algopara,&
                numfix, meelem   , measse)
!
! - Prepare storing
!
    call nmnoli(sddisc, sderro, ds_constitutive, ds_print , sdcrit  ,&
                fonact, sddyna, sdpost         , model    , mate    ,&
                cara_elem, sdpilo, ds_measure     , ds_energy, ds_inout,&
                sdcriq)
!
! - Make initial observation
!
    l_obsv = .false.
    call lobs(sd_obsv, numins, instin, l_obsv)
    if (l_obsv) then
        call nmobsv(mesh     , model, sddisc         , sd_obsv  , numins,&
                    cara_elem, mate , ds_constitutive, varc_refe, valinc,&
                    ds_inout)
    endif
!
! - Update name of fields
!
    call nmetpl(ds_inout, sd_suiv, sd_obsv)
!
! --- CREATION DE LA TABLE DES GRANDEURS
!
    if (lerrt) then
        call cetule(model, r8bid3, iret)
    endif
!
! --- CALCUL DU SECOND MEMBRE INITIAL POUR MULTI-PAS
!
    if (lmpas) then
        call nmihht(model , numedd   , mate     , ds_constitutive,&
                    cara_elem, list_load, varc_refe, fonact    , ds_measure   ,&
                    sddyna,  sdnume   , ds_contact, valinc,&
                    sddisc, solalg   , veasse   , measse    , ds_inout)
    endif
!
! - Reset times and counters
!
    call nmrini(ds_measure, 'T')
!
end subroutine
