subroutine nmnewt(mesh       , model    , numins    , numedd         , numfix   ,&
                  mate       , cara_elem, comref    , ds_constitutive, list_load,&
                  ds_algopara, fonact   , ds_measure, sderro         , ds_print ,&
                  sdnume     , sddyna   , sddisc    , sdcrit         , sdsuiv   ,&
                  sdpilo     , ds_conv  , solveu    , maprec         , matass   ,&
                  ds_inout   , valinc   , solalg    , meelem         , measse   ,&
                  veelem     , veasse   , ds_contact, ds_algorom     , eta      ,&
                  nbiter  )
!
use NonLin_Datastructure_type
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmactf.h"
#include "asterfort/nmactn.h"
#include "asterfort/nmaffi.h"
#include "asterfort/nmchar.h"
#include "asterfort/nmconv.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmcvgf.h"
#include "asterfort/nmcvgn.h"
#include "asterfort/nmdepl.h"
#include "asterfort/nmdesc.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmeraz.h"
#include "asterfort/nmevdt.h"
#include "asterfort/nmevr0.h"
#include "asterfort/nmfcon.h"
#include "asterfort/nmfcor.h"
#include "asterfort/nmible.h"
#include "asterfort/nmimci.h"
#include "asterfort/nmimcr.h"
#include "asterfort/nmimr0.h"
#include "asterfort/nmleeb.h"
#include "asterfort/nmnble.h"
#include "asterfort/nmnpas.h"
#include "asterfort/nmpred.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmrini.h"
#include "asterfort/nmstat.h"
#include "asterfort/nmsuiv.h"
#include "asterfort/nmtble.h"
#include "asterfort/nmtime.h"
#include "asterfort/nmtimr.h"
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
    integer :: numins
    character(len=24) :: numedd
    character(len=24) :: numfix
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=24) :: comref
    type(NL_DS_Constitutive), intent(inout) :: ds_constitutive
    character(len=19), intent(in) :: list_load
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    integer :: fonact(*)
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: sderro
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=19) :: sdnume
    character(len=19) :: sddyna
    character(len=19) :: sddisc
    character(len=19) :: sdcrit
    character(len=24) :: sdsuiv
    character(len=19) :: sdpilo
    type(NL_DS_Conv), intent(inout) :: ds_conv
    character(len=19) :: solveu
    character(len=19) :: maprec
    character(len=19) :: matass
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=19) :: valinc(*)
    character(len=19) :: solalg(*)
    character(len=19) :: meelem(*)
    character(len=19) :: measse(*)
    character(len=19) :: veelem(*)
    character(len=19) :: veasse(*)
    type(NL_DS_Contact), intent(inout) :: ds_contact
    type(ROM_DS_AlgoPara), intent(inout) :: ds_algorom
    real(kind=8) :: eta
    integer :: nbiter
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algorithm
!
! Newton
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
! In  sd_suiv          : datastructure for dof monitoring parameters
! In  sd_obsv          : datastructure for observation parameters
! IO  ds_inout         : datastructure for input/output management
! IO  ds_energy        : datastructure for energy management
! IO  ds_conv          : datastructure for convergence management
! IO  ds_contact       : datastructure for contact management
! IO  ds_measure       : datastructure for measure and statistics management
! IO  ds_algorom       : datastructure for ROM parameters
! IO  ds_print         : datastructure for printing parameters
! IO  ds_algorom       : datastructure for ROM parameters
! I/O ETA    : PARAMETRE DE PILOTAGE
! OUT NBITER : NOMBRE D'ITERATIONS DE NEWTON
! OUT ETATIN : ETAT DE LA CONVERGENCE DU PAS DE TEMPS
!     0 - CVG  -> LE PAS DE TEMPS A CONVERGE
!     1 - NOOK -> UN EVENEMENT DURANT LE PAS DE TEMPS
!     2 - NCVG -> LE PAS DE TEMPS N'A PAS CONVERGE
!     3 - STOP -> ERREUR FATALE - ARRET DU CALCUL
!
! --------------------------------------------------------------------------------------------------
!
    integer :: niveau, iterat
    aster_logical :: lerrit
    aster_logical :: l_loop_cont, l_cont_disc
    character(len=4) :: etnewt, etfixe
    real(kind=8) :: time
!
! --------------------------------------------------------------------------------------------------
!
    iterat = 0
    niveau = 0
    nbiter = 0
    lerrit = .false.
!
! - Active functionnalities
!
    l_loop_cont = isfonc(fonact,'BOUCLE_EXTERNE')
    l_cont_disc = isfonc(fonact,'CONT_DISCRET')
!
! - Reset events
!
    call nmeraz(sderro, 'TOUS')
    call nmevr0(sddisc)
!
! - Reset values in convergence table for Newton loop
!
    call nmimr0(ds_print, 'NEWT')
!
! - Loops for contact
!
    if (l_loop_cont) then
        niveau = 3
    endif
!
! --- INITIALISATIONS POUR LE NOUVEAU PAS DE TEMPS
!
    call nmnpas(model     , mesh  , mate  , cara_elem , fonact    ,&
                ds_print  , sddisc, sdsuiv, sddyna    , sdnume    ,&
                ds_measure, numedd, numins, ds_contact, ds_algorom,&
                valinc    , solalg, solveu, ds_conv   , list_load )
!
! --- CALCUL DES CHARGEMENTS CONSTANTS AU COURS DU PAS DE TEMPS
!
    call nmchar('FIXE'   , ' '            , model    , numedd  , mate      ,&
                cara_elem, ds_constitutive, list_load, numins  , ds_measure,&
                sddisc   , fonact         , comref   , ds_inout, valinc    ,&
                solalg   , veelem         , measse   , veasse  , sddyna)
!
! ======================================================================
!     BOUCLE POINTS FIXES
! ======================================================================
!
100 continue
!
    iterat = 0
    nbiter = nbiter + 1
!
! --- GESTION DEBUT DE BOUCLE POINTS FIXES
!
    call nmible(niveau, model     , mesh    , ds_contact,&
                fonact, ds_measure, ds_print)
!
! --- CREATION OBJETS POUR CONTACT CONTINU
!
    call nmnble(numins, model , mesh  , numedd    , ds_measure,&
                sddyna, sddisc, fonact, ds_contact, valinc    ,&
                solalg)
!
! ======================================================================
!     PREDICTION
! ======================================================================
!
!
! - Launch timer
!
    call nmtime(ds_measure, 'Launch', 'Newt_Iter')
!
! --- PREDICTION D'UNE DIRECTION DE DESCENTE
!
    call nmpred(model , numedd         , numfix    , mate       , cara_elem,&
                comref, ds_constitutive, list_load , ds_algopara, solveu   ,&
                fonact, ds_print       , ds_measure, ds_algorom , sddisc   ,&
                sdnume, sderro         , numins    , valinc     , solalg   ,&
                matass, maprec         , ds_contact, sddyna     , ds_inout ,&
                meelem, measse         , veelem    , veasse     , lerrit)
!
    if (lerrit) goto 315
!
! ======================================================================
!     BOUCLE SUR LES ITERATIONS DE NEWTON
! ======================================================================
!
300 continue
!
! - Launch timer
!
    if (iterat .ne. 0) then
        call nmtime(ds_measure, 'Launch', 'Newt_Iter')
    endif
!
! --- CALCUL PROPREMENT DIT DE L'INCREMENT DE DEPLACEMENT
! --- EN CORRIGEANT LA (LES) DIRECTIONS DE DESCENTE
! --- SI CONTACT OU PILOTAGE OU RECHERCHE LINEAIRE
!
    call nmdepl(model          , numedd   , mate  , cara_elem , comref     ,&
                ds_constitutive, list_load, fonact, ds_measure, ds_algopara,&
                mesh           , numins   , iterat, solveu    , matass     ,&
                sddisc         , sddyna   , sdnume, sdpilo    , sderro     ,&
                ds_contact     , valinc   , solalg, veelem    , veasse     ,&
                eta            , ds_conv  , lerrit)
!
    if (lerrit) goto 315
!
! --- CALCUL DES FORCES APRES CORRECTION
!
    call nmfcor(model          , numedd    , mate    , cara_elem  , comref,&
                ds_constitutive, list_load , fonact  , ds_algopara, numins,&
                iterat         , ds_measure, sddisc  , sddyna     , sdnume,&
                sderro         , ds_contact, ds_inout, valinc     , solalg,&
                veelem         , veasse    , meelem  , measse     , matass,&
                lerrit)
!
    if (lerrit) goto 315
!
! - DOF monitoring
!
    call nmsuiv(mesh  , sdsuiv         , ds_print, cara_elem, model ,&
                mate  , ds_constitutive, valinc  , comref   , sddisc,&
                numins)
!
! --- ESTIMATION DE LA CONVERGENCE
!
315 continue
    call nmconv(mesh    , model, mate   , numedd  , sdnume     ,&
                fonact  , sddyna, ds_conv, ds_print, ds_measure,&
                sddisc  , sdcrit , sderro  , ds_algopara, ds_algorom,&
                ds_inout, comref, matass , solveu  , numins     ,&
                iterat  , eta   , ds_contact, valinc     ,&
                solalg  , measse, veasse )
!
! --- MISE A JOUR DES EFFORTS DE CONTACT
!
    call nmfcon(model, numedd, mate  , fonact, ds_contact,&
                ds_measure, valinc, solalg,&
                veelem, veasse)
!
! - Evaluate events at current Newton iteration
!
    call nmcvgn(sddisc, sderro, valinc, ds_contact)   
!
! - Print during Newton loop
!
    call nmaffi(fonact, ds_conv, ds_print, sderro, sddisc,&
                'NEWT')
!
! - Stop Newton iterations
!
    call nmleeb(sderro, 'NEWT', etnewt)
    if (etnewt .ne. 'CONT') then
        goto 330
    endif
!
! --- ON CONTINUE LES ITERATIONS DE NEWTON : CALCUL DE LA DESCENTE
!
320 continue
!
    call nmdesc(model   , numedd         , numfix    , mate      , cara_elem  ,&
                comref  , ds_constitutive, list_load , ds_contact, ds_algopara,&
                solveu  , fonact         , numins    , iterat    , sddisc     ,&
                ds_print, ds_measure     , ds_algorom, sddyna    , sdnume     ,&
                sderro  , matass         , maprec    , valinc    , solalg     ,&
                meelem  , measse         , veasse    , veelem    , lerrit)
!
    if (lerrit) goto 315
!
! --- ON CONTINUE NEWTON
!
    iterat = iterat + 1
    nbiter = nbiter + 1
!
! --- CAS DU CONTACT DISCRET
!
    call nmleeb(sderro, 'NEWT', etnewt)
    if (etnewt .eq. 'CTCD') then
        call nmeceb(sderro, 'NEWT', 'CONT')
        call nmtime(ds_measure, 'Stop', 'Newt_Iter')
        goto 300
    endif
!
330 continue
!
! - Timer for current Newton iteration (not for prediction)
!
    call nmtime(ds_measure, 'Stop', 'Newt_Iter')
    call nmrinc(ds_measure, 'Newt_Iter')
    call nmtimr(ds_measure, 'Newt_Iter', 'N', time)
    call nmimcr(ds_print, 'ITER_TIME', time, .true._1)
!
! --- VERIFICATION DU DECLENCHEMENT DES ERREURS FATALES
!
    call nmevdt(ds_measure, sderro, 'ITE')
!
! - Reset times and counters
!
    call nmrini(ds_measure, 'N')
!
! --- ON CONTINUE NEWTON ?
!
    call nmleeb(sderro, 'NEWT', etnewt)
    if (etnewt .eq. 'CONT') goto 300
!
! ======================================================================
!     FIN BOUCLE SUR LES ITERATIONS DE NEWTON
! ======================================================================
!
!
!
! --- GESTION DES ACTIONS A LA FIN DE LA BOUCLE DE NEWTON
!
    call nmactn(ds_print, sddisc, sderro, ds_contact,&
                ds_conv , iterat, numins)
!
! --- ON FAIT DES ITERATIONS SUPPLEMENTAIRES ?
!
    call nmleeb(sderro, 'NEWT', etnewt)
    if (etnewt .eq. 'CONT') then
        call nmtime(ds_measure, 'Launch', 'Newt_Iter')
        call nmcrel(sderro, 'ITER_MAXI', .false._1)
        goto 320
    endif
!
! --- GESTION FIN DE BOUCLE POINTS FIXES
!
    call nmtble(niveau, model, mesh    , mate  , ds_contact, &
                fonact, ds_print, ds_measure, sddyna,&
                sderro, ds_conv , sddisc, numins, valinc,&
                solalg)
!
! --- ETAT DE LA CONVERGENCE POINT FIXE
!
    call nmcvgf(sddisc, sderro, valinc, ds_contact)
!
! --- GESTION DES ACTIONS A LA FIN D'UNE BOUCLE DE POINT FIXE
!
    call nmactf(ds_print, sddisc, sderro, ds_contact,&
                ds_conv , iterat, numins)
!
! --- POUR LA CONTINUATION DU POINT FIXE: GLUTE DUE AU CONTACT DISCRET
!
    call nmleeb(sderro, 'FIXE', etfixe)
    if (etfixe .eq. 'CONT') then
        if (l_cont_disc) then
            call nmeceb(sderro, 'NEWT', 'CTCD')
            call nmtime(ds_measure, 'Launch', 'Newt_Iter')
            goto 320
        else if (l_loop_cont) then
            goto 100
        else
            call nmeceb(sderro, 'FIXE', 'CONV')
        endif
    endif
!
! ======================================================================
!     FIN BOUCLE POINTS FIXES
! ======================================================================
!
end subroutine
