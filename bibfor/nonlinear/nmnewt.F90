subroutine nmnewt(noma       , modele  , numins  , numedd , numfix    ,&
                  mate       , carele  , comref  , compor , lischa    ,&
                  ds_algopara, fonact  , carcri  , ds_measure,&
                  sderro     , ds_print, sdnume  , sddyna , sddisc    ,&
                  sdcrit     , sdsuiv  , sdpilo  , ds_conv, solveu    ,&
                  maprec     , matass  , ds_inout, valinc , solalg    ,&
                  meelem     , measse  , veelem  , veasse , ds_contact,&
                  eta        , nbiter)
!
use NonLin_Datastructure_type
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
    integer :: numins
    integer :: fonact(*)
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=24) :: carcri
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: sderro, sdsuiv
    character(len=19) :: sdnume, sddyna, sddisc, sdcrit
    character(len=19) :: sdpilo
    character(len=19) :: valinc(*), solalg(*)
    character(len=19) :: meelem(*), veelem(*)
    character(len=19) :: measse(*), veasse(*)
    type(NL_DS_Print), intent(inout) :: ds_print
    type(NL_DS_Conv), intent(inout) :: ds_conv
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=19) :: lischa
    character(len=19) :: solveu, maprec, matass
    character(len=24) :: modele, numedd, numfix
    character(len=24) :: comref, compor
    character(len=24) :: mate, carele
    type(NL_DS_Contact), intent(inout) :: ds_contact
    real(kind=8) :: eta
    integer :: nbiter
    character(len=8) :: noma
!
! ----------------------------------------------------------------------
!
! OPERATEUR NON-LINEAIRE MECANIQUE
!
! ALGORITHME DE NEWTON (STATIQUE ET DYNAMIQUE)
!
! ----------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARIABLES DE COMMANDE DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  SOLVEU : SOLVEUR
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! In  ds_algopara      : datastructure for algorithm parameters
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  SDDISC : SD DISC_INST
! IO  ds_measure       : datastructure for measure and statistics management
! IN  SDERRO : GESTION DES ERREURS
! IO  ds_print         : datastructure for printing parameters
! IO  ds_conv          : datastructure for convergence management
! In  ds_inout         : datastructure for input/output management
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IO  ds_contact       : datastructure for contact management
! IN  SDDYNA : SD DYNAMIQUE
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  MAPREC : NOM DE LA MATRICE DE PRECONDITIONNEMENT (GCPC)
! IN  SDNUME : SD NUMEROTATION
! I/O ETA    : PARAMETRE DE PILOTAGE
! OUT NBITER : NOMBRE D'ITERATIONS DE NEWTON
! OUT ETATIN : ETAT DE LA CONVERGENCE DU PAS DE TEMPS
!     0 - CVG  -> LE PAS DE TEMPS A CONVERGE
!     1 - NOOK -> UN EVENEMENT DURANT LE PAS DE TEMPS
!     2 - NCVG -> LE PAS DE TEMPS N'A PAS CONVERGE
!     3 - STOP -> ERREUR FATALE - ARRET DU CALCUL
!
! ----------------------------------------------------------------------
!
    integer :: niveau, iterat
    aster_logical :: lerrit
    aster_logical :: l_loop_cont, l_cont_disc
    character(len=4) :: etnewt, etfixe
    real(kind=8) :: time
!
! ----------------------------------------------------------------------
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
    call nmnpas(modele  , noma  , mate  , carele , fonact    ,&
                ds_print, sddisc, sdsuiv, sddyna , sdnume    ,&
                ds_measure, numedd, numins , ds_contact,&
                valinc  , solalg, solveu, ds_conv, lischa  )
!
! --- CALCUL DES CHARGEMENTS CONSTANTS AU COURS DU PAS DE TEMPS
!
    call nmchar('FIXE'  , ' '   , modele, numedd, mate  ,&
                carele  , compor, lischa, numins, ds_measure,&
                sddisc  , fonact, comref,&
                ds_inout, valinc, solalg, veelem, measse,&
                veasse  , sddyna)
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
    call nmible(niveau, modele, noma      , ds_contact,&
                fonact, numedd, ds_measure, ds_print)
!
! --- CREATION OBJETS POUR CONTACT CONTINU
!
    call nmnble(numins, modele, noma  , numedd, ds_measure,&
                sddyna, sddisc, fonact, ds_contact,&
                valinc, solalg)
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
    call nmpred(modele  , numedd, numfix  , mate       , carele,&
                comref  , compor, lischa  , ds_algopara, solveu,&
                fonact  , carcri, ds_print, ds_measure ,&
                sddisc  , sdnume, sderro  , numins     , valinc,&
                solalg  , matass, maprec  , ds_contact , sddyna,&
                ds_inout, meelem, measse  , veelem     , veasse,&
                lerrit)
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
    call nmdepl(modele, numedd, mate      , carele , comref     ,&
                compor, lischa, fonact    , ds_measure , ds_algopara,&
                carcri, noma  , numins    , iterat , solveu     ,&
                matass, sddisc, sddyna    , sdnume , sdpilo     ,&
                sderro, ds_contact, valinc , solalg     ,&
                veelem, veasse, eta       , ds_conv, lerrit)
!
    if (lerrit) goto 315
!
! --- CALCUL DES FORCES APRES CORRECTION
!
    call nmfcor(modele, numedd, mate  , carele     , comref  ,&
                compor, lischa, fonact, ds_algopara, carcri  ,&
                numins, iterat, ds_measure, sddisc  ,&
                sddyna, sdnume, sderro, ds_contact , ds_inout,&
                valinc, solalg, veelem, veasse     , meelem  ,&
                measse, matass, lerrit)
!
    if (lerrit) goto 315
!
! - DOF monitoring
!
    call nmsuiv(noma  , sdsuiv, ds_print, carele, modele,&
                mate  , compor, valinc  , comref, sddisc,&
                numins)
!
! --- ESTIMATION DE LA CONVERGENCE
!
315 continue
    call nmconv(noma    , modele, mate   , numedd  , sdnume     ,&
                fonact  , sddyna, ds_conv, ds_print, ds_measure,&
                sddisc  , sdcrit , sderro  , ds_algopara,&
                ds_inout, comref, matass , solveu  , numins     ,&
                iterat  , eta   , ds_contact, valinc     ,&
                solalg  , measse, veasse )
!
! --- MISE A JOUR DES EFFORTS DE CONTACT
!
    call nmfcon(modele, numedd, mate  , fonact, ds_contact,&
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
    call nmdesc(modele, numedd  , numfix, mate      , carele     ,&
                comref, compor  , lischa, ds_contact, ds_algopara,&
                solveu, carcri  , fonact, numins    , iterat     ,&
                sddisc, ds_print, ds_measure, sddyna     ,&
                sdnume, sderro  , matass, maprec    , valinc     ,&
                solalg, meelem  , measse, veasse    , veelem     ,&
                lerrit)
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
    call nmtble(niveau, modele, noma    , mate  , ds_contact, &
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
