subroutine nmchar(mode    , phasez, modele, numedd, mate  ,&
                  carele  , compor, lischa, numins, sdtime,&
                  sddisc  , fonact, resoco, resocu, comref,&
                  ds_inout, valinc, solalg, veelem, measse,&
                  veasse  , sddyna)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmcvec.h"
#include "asterfort/nmxvec.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
! aslint: disable=W1504
!
    character(len=4) :: mode
    character(len=*) :: phasez
    character(len=19) :: lischa
    character(len=24) :: modele, mate, carele, numedd, sdtime
    character(len=24) :: compor, comref
    character(len=19) :: sddyna, sddisc
    integer :: fonact(*)
    integer :: numins
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=24) :: resoco, resocu
    character(len=19) :: veelem(*), measse(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
!
! CALCUL ET ASSEMBLAGE DES FORCES EXTERNES
!
! ----------------------------------------------------------------------
!
!
! IN  MODE   : 'FIXE' -> CALCUL CHARGES FIXES AU COURS DU TEMPS
!              'VARI' -> CALCUL CHARGES VARIABLES AU COURS DU TEMPS
!              'ACCI' -> CALCUL CHARGES POUR ACCELERATION INITIALE
! IN  PHASE  : PHASE DE CALCUL
!               'CORRECTION' OU 'PREDICTION'
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  LISCHA : LISTE DES CHARGES
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMPOR : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
! IN  NUMEDD : NUME_DDL
! IN  NUMINS : NUMERO INSTANT
! IN  SDTIME : SD TIMER
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  COMREF : VARI_COM DE REFERENCE
! In  ds_inout         : datastructure for input/output management
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  RESOCU : SD POUR LA RESOLUTION DE LIAISON_UNILATER
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  SDDYNA : SD DYNAMIQUE
!
!
!
!
    aster_logical :: ldyna, lexpl
    aster_logical :: londe, llapl, lammo, lsstf, lviss
    aster_logical :: limpe, lpilo, lmacr, limpex, l_diri_undead
    character(len=10) :: phase
    integer :: nbvect
    character(len=16) :: loptve(20)
    character(len=6) :: ltypve(20)
    aster_logical :: lassve(20), lcalve(20)
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><CHAR> CALCUL DU CHARGEMENT: ',mode
    endif
!
! --- INITIALISATIONS
!
    phase = phasez
    call nmcvec('INIT', ' ', ' ', .false._1, .false._1,&
                nbvect, ltypve, loptve, lcalve, lassve)
!
! - Active functionnalities
!
    londe         = ndynlo(sddyna,'ONDE_PLANE')
    ldyna         = ndynlo(sddyna,'DYNAMIQUE')
    lexpl         = ndynlo(sddyna,'EXPLICITE')
    llapl         = isfonc(fonact,'LAPLACE')
    limpe         = ndynlo(sddyna,'IMPE_ABSO')
    lammo         = ndynlo(sddyna,'AMOR_MODAL')
    lpilo         = isfonc(fonact,'PILOTAGE')
    limpex        = isfonc(fonact,'IMPLEX')
    lmacr         = isfonc(fonact,'MACR_ELEM_STAT')
    lsstf         = isfonc(fonact,'SOUS_STRUC')
    lviss         = ndynlo(sddyna,'VECT_ISS')
    l_diri_undead = isfonc(fonact,'DIRI_UNDEAD')
!
! --- CHARGEMENTS FIXES PENDANT LE PAS DE TEMPS (ON EST EN PREDICTION)
!
    if (mode .eq. 'FIXE') then
!
! --- DEPLACEMENTS IMPOSES DONNES
!
        call nmcvec('AJOU', 'CNDIDO', ' ', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
! --- DEPLACEMENTS IMPOSES PILOTES
!
        if (lpilo) then
            call nmcvec('AJOU', 'CNDIPI', ' ', .true._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- CHARGEMENTS FORCES DE LAPLACE
!
        if (llapl) then
            call nmcvec('AJOU', 'CNLAPL', ' ', .true._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- CHARGEMENTS ONDE_PLANE
!
        if (londe) then
            call nmcvec('AJOU', 'CNONDP', ' ', .true._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- CHARGEMENTS MECANIQUES FIXES DONNES
!
        call nmcvec('AJOU', 'CNFEDO', ' ', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
! --- CHARGEMENTS MECANIQUES PILOTES
!
        if (lpilo) then
            call nmcvec('AJOU', 'CNFEPI', ' ', .true._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- CONDITIONS CINEMATIQUES IMPOSEES  (AFFE_CHAR_CINE)
!
        call nmcvec('AJOU', 'CNCINE', ' ', .false._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
! --- FORCE DE REFERENCE LIEE AUX VAR. COMMANDES EN T+
!
        call nmcvec('AJOU', 'CNVCF0', ' ', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
! --- FORCES NODALES POUR PREDICTION (SKIP FOR IMPLEX/EXPLICITE)
!
        if (.not.(lexpl.or.limpex)) then
            call nmcvec('AJOU', 'CNFNOD', 'SIGMOI', .true._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- FORCES POUR VAR. COMM. (POUR PREDICTION)
!
        call nmcvec('AJOU', 'CNVCPR', ' ', .false._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
! --- FORCES ISSUES DU CALCUL PAR SOUS-STRUCTURATION
!
        if (lsstf) then
            call nmcvec('AJOU', 'CNSSTF', ' ', .true._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- CHARGES VEC_ISS
!
        if (lviss) then
            call nmcvec('AJOU', 'CNVISS', ' ', .false._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- CALCUL ET ASSEMBLAGE
!
        call nmxvec(modele  , mate  , carele, compor, sdtime,&
                    sddisc  , sddyna, numins, valinc, solalg,&
                    lischa  , comref, resoco, resocu, numedd,&
                    ds_inout, veelem, veasse, measse, nbvect,&
                    ltypve  , lcalve, loptve, lassve)
!
! --- CHARGEMENTS VARIABLES PENDANT LE PAS DE TEMPS
!
    else if (mode.eq.'VARI') then
!
! --- DEPLACEMENTS IMPOSES DONNES
!
        if (l_diri_undead) then
        call nmcvec('AJOU', 'CNDIDO', ' ', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- FORCES NODALES (POUR METHODE IMPLEX)
!
        if (limpex) then
            if (phase .eq. 'PREDICTION') then
                call nmcvec('AJOU', 'CNFNOD', 'SIGEXT', .true._1, .true._1,&
                            nbvect, ltypve, loptve, lcalve, lassve)
            endif
        endif
!
! --- FORCES SUIVEUSES DONNEES
!
        call nmcvec('AJOU', 'CNFSDO', ' ', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
        if (ldyna) then
!
! --- FORCES D'EQUILIBRE DYNAMIQUE
!
            call nmcvec('AJOU', 'CNDYNA', ' ', .false._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
!
! --- FORCES D'AMORTISSEMENT MODAL
!
            if (lammo) then
                if (phase .eq. 'PREDICTION') then
                    call nmcvec('AJOU', 'CNMODP', ' ', .false._1, .true._1,&
                                nbvect, ltypve, loptve, lcalve, lassve)
                else if (phase.eq.'CORRECTION') then
                    call nmcvec('AJOU', 'CNMODC', ' ', .false._1, .true._1,&
                                nbvect, ltypve, loptve, lcalve, lassve)
                else
                    ASSERT(.false.)
                endif
            endif
!
! --- FORCES IMPEDANCES
!
            if (limpe) then
                if (phase .eq. 'PREDICTION') then
                    call nmcvec('AJOU', 'CNIMPP', ' ', .true._1, .true._1,&
                                nbvect, ltypve, loptve, lcalve, lassve)
                else if (phase.eq.'CORRECTION') then
                    call nmcvec('AJOU', 'CNIMPC', ' ', .true._1, .true._1,&
                                nbvect, ltypve, loptve, lcalve, lassve)
                else
                    ASSERT(.false.)
                endif
            endif
        endif
!
! --- FORCES ISSUES DES MACRO-ELEMENTS
! --- VECT_ASSE(MACR_ELEM) = MATR_ASSE(MACR_ELEM) * VECT_DEPL
!
        if (lmacr) then
            call nmcvec('AJOU', 'CNSSTR', ' ', .false._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- CALCUL EFFECTIF
!
        call nmxvec(modele  , mate  , carele, compor, sdtime,&
                    sddisc  , sddyna, numins, valinc, solalg,&
                    lischa  , comref, resoco, resocu, numedd,&
                    ds_inout, veelem, veasse, measse, nbvect,&
                    ltypve  , lcalve, loptve, lassve)
!
! --- CHARGEMENTS POUR ACCELERATION INITIALE
!
    else if (mode.eq.'ACCI') then
        if (numins .ne. 0) then
            ASSERT(.false.)
        endif
!
! --- CHARGEMENTS MECANIQUES FIXES DONNES
!
        call nmcvec('AJOU', 'CNFEDO', ' ', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
! --- FORCES SUIVEUSES DONNEES
!
        call nmcvec('AJOU', 'CNFSDO', ' ', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
! --- DEPLACEMENTS IMPOSES DONNES
!
        call nmcvec('AJOU', 'CNDIDO', ' ', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
! --- FORCES NODALES
!
        call nmcvec('AJOU', 'CNFNOD', 'SIGMOI', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
! --- CONDITIONS CINEMATIQUES IMPOSEES  (AFFE_CHAR_CINE)
!
        call nmcvec('AJOU', 'CNCINE', ' ', .false._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
!
! --- FORCES ISSUES DES MACRO-ELEMENTS
!
        if (lmacr) then
            call nmcvec('AJOU', 'CNSSTR', ' ', .false._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- FORCES ISSUES DU CALCUL PAR SOUS-STRUCTURATION
!
        if (lsstf) then
            call nmcvec('AJOU', 'CNSSTF', ' ', .true._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- FORCES IMPEDANCES
!
        if (limpe) then
            call nmcvec('AJOU', 'CNIMPP', ' ', .true._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- CHARGES VEC_ISS
!
        if (lviss) then
            call nmcvec('AJOU', 'CNVISS', ' ', .false._1, .true._1,&
                        nbvect, ltypve, loptve, lcalve, lassve)
        endif
!
! --- CALCUL ET ASSEMBLAGE
!
        call nmxvec(modele  , mate  , carele, compor, sdtime,&
                    sddisc  , sddyna, numins, valinc, solalg,&
                    lischa  , comref, resoco, resocu, numedd,&
                    ds_inout, veelem, veasse, measse, nbvect,&
                    ltypve  , lcalve, loptve, lassve)
    else
        ASSERT(.false.)
    endif
!
end subroutine
