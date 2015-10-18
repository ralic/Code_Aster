subroutine nmcoun(noma      , fonact, solveu, numedz, matass,&
                  ds_contact, iterat, valinc, solalg, veasse,&
                  instan    , resigr, sdtime, sdstat, ctccvg)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcofr.h"
#include "asterfort/nmunil.h"
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
    character(len=8) :: noma
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24) :: sdtime, sdstat
    character(len=19) :: valinc(*)
    character(len=19) :: solalg(*), veasse(*)
    character(len=19) :: solveu, matass
    character(len=*) :: numedz
    integer :: iterat
    real(kind=8) :: instan
    real(kind=8) :: resigr
    integer :: ctccvg
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MISE A JOUR DE L'INCREMENT DE DEPLACEMENT SI CONTACT OU
! LIAISON_UNILATER
!
!
! ----------------------------------------------------------------------
!
! IN  MAILLA : NOM DU MAILLAGE
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SOLVEU : SD SOLVEUR
! IN  NUMEDD : NUME_DDL
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! In  ds_contact       : datastructure for contact management
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!               OUT: DDEPLA
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  RESIGR : RESI_GLOB_RELA
! IN  ITERAT : ITERATION DE NEWTON
! IN  INSTAN : VALEUR DE L'INSTANT DE CALCUL
! OUT CTCCVG : CODE RETOUR CONTACT DISCRET
!                -1 : PAS DE CALCUL DU CONTACT DISCRET
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXI D'ITERATIONS
!                 2 : MATRICE SINGULIERE
!
!
!
!
    aster_logical :: lunil, lctcd
    aster_logical :: lallv
    character(len=19) :: depdel, ddepla, cncine
    character(len=19) :: depplu
    character(len=14) :: numedd
!
! ----------------------------------------------------------------------
!
!
! --- FONCTIONNALITES ACTIVEES
!
    lunil = isfonc(fonact,'LIAISON_UNILATER')
    lctcd = isfonc(fonact,'CONT_DISCRET')
    ctccvg = -1
    numedd = numedz(1:14)
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(veasse, 'VEASSE', 'CNCINE', cncine)
!
! --- TRAITEMENT DU CONTACT ET/OU DU FROTTEMENT DISCRET
!
    if (lctcd) then
        lallv = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
        if (.not.lallv) then
            call nmcofr(noma  , depplu, depdel    , ddepla, solveu,&
                        numedd, matass, ds_contact, iterat, resigr,&
                        sdstat, sdtime, ctccvg    , instan)
        endif
        ctccvg = 0
    endif
!
! --- TRAITEMENT DE LIAISON_UNILATER
!
    if (lunil) then
        call nmunil(noma      , depplu, ddepla, solveu, matass,&
                    ds_contact, cncine, iterat, instan, ctccvg)
    endif
!
! --- LE CALCUL DE CONTACT A FORCEMENT ETE REALISE
!
    ASSERT(ctccvg.ge.0)
!
end subroutine
