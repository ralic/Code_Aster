subroutine nmassp(modele         , numedd, mate  , carele    , comref    ,&
                  ds_constitutive, lischa, fonact, ds_measure, ds_contact,&
                  sddyna         , valinc, solalg, veelem    , veasse    ,&
                  ldccvg         , cnpilo, cndonn, sdnume    , matass)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/ndassp.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nsassp.h"
#include "asterfort/vtzero.h"
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
! aslint: disable=W1504
!
    integer :: ldccvg
    integer :: fonact(*)
    character(len=19) :: lischa, sddyna, sdnume, matass
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: modele, numedd, mate
    character(len=24) :: carele, comref
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: veasse(*), veelem(*)
    character(len=19) :: cnpilo, cndonn
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION)
!
! CALCUL DU SECOND MEMBRE POUR LA PREDICTION
!
! --------------------------------------------------------------------------------------------------
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VALEURS DE REF DES VARIABLES DE COMMANDE
! IN  LISCHA : SD L_CHARGES
! In  ds_constitutive  : datastructure for constitutive laws management
! In  ds_contact       : datastructure for contact management
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IO  ds_measure       : datastructure for measure and statistics management
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  SDNUME : SD NUMEROTATION
! OUT CNPILO : VECTEUR ASSEMBLE DES FORCES PILOTEES
! OUT CNDONN : VECTEUR ASSEMBLE DES FORCES DONNEES
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: lstat, ldyna
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CALCUL SECOND MEMBRE'
    endif
!
! --- INITIALISATIONS
!
    ldccvg = -1
    call vtzero(cnpilo)
    call vtzero(cndonn)
!
! --- FONCTIONNALITES ACTIVEES
!
    lstat = ndynlo(sddyna,'STATIQUE')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
!
! --- EVALUATION DU SECOND MEMBRE
!
    if (ldyna) then
        call ndassp(modele         , numedd, mate      , carele, comref    ,&
                    ds_constitutive, lischa, ds_measure, fonact, ds_contact,&
                    sddyna         , valinc, solalg    , veelem, veasse    ,&
                    ldccvg         , cndonn, sdnume    , matass)
    else if (lstat) then
        call nsassp(modele, numedd, lischa, fonact, sddyna,&
                    ds_measure, valinc, veelem, veasse, cnpilo,&
                    cndonn, mate, carele, ds_contact, matass)
    else
        ASSERT(.false.)
    endif
!
end subroutine
