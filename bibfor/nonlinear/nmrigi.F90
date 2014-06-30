subroutine nmrigi(modelz, mate, carele, compor, carcri,&
                  sddyna, sdstat, sdtime, fonact, iterat,&
                  valinc, solalg, comref, meelem, veelem,&
                  optioz, ldccvg, codere)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/isfonc.h"
#include "asterfort/merimo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdep0.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
    character(len=*) :: optioz
    character(len=*) :: modelz
    character(len=*) :: mate
    character(len=24) :: sdstat, sdtime
    character(len=24) :: compor, carcri, carele
    integer :: iterat, ldccvg
    character(len=19) :: sddyna
    character(len=24) :: comref, codere
    character(len=19) :: meelem(*), veelem(*)
    character(len=19) :: solalg(*), valinc(*)
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL DES MATR_ELEM DE RIGIDITE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  OPTRIG : OPTION DE CALCUL POUR MERIMO
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  ITERAT : NUMERO D'ITERATION
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORS
! OUT CODERE : CHAM_ELEM CODE RETOUR ERREUR INTEGRATION LDC
!
! ----------------------------------------------------------------------
!
    character(len=19) :: vefint, merigi
    character(len=1) :: base
    character(len=24) :: modele
    character(len=16) :: optrig
    logical(kind=1) :: tabret(0:10), lendo
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    base = 'V'
    modele = modelz
    ldccvg = 0
    optrig = optioz
    codere = '&&OP0070.CODERE'
!
! --- VECT_ELEM ET MATR_ELEM
!
    call nmchex(veelem, 'VEELEM', 'CNFINT', vefint)
    call nmchex(meelem, 'MEELEM', 'MERIGI', merigi)
!
! --- INCREMENT DE DEPLACEMENT NUL EN PREDICTION
!
    lendo = isfonc(fonact,'ENDO_NO')
!
    if (.not.lendo) then
        if (optrig(1:9) .eq. 'RIGI_MECA') then
            call nmdep0('ON ', solalg)
        endif
    endif
!
! --- INIT TIMER
!
    call nmtime(sdtime, 'INI', 'INTEGRATION')
    call nmtime(sdtime, 'RUN', 'INTEGRATION')
!
! --- CALCUL DES MATR_ELEM DE RIGIDITE
!
    call merimo(base, modele, carele, mate, comref,&
                compor, carcri, iterat+1, fonact, sddyna,&
                valinc, solalg, merigi, vefint, optrig,&
                tabret, codere)
!
! --- FIN TIMER
!
    call nmtime(sdtime, 'END', 'INTEGRATION')
    call nmrinc(sdstat, 'INTEGRATION')
!
! --- CODE RETOUR ERREUR INTEGRATION LDC
!
    if (tabret(0)) then
        if (tabret(4)) then
            ldccvg = 4
        else if (tabret(3)) then
            ldccvg = 3
        else if (tabret(2)) then
            ldccvg = 2
        else
            ldccvg = 1
        endif
        if (tabret(1)) then
            ldccvg = 1
        endif
    endif
!
! --- REMISE INCREMENT DE DEPLACEMENT
!
    if (optrig(1:9) .eq. 'RIGI_MECA') then
        call nmdep0('OFF', solalg)
    endif
!
end subroutine
