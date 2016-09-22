subroutine nmfint(modele, mate  , carele, comref    , ds_constitutive,&
                  fonact, iterat, sddyna, ds_measure, valinc         ,&
                  solalg, ldccvg, vefint)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/merimo.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
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
!
    integer :: ldccvg
    integer :: iterat
    integer :: fonact(*)
    character(len=19) :: sddyna
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: modele, mate
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    character(len=24) :: carele, comref
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: vefint
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! INTEGRATION DE LA LOI DE COMPORTEMENT
! CALCUL DES FORCES INTERIEURES
!
! --------------------------------------------------------------------------------------------------
!
! IN  MODELE : NOM DU MODELE
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IO  ds_measure       : datastructure for measure and statistics management
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VALEURS DE REF DES VARIABLES DE COMMANDE
! In  ds_constitutive  : datastructure for constitutive laws management
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  ITERAT : NUMERO DE L'ITERATION DE NEWTON
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT VEFINT : VECT_ELEM DES FORCES INTERNES
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DE FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: tabret(0:10)
    integer :: iter
    integer :: ifm, niv
    character(len=1) :: base
    character(len=16) :: option
    character(len=19) :: k19bla
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CALCUL DES FORCES INTERNES'
    endif
!
! --- INITIALISATIONS
!
    iter = iterat+1
    base = 'V'
    k19bla = ' '
    option = 'RAPH_MECA'
    ldccvg = 0
!
! - Launch timer
!
    call nmtime(ds_measure, 'Init', 'Integrate')
    call nmtime(ds_measure, 'Launch', 'Integrate')
!
! --- CALCUL DES FORCES INTERIEURES
!
    call merimo(base, modele, carele, mate, comref,&
                ds_constitutive, iter, fonact, sddyna,&
                valinc, solalg, k19bla, vefint, option,&
                tabret)
!
! - End timer
!
    call nmtime(ds_measure, 'Stop', 'Integrate')
    call nmrinc(ds_measure, 'Integrate')
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
    call jedema()
end subroutine
