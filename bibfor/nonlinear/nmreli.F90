subroutine nmreli(modele, numedd, mate, carele, comref,&
                  compor, lischa, carcri, fonact, iterat,&
                  sdstat, sdnume, sddyna, parmet, method,&
                  defico, valinc, solalg, veelem, veasse,&
                  sdtime, conv, ldccvg)
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
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmrelp.h"
    integer :: fonact(*)
    integer :: iterat, ldccvg
    real(kind=8) :: parmet(*), conv(*)
    character(len=16) :: method(*)
    character(len=24) :: carcri, sdtime, defico, sdstat
    character(len=19) :: lischa, sddyna, sdnume
    character(len=24) :: modele, numedd, mate, carele, comref, compor
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! RECHERCHE LINEAIRE DANS LA DIRECTION DE DESCENTE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  DEFICO : SD DEFINITION CONTACT
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  SDNUME : SD NUMEROTATION
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  SDDYNA : SD DYNAMIQUE
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! OUT CONV   : INFORMATIONS SUR LA CONVERGENCE DU CALCUL
!                10 : ITERATIONS RECHERCHE LINEAIRE
!                11 : VALEUR DE RHO
!
!
!
!
    character(len=19) :: ddepla, deppr1
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(solalg, 'SOLALG', 'DEPPR1', deppr1)
!
! --- RECOPIE DE L'INC. PREDICTION EN INC. SOLUTION
!
    call copisd('CHAMP_GD', 'V', deppr1, ddepla)
!
! --- RECHERCHE LINEAIRE DANS LA DIRECTION DE DESCENTE
!
    call nmrelp(modele, numedd, mate, carele, comref,&
                compor, lischa, carcri, fonact, iterat,&
                sdstat, sdnume, sddyna, parmet, method,&
                defico, valinc, solalg, veelem, veasse,&
                sdtime, conv, ldccvg)
!
    call jedema()
!
end subroutine
