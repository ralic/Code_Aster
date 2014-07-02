subroutine nmpich(modele, numedd, mate, carele, comref,&
                  compor, lischa, carcri, fonact, sdstat,&
                  defico, resoco, sdpilo, iterat, sdnume,&
                  deltat, valinc, solalg, veelem, veasse,&
                  sdtime, sddisc, eta, rho, offset,&
                  ldccvg, pilcvg, matass)
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
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmceta.h"
#include "asterfort/nmpilo.h"
    integer :: fonact(*)
    integer :: iterat, pilcvg, ldccvg
    real(kind=8) :: deltat, eta, rho, offset
    character(len=19) :: lischa, sdnume, sdpilo, sddisc, matass
    character(len=24) :: carcri, modele, numedd, mate, carele, comref, compor
    character(len=24) :: defico, resoco
    character(len=24) :: sdstat, sdtime
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! CALCUL DU ETA DE PILOTAGE ET CALCUL DE LA CORRECTION
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  SDNUME : SD NUMEROTATION
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  SDSTAT : SD STATISTIQUES
! IN  SDPILO : SD PILOTAGE
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  DEFICO : SD DEFINITION CONTACT
! IN  RESOCO : SD RESOLUTION CONTACT VOLATILE OP0070
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  DELTAT : INCREMENT DE TEMPS
! IN  NBEFFE : NOMBRE DE VALEURS DE PILOTAGE ENTRANTES
! IN  SDTIME : SD TIMER
! IN  SDDISC : SD DISCRETISATION
! OUT ETA    : PARAMETRE DE PILOTAGE
! OUT RHO    : PARAMETRE DE RECHERCHE_LINEAIRE
! OUT OFFSET : DECALAGE DE ETA_PILOTAGE EN FONCTION DE RHO
! OUT PILCVG : CODE DE CONVERGENCE POUR LE PILOTAGE
!                -1 : PAS DE CALCUL DU PILOTAGE
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : PAS DE SOLUTION
!                 2 : BORNE ATTEINTE -> FIN DU CALCUL
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! IN  MATASS : SD MATRICE ASSEMBLEE
!
!
!
!
    integer :: nbeffe, nbatte
    real(kind=8) :: proeta(2), residu
    integer :: ifm, niv
    aster_logical :: irecli
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PILOTAGE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<PILOTAGE> ... PILOTAGE SANS RECH_LINE'
    endif
!
! --- INITIALISATIONS
!
    pilcvg = -1
    rho = 1.d0
    offset = 0.d0
    nbatte = 2
    irecli = .false.
!
! --- RESOLUTION DE L'EQUATION DE PILOTAGE
!
    call nmpilo(sdpilo, deltat, rho, solalg, veasse,&
                modele, mate, compor, resoco, valinc,&
                nbatte, numedd, nbeffe, proeta, pilcvg,&
                carele)
!
    if (pilcvg .eq. 1) goto 9999
!
! --- CHOIX DE ETA_PILOTAGE
!
    call nmceta(modele, numedd, mate, carele, comref,&
                compor, lischa, carcri, fonact, sdstat,&
                defico, sdpilo, iterat, sdnume, valinc,&
                solalg, veelem, veasse, sdtime, sddisc,&
                nbeffe, irecli, proeta, offset, rho,&
                eta, ldccvg, pilcvg, residu, matass)
!
9999 continue
!
! --- LE CALCUL DE PILOTAGE A FORCEMENT ETE REALISE
!
    ASSERT(pilcvg.ge.0)
!
    call jedema()
end subroutine
