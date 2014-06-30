subroutine nminvc(modelz, mate, carele, compor, carcri,&
                  sdtime, sddisc, sddyna, valinc, solalg,&
                  lischa, comref, resoco, resocu, numedd,&
                  fonact, parcon, veelem, veasse, measse)
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
    implicit      none
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmcvec.h"
#include "asterfort/nmxvec.h"
    integer :: fonact(*)
    character(len=*) :: modelz
    character(len=24) :: mate, carele
    character(len=24) :: compor, carcri
    real(kind=8) :: parcon(8)
    character(len=19) :: sddisc, sddyna, lischa
    character(len=24) :: resoco, resocu
    character(len=24) :: comref, numedd, sdtime
    character(len=19) :: veelem(*), veasse(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL ET ASSEMBLAGE DES VECT_ELEM CONSTANTS AU COURS DU CALCUL
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDDYNA : SD DYNAMIQUE
! IN  COMPOR : CARTE COMPORTEMENT
! IN  MODELE : NOM DU MODELE
! IN  SOLVEU : SOLVEUR
! IN  NUMEDD : NUME_DDL
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  RESOCU : SD RESOLUTION LIAISON_UNILATER
! IN  LISCHA : LISTE DES CHARGEMENTS
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  SDDISC : SD DISCRETISATION
! IN  SDTIME : SD TIMER
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT MEELEM : MATRICES ELEMENTAIRES
! OUT MEASSE : MATRICES ASSEMBLEES
!
! ----------------------------------------------------------------------
!
    logical(kind=1) :: lrefe, ldidi
    integer :: ifm, niv
    integer :: numins
    integer :: nbvect
    character(len=6) :: ltypve(20)
    character(len=16) :: loptve(20)
    logical(kind=1) :: lcalve(20), lassve(20)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> PRECALCUL DES VECT_ELEM CONSTANTES'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    ldidi = isfonc(fonact,'DIDI')
    lrefe = isfonc(fonact,'RESI_REFE')
!
! --- INITIALISATIONS
!
    numins = 1
!
    call nmcvec('INIT', ' ', ' ', .false._1, .false._1,&
                nbvect, ltypve, loptve, lcalve, lassve)
!
! --- CREATION DU VECT_ELEM POUR DIRICHLET DIFFERENTIEL
!
    if (ldidi) then
        call nmcvec('AJOU', 'CNDIDI', ' ', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
    endif
!
! --- CREATION DU VECT_ELEM POUR CRITERE EN CONTRAINTE GENERALISEE
!
    if (lrefe) then
        call nmcvec('AJOU', 'CNREFE', ' ', .true._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
    endif
!
! --- CREATION DU VECT_ELEM POUR FORCE DE REFERENCE LIEE
! --- AUX VAR. COMMANDES EN T-
!
    call nmcvec('AJOU', 'CNVCF1', ' ', .true._1, .true._1,&
                nbvect, ltypve, loptve, lcalve, lassve)
!
! --- CALCUL DES VECT_ELEM DE LA LISTE
!
    if (nbvect .gt. 0) then
        call nmxvec(modelz, mate, carele, compor, carcri,&
                    sdtime, sddisc, sddyna, numins, valinc,&
                    solalg, lischa, comref, resoco, resocu,&
                    numedd, parcon, veelem, veasse, measse,&
                    nbvect, ltypve, lcalve, loptve, lassve)
    endif
!
    call jedema()
end subroutine
