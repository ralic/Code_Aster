subroutine nmpred(modele, numedd, numfix, mate, carele,&
                  comref, compor, lischa, method, solveu,&
                  fonact, parmet, carcri, sdimpr, sdstat,&
                  sdtime, sddisc, sdnume, sderro, numins,&
                  valinc, solalg, matass, maprec, defico,&
                  resoco, resocu, sddyna, meelem, measse,&
                  veelem, veasse, lerrit)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=W1504
    implicit none
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/nmcret.h"
#include "asterfort/nmltev.h"
#include "asterfort/nmprde.h"
#include "asterfort/nmprta.h"
    integer :: fonact(*)
    integer :: numins
    real(kind=8) :: parmet(*)
    character(len=16) :: method(*)
    character(len=19) :: matass, maprec
    character(len=24) :: sdimpr, sdtime, sdstat
    character(len=19) :: lischa, solveu, sddisc, sddyna, sdnume
    character(len=24) :: modele, mate, carele, comref, compor
    character(len=24) :: numedd, numfix
    character(len=24) :: carcri, sderro
    character(len=24) :: defico, resoco, resocu
    character(len=19) :: meelem(*), veelem(*)
    character(len=19) :: measse(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    logical :: lerrit
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! PHASE DE PREDICTION
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARIABLES DE COMMANDE DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION
! IN  SOLVEU : SOLVEUR
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  SDIMPR : SD AFFICHAGE
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : GESTION DES ERREURS
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  DEFICO : SD DEFINITION CONTACT
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  RESOCU : SD RESOLUTION LIAISON_UNILATER
! IN  SDDYNA : SD DYNAMIQUE
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  MAPREC : NOM DE LA MATRICE DE PRECONDITIONNEMENT (GCPC)
! IN  SDNUME : SD NUMEROTATION
! OUT LERRIT  : .TRUE. SI ERREUR PENDANT PREDICTION
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: faccvg, rescvg, ldccvg
    character(len=24) :: codere
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CALCUL DE PREDICTION'
    endif
!
! --- INITIALISATION CODES RETOURS
!
    codere = '&&NMPRED.CODERE'
    faccvg = -1
    rescvg = -1
    ldccvg = -1
!
! --- PREDICTION PAR LINEARISATION DU SYSTEME
!
    if (method(5) .eq. 'ELASTIQUE' .or. method(5) .eq. 'TANGENTE') then
        call nmprta(modele, numedd, numfix, mate, carele,&
                    comref, compor, lischa, method, solveu,&
                    fonact, parmet, carcri, sdimpr, sdstat,&
                    sdtime, sddisc, numins, valinc, solalg,&
                    matass, maprec, defico, resoco, resocu,&
                    sddyna, meelem, measse, veelem, veasse,&
                    sdnume, ldccvg, faccvg, rescvg, codere)
!
! --- PREDICTION PAR EXTRAPOLATION DU PAS PRECEDENT OU PAR DEPLACEMENT
! --- CALCULE
!
        elseif ((method(5) .eq. 'EXTRAPOLE').or. (method(5) .eq.&
    'DEPL_CALCULE')) then
        call nmprde(modele, numedd, numfix, mate, carele,&
                    comref, compor, lischa, method, solveu,&
                    fonact, parmet, carcri, sdimpr, sdstat,&
                    sdtime, sddisc, numins, valinc, solalg,&
                    matass, maprec, defico, resoco, sddyna,&
                    meelem, measse, veelem, veasse, ldccvg,&
                    faccvg, rescvg, codere)
    else
        ASSERT(.false.)
    endif
!
! --- TRANSFORMATION DES CODES RETOURS EN EVENEMENTS
!
    call nmcret(sderro, 'LDC', ldccvg)
    call nmcret(sderro, 'FAC', faccvg)
    call nmcret(sderro, 'RES', rescvg)
!
! --- EVENEMENT ERREUR ACTIVE ?
!
    call nmltev(sderro, 'ERRI', 'NEWT', lerrit)
!
end subroutine
