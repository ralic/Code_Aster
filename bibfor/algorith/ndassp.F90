subroutine ndassp(modele, numedd, mate, carele, comref,&
                  compor, lischa, carcri, sdstat, fonact,&
                  defico, sddyna, valinc, solalg, veelem,&
                  veasse, sdtime, ldccvg, codere, cndonn,&
                  sdnume, matass)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndasva.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmadir.h"
#include "asterfort/nmaint.h"
#include "asterfort/nmasdi.h"
#include "asterfort/nmasfi.h"
#include "asterfort/nmasva.h"
#include "asterfort/nmbudi.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdiri.h"
#include "asterfort/nmfint.h"
#include "asterfort/nmtime.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
    integer :: ldccvg
    integer :: fonact(*)
    character(len=19) :: lischa, sddyna, sdnume, matass
    character(len=24) :: sdstat
    character(len=24) :: modele, numedd, mate, codere, sdtime
    character(len=24) :: carele, compor, comref, carcri, defico
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: veasse(*), veelem(*)
    character(len=19) :: cndonn
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION)
!
! CALCUL DU SECOND MEMBRE POUR LA PREDICTION - DYNAMIQUE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  LISCHA : SD LISTE CHARGES
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  SDNUME : SD NUMEROTATION
! IN  MATASS  : SD MATRICE ASSEMBLEE
! OUT CNDONN : VECTEUR ASSEMBLE DES FORCES DONNEES
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
!
!
!
!
    integer :: i, nbvec, iterat, nbcoef
    character(len=19) :: cnffdo, cndfdo, cnfvdo, cnvady
    character(len=19) :: cndumm
    character(len=19) :: vebudi
    parameter    (nbcoef=8)
    real(kind=8) :: coef(nbcoef)
    character(len=19) :: vect(nbcoef)
    character(len=19) :: cnfint, cndiri, k19bla
    character(len=19) :: vefint, vediri
    character(len=19) :: cnbudi, cnvcpr
    character(len=19) :: depmoi, vitmoi, accmoi
    character(len=19) :: veclag
    logical :: ldepl, lvite, lacce
    real(kind=8) :: coeequ
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ldccvg = -1
    iterat = 0
    call vtzero(cndonn)
    cndumm = '&&CNCHAR.DUMM'
    cnffdo = '&&CNCHAR.FFDO'
    cndfdo = '&&CNCHAR.DFDO'
    cnfvdo = '&&CNCHAR.FVDO'
    cnvady = '&&CNCHAR.FVDY'
    k19bla = ' '
!
! --- TYPE DE FORMULATION SCHEMA DYNAMIQUE GENERAL
!
    ldepl = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.1
    lvite = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.2
    lacce = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.3
!
! --- COEFFICIENTS POUR MULTI-PAS
!
    coeequ = ndynre(sddyna,'COEF_MPAS_EQUI_COUR')
!
! --- MESURES
!
    call nmtime(sdtime, 'INI', 'SECO_MEMB')
    call nmtime(sdtime, 'RUN', 'SECO_MEMB')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
    call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veelem, 'VEELEM', 'CNFINT', vefint)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS FIXES        (NEUMANN)
!
    call nmasfi(fonact, sddyna, veasse, cnffdo, cndumm)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS FIXES        (DIRICHLET)
!
    call nmasdi(fonact, veasse, cndfdo, cndumm)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS VARIABLES    (NEUMANN)
!
    call nmasva(sddyna, veasse, cnfvdo)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS VARIABLES DYNAMIQUES (NEUMANN)
!
    call ndasva('PRED', sddyna, veasse, cnvady)
!
! --- SECOND MEMBRE DES VARIABLES DE COMMANDE
!
    call nmchex(veasse, 'VEASSE', 'CNVCPR', cnvcpr)
!
! --- QUEL VECTEUR D'INCONNUES PORTE LES LAGRANGES ?
!
    if (ldepl) then
        veclag = depmoi
    else if (lvite) then
!        VECLAG = VITMOI
!       VILAINE GLUTE POUR L'INSTANT
        veclag = depmoi
    else if (lacce) then
        veclag = accmoi
    else
        call assert(.false.)
    endif
!
! --- CONDITIONS DE DIRICHLET B.U
!
    call nmchex(veasse, 'VEASSE', 'CNBUDI', cnbudi)
    call nmchex(veelem, 'VEELEM', 'CNBUDI', vebudi)
    call nmbudi(modele, numedd, lischa, veclag, vebudi,&
                cnbudi, matass)
!
! --- CALCUL DES REACTIONS D'APPUI BT.LAMBDA
!
    call nmchex(veelem, 'VEELEM', 'CNDIRI', vediri)
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call nmdiri(modele, mate, carele, lischa, k19bla,&
                depmoi, k19bla, k19bla, vediri)
    call nmadir(numedd, fonact, defico, veasse, vediri,&
                cndiri)
!
    call nmtime(sdtime, 'END', 'SECO_MEMB')
!
! --- CALCUL DES FORCES INTERIEURES
!
    call nmfint(modele, mate, carele, comref, compor,&
                carcri, fonact, iterat, sddyna, sdstat,&
                sdtime, valinc, solalg, ldccvg, codere,&
                vefint)
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if (ldccvg .eq. 1) goto 9999
!
! --- ASSEMBLAGE DES FORCES INTERIEURES
!
    call nmaint(numedd, fonact, defico, veasse, vefint,&
                cnfint, sdnume)
!
! --- CHARGEMENTS DONNES
!
    nbvec = 8
    coef(1) = 1.d0
    coef(2) = 1.d0
    coef(3) = -1.d0
    coef(4) = -1.d0
    coef(5) = -1.d0
    coef(6) = 1.d0
    coef(7) = 1.d0
    coef(8) = coeequ
    vect(1) = cnffdo
    vect(2) = cnfvdo
    vect(3) = cnbudi
    vect(4) = cnfint
    vect(5) = cndiri
    vect(6) = cnvcpr
    vect(7) = cndfdo
    vect(8) = cnvady
!
! --- CHARGEMENT DONNE
!
    if (nbvec .gt. nbcoef) then
        call assert(.false.)
    endif
    do 10 i = 1, nbvec
        call vtaxpy(coef(i), vect(i), cndonn)
10  end do
!
9999  continue
!
    call jedema()
end subroutine
