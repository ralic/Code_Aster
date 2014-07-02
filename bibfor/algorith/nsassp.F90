subroutine nsassp(modele, numedd, lischa, fonact, sddyna,&
                  sdtime, valinc, veelem, veasse, cnpilo,&
                  cndonn, mate, carele, defico, matass)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmadir.h"
#include "asterfort/nmasdi.h"
#include "asterfort/nmasfi.h"
#include "asterfort/nmasva.h"
#include "asterfort/nmbudi.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdiri.h"
#include "asterfort/nmtime.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
    integer :: fonact(*)
    character(len=19) :: lischa, sddyna, matass
    character(len=24) :: modele, numedd, mate, carele, defico, sdtime
    character(len=19) :: veasse(*), veelem(*), valinc(*)
    character(len=19) :: cnpilo, cndonn
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION)
!
! CALCUL DU SECOND MEMBRE POUR LA PREDICTION - STATIQUE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  LISCHA : SD L_CHARGES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNPILO : VECTEUR ASSEMBLE DES FORCES PILOTEES
! OUT CNDONN : VECTEUR ASSEMBLE DES FORCES DONNEES
! IN  MATASS : SD MATRICE ASSEMBLEE
!
!
!
!
    integer :: i, nbvec, nbcoef
    character(len=19) :: cnffdo, cndfdo, cnfvdo
    character(len=19) :: cnffpi, cndfpi, cndiri
    character(len=19) :: vebudi, vediri
    parameter    (nbcoef=9)
    real(kind=8) :: coef(nbcoef)
    character(len=19) :: vect(nbcoef)
    character(len=19) :: cnfnod, cnbudi, cnvcpr, cnsstr, cneltc, cneltf
    character(len=19) :: depmoi, k19bla
    aster_logical :: lmacr, leltc, leltf, lallv
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call vtzero(cndonn)
    call vtzero(cnpilo)
    cnffdo = '&&CNCHAR.FFDO'
    cnffpi = '&&CNCHAR.FFPI'
    cndfdo = '&&CNCHAR.DFDO'
    cndfpi = '&&CNCHAR.DFPI'
    cnfvdo = '&&CNCHAR.FVDO'
    k19bla = ' '
!
! --- FONCTIONNALITES ACTIVEES
!
    lmacr = isfonc(fonact,'MACR_ELEM_STAT')
    leltc = isfonc(fonact,'ELT_CONTACT')
    leltf = isfonc(fonact,'ELT_FROTTEMENT')
    lallv = isfonc(fonact,'CONT_ALL_VERIF' )
!
! --- MESURES
!
    call nmtime(sdtime, 'INI', 'SECO_MEMB')
    call nmtime(sdtime, 'RUN', 'SECO_MEMB')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS FIXES        (NEUMANN)
!
    call nmasfi(fonact, sddyna, veasse, cnffdo, cnffpi)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS FIXES        (DIRICHLET)
!
    call nmasdi(fonact, veasse, cndfdo, cndfpi)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS VARIABLES    (NEUMANN)
!
    call nmasva(sddyna, veasse, cnfvdo)
!
! --- SECOND MEMBRE DES VARIABLES DE COMMANDE
!
    call nmchex(veasse, 'VEASSE', 'CNVCPR', cnvcpr)
!
! --- FORCES NODALES
!
    call nmchex(veasse, 'VEASSE', 'CNFNOD', cnfnod)
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
! --- CONDITIONS DE DIRICHLET B.U
!
    call nmchex(veasse, 'VEASSE', 'CNBUDI', cnbudi)
    call nmchex(veelem, 'VEELEM', 'CNBUDI', vebudi)
    call nmbudi(modele, numedd, lischa, depmoi, vebudi,&
                cnbudi, matass)
!
! --- CHARGEMENTS DONNES
!
    nbvec = 7
    coef(1) = 1.d0
    coef(2) = 1.d0
    coef(3) = -1.d0
    coef(4) = -1.d0
    coef(5) = -1.d0
    coef(6) = 1.d0
    coef(7) = 1.d0
    vect(1) = cnffdo
    vect(2) = cnfvdo
    vect(3) = cndiri
    vect(4) = cnbudi
    vect(5) = cnfnod
    vect(6) = cnvcpr
    vect(7) = cndfdo
!
! --- FORCES ISSUES DES MACRO-ELEMENTS STATIQUES
!
    if (lmacr) then
        call nmchex(veasse, 'VEASSE', 'CNSSTR', cnsstr)
        nbvec = nbvec + 1
        coef(nbvec) = 1.d0
        vect(nbvec) = cnsstr
    endif
!
! --- FORCES DES ELEMENTS DE CONTACT (XFEM+CONTINUE)
!
    if (leltc .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNELTC', cneltc)
        nbvec = nbvec + 1
        coef(nbvec) = -1.d0
        vect(nbvec) = cneltc
    endif
    if (leltf .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNELTF', cneltf)
        nbvec = nbvec + 1
        coef(nbvec) = -1.d0
        vect(nbvec) = cneltf
    endif
!
! --- CHARGEMENT DONNE
!
    if (nbvec .gt. nbcoef) then
        ASSERT(.false.)
    endif
    do 10 i = 1, nbvec
        call vtaxpy(coef(i), vect(i), cndonn)
 10 end do
!
! --- CHARGEMENT PILOTE
!
    nbvec = 2
    coef(1) = 1.d0
    coef(2) = 1.d0
    vect(1) = cnffpi
    vect(2) = cndfpi
    if (nbvec .gt. nbcoef) then
        ASSERT(.false.)
    endif
    do 18 i = 1, nbvec
        call vtaxpy(coef(i), vect(i), cnpilo)
 18 end do
!
    call nmtime(sdtime, 'END', 'SECO_MEMB')
!
    call jedema()
end subroutine
