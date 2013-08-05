subroutine nmassc(fonact, sddyna, sdtime, veasse, cnpilo,&
                  cndonn)
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
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndasva.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmasdi.h"
#include "asterfort/nmasfi.h"
#include "asterfort/nmasva.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmtime.h"
#include "asterfort/vtaxpy.h"
    character(len=24) :: sdtime
    character(len=19) :: cnpilo, cndonn
    integer :: fonact(*)
    character(len=19) :: sddyna
    character(len=19) :: veasse(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CORRECTION)
!
! CALCUL DU SECOND MEMBRE POUR LA CORRECTION
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNPILO : VECTEUR ASSEMBLE DES FORCES PILOTEES
! OUT CNDONN : VECTEUR ASSEMBLE DES FORCES DONNEES
!
!
!
!
    integer :: ifm, niv
    integer :: i, nbvec, nbcoef
    character(len=19) :: cnffdo, cndfdo, cnfvdo, cnvady
    character(len=19) :: cnffpi, cndfpi, cndiri
    character(len=19) :: cnfint, cnbudi
    parameter    (nbcoef=7)
    real(kind=8) :: coef(nbcoef)
    character(len=19) :: vect(nbcoef)
    real(kind=8) :: coeequ
    logical :: ldyna
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... CALCUL SECOND MEMBRE'
    endif
!
! --- INITIALISATIONS
!
    cnffdo = '&&CNCHAR.FFDO'
    cnffpi = '&&CNCHAR.FFPI'
    cndfdo = '&&CNCHAR.DFDO'
    cndfpi = '&&CNCHAR.DFPI'
    cnfvdo = '&&CNCHAR.FVDO'
    cnvady = '&&CNCHAR.FVDY'
!
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veasse, 'VEASSE', 'CNBUDI', cnbudi)
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
!
! --- MESURES
!
    call nmtime(sdtime, 'INI', 'SECO_MEMB')
    call nmtime(sdtime, 'RUN', 'SECO_MEMB')
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
! --- CALCUL DU VECTEUR DES CHARGEMENTS VARIABLES DYNAMIQUES (NEUMANN)
!
    if (ldyna) then
        coeequ = ndynre(sddyna,'COEF_MPAS_EQUI_COUR')
        call ndasva('CORR', sddyna, veasse, cnvady)
    endif
!
! --- CHARGEMENTS DONNES AVEC PRISE EN COMPTE L'ERREUR DE L'ERREUR QUI
!    FAITE SUR LES DDLS IMPOSES (CNDFDO - CNBUDI)
!
    nbvec = 6
    coef(1) = 1.d0
    coef(2) = 1.d0
    coef(3) = -1.d0
    coef(4) = -1.d0
    coef(5) = -1.d0
    coef(6) = 1.d0
!
    vect(1) = cnffdo
    vect(2) = cnfvdo
    vect(3) = cnfint
    vect(4) = cnbudi
    vect(5) = cndiri
    vect(6) = cndfdo
!
    if (ldyna) then
        nbvec = 7
        coef(nbvec) = coeequ
        vect(nbvec) = cnvady
    endif
!
    if (nbvec .gt. nbcoef) then
        ASSERT(.false.)
    endif
    do 10 i = 1, nbvec
        call vtaxpy(coef(i), vect(i), cndonn)
10  end do
!
! --- CHARGEMENTS PILOTES
!
    nbvec = 2
    coef(1) = 1.d0
    coef(2) = 1.d0
    vect(1) = cnffpi
    vect(2) = cndfpi
    if (nbvec .gt. nbcoef) then
        ASSERT(.false.)
    endif
    do 15 i = 1, nbvec
        call vtaxpy(coef(i), vect(i), cnpilo)
15  end do
!
    call nmtime(sdtime, 'END', 'SECO_MEMB')
!
    call jedema()
end subroutine
