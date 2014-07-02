subroutine ndasva(phase, sddyna, veasse, cnvady)
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
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
    character(len=4) :: phase
    character(len=19) :: cnvady
    character(len=19) :: veasse(*)
    character(len=19) :: sddyna
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DES COMPOSANTES DU VECTEUR SECOND MEMBRE DANS LE CAS DYNAMIQUE
!  - CHARGEMENT DE TYPE NEUMANN
!  - CHARGEMENT VARIABLE AU COURS DU PAS DE TEMPS
!  - CHARGEMENT DONNE
!
! ----------------------------------------------------------------------
!
!
! IN  PHASE  : 'PRED' OU 'CORR'
! IN  SDDYNA : SD DYNAMIQUE
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNVADY : VECT_ASSE DE TOUS LES CHARGEMENTS VARIABLES DONNES
!
!
!
!
    integer :: ifm, niv
    integer :: ifdo, n
    character(len=19) :: cnvari(20)
    real(kind=8) :: covari(20)
    character(len=19) :: cndyna, cnmoda, cnimpe
    aster_logical :: limpe, lammo
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... CALCUL NEUMANN VARIABLE '//&
     &                ' EN DYNAMIQUE'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    limpe = ndynlo(sddyna,'IMPE_ABSO')
    lammo = ndynlo(sddyna,'AMOR_MODAL')
!
! --- INITIALISATIONS
!
    ifdo = 0
    call vtzero(cnvady)
!
! --- CALCUL DES FORCES DYNAMIQUES VARIABLES
!
    call nmchex(veasse, 'VEASSE', 'CNDYNA', cndyna)
    ifdo = ifdo+1
    cnvari(ifdo) = cndyna
    covari(ifdo) = -1.d0
    if (lammo) then
        if (phase .eq. 'PRED') then
            call nmchex(veasse, 'VEASSE', 'CNMODP', cnmoda)
        else if (phase.eq.'CORR') then
            call nmchex(veasse, 'VEASSE', 'CNMODC', cnmoda)
        else
            ASSERT(.false.)
        endif
        ifdo = ifdo+1
        cnvari(ifdo) = cnmoda
        covari(ifdo) = -1.d0
    endif
    if (limpe) then
        if (phase .eq. 'PRED') then
            call nmchex(veasse, 'VEASSE', 'CNIMPP', cnimpe)
        else if (phase.eq.'CORR') then
            call nmchex(veasse, 'VEASSE', 'CNIMPC', cnimpe)
        else
            ASSERT(.false.)
        endif
        ifdo = ifdo+1
        cnvari(ifdo) = cnimpe
        covari(ifdo) = -1.d0
    endif
!
!
! --- VECTEUR RESULTANT CHARGEMENT DONNE
!
    do 10 n = 1, ifdo
        call vtaxpy(covari(n), cnvari(n), cnvady)
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ......... FORC. DYNA. DONNEES'
            write (ifm,*) '<MECANONLINE> .........  ',n,' - COEF: ',&
     &                   covari(n)
            call nmdebg('VECT', cnvari(n), ifm)
        endif
 10 end do
!
    call jedema()
end subroutine
