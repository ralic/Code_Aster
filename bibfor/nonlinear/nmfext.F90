subroutine nmfext(eta, fonact, sddyna, veasse, cnfext)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndasva.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmasfi.h"
#include "asterfort/nmasva.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
    real(kind=8) :: eta
    integer :: fonact(*)
    character(len=19) :: sddyna
    character(len=19) :: veasse(*)
    character(len=19) :: cnfext
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! RESULTANTE DES EFFORTS EXTERIEURS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  ETA    : COEFFICIENT DE PILOTAGE
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNFEXT : CHARGEMENT EXTERIEUR RESULTANT
!
!
!
!
    integer :: ifm, niv
    character(len=19) :: cnunil, cnctdc
    character(len=19) :: cnffdo, cnffpi, cnfvdo, cnvady
    logical :: lctcd, lunil
    real(kind=8) :: coeequ
    logical :: ldyna, lallv
    integer :: ifdo, n
    character(len=19) :: vect(20)
    real(kind=8) :: coef(20)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CALCUL DES FORCES EXTERNES'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    lctcd = isfonc(fonact,'CONT_DISCRET' )
    lunil = isfonc(fonact,'LIAISON_UNILATER')
    lallv = isfonc(fonact,'CONT_ALL_VERIF' )
!
! --- INITIALISATIONS
!
    ifdo = 0
    cnffdo = '&&CNCHAR.FFDO'
    cnffpi = '&&CNCHAR.FFPI'
    cnfvdo = '&&CNCHAR.FVDO'
    cnvady = '&&CNCHAR.FVDY'
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    call vtzero(cnfext)
!
! --- FORCES DE CONTACT DISCRET
!
    if (lctcd .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNCTDC', cnctdc)
        ifdo = ifdo + 1
        coef(ifdo) = -1.d0
        vect(ifdo) = cnctdc
    endif
!
! --- FORCES DE LIAISON_UNILATER
!
    if (lunil) then
        call nmchex(veasse, 'VEASSE', 'CNUNIL', cnunil)
        ifdo = ifdo + 1
        coef(ifdo) = -1.d0
        vect(ifdo) = cnunil
    endif
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS FIXES        (NEUMANN)
!
    call nmasfi(fonact, sddyna, veasse, cnffdo, cnffpi)
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
! --- CHARGEMENTS EXTERIEURS DONNEES
!
    ifdo = ifdo + 1
    coef(ifdo) = 1.d0
    vect(ifdo) = cnffdo
    ifdo = ifdo + 1
    coef(ifdo) = 1.d0
    vect(ifdo) = cnfvdo
!
! --- CHARGEMENTS EXTERIEURS PILOTES
!
    ifdo = ifdo + 1
    coef(ifdo) = eta
    vect(ifdo) = cnffpi
!
! --- TERMES DE RAPPEL DYNAMIQUE
!
    if (ldyna) then
        ifdo = ifdo + 1
        coef(ifdo) = coeequ
        vect(ifdo) = cnvady
    endif
!
! --- VECTEUR RESULTANT
!
    if (ifdo .gt. 20) then
        call assert(.false.)
    endif
    do 10 n = 1, ifdo
        call vtaxpy(coef(n), vect(n), cnfext)
10  end do
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        call nmdebg('VECT', cnfext, ifm)
    endif
!
    call jedema()
end subroutine
