subroutine nmequi(eta, fonact, sddyna, veasse,&
                  cnfext, cnfint)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/ndynin.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmfext.h"
    real(kind=8) :: eta
    integer :: fonact(*)
    character(len=19) :: sddyna
    character(len=19) :: veasse(*)
    character(len=19) :: cnfext, cnfint
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! RESULTANTE DES EFFORTS POUR ESTIMATION DE L'EQUILIBRE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  ETA    : COEFFICIENT DE PILOTAGE
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
!
!
!
    integer :: ifm, niv
    aster_logical :: ldyna, lstat
    aster_logical :: lnewma
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CALCUL DES FORCES POUR '//&
        'ESTIMATION DE L''EQUILIBRE'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
!
! --- INITIALISATIONS
!
    lnewma = .false.
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lstat = ndynlo(sddyna,'STATIQUE')
    if (ldyna) then
        lnewma = ndynlo(sddyna,'FAMILLE_NEWMARK')
    endif
!
! --- VECTEURS EN SORTIE
!
    if (lstat .or. lnewma) then
        call nmchex(veasse, 'VEASSE', 'CNFEXT', cnfext)
        call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    else
        ASSERT(.false.)
    endif
!
! --- CALCUL DES TERMES
!
    if (lstat .or. lnewma) then
        call nmfext(eta, fonact, sddyna, veasse, cnfext)
    else
        ASSERT(.false.)
    endif
!
!
    call jedema()
end subroutine
