subroutine ndthet(fonact, sddyna, foiner, veasse, cnfint,&
                  cnfext)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmasfi.h"
#include "asterfort/nmasva.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
    integer :: fonact(*)
    character(len=19) :: sddyna
    character(len=19) :: veasse(*)
    character(len=19) :: foiner, cnfint, cnfext
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! RESULTANTE DES EFFORTS POUR ESTIMATION DE L'EQUILIBRE
! CAS DU THETA_SCHEMA EN VITESSE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  FOINER : VECTEUR DES FORCES D'INERTIE POUR CONVERGENCE
! OUT CNFINT : VECTEUR RESULTANT DES FORCES INTERNES
! OUT CNFEXT : VECTEUR RESULTANT DES FORCES EXTERNES
!
!
!
!
    integer :: ifm, niv
    character(len=19) :: cndumm, cnunil, cnctdc
    character(len=19) :: cnffdo, cnfvdo
    character(len=19) :: fintpr, fintco
    aster_logical :: lctcd, lunil, lkrenk, lallv
    integer :: ifdo, n
    character(len=19) :: vect(20)
    real(kind=8) :: coef(20)
    real(kind=8) :: coeext, coeex2, kappa
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
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
    cnfvdo = '&&CNCHAR.FVDO'
    cndumm = '&&CNCHAR.DUMM'
    call vtzero(cnfext)
!
! --- COEFFICIENTS POUR MULTI-PAS
!
    coeext = ndynre(sddyna,'COEF_MPAS_FEXT_PREC')
    coeex2 = ndynre(sddyna,'COEF_MPAS_FEXT_COUR')
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS FIXES        (NEUMANN)
!
    call nmasfi(fonact, sddyna, veasse, cnffdo, cndumm)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS VARIABLES    (NEUMANN)
!
    call nmasva(sddyna, veasse, cnfvdo)
!
! --- CHARGEMENTS EXTERIEURS DONNEES DANS CNFEXT
!
    ifdo = ifdo + 1
    coef(ifdo) = 1.d0
    vect(ifdo) = cnffdo
    ifdo = ifdo + 1
    coef(ifdo) = 1.d0
    vect(ifdo) = cnfvdo
!
! --- FORCES DE CONTACT DISCRET DANS CNFEXT
!
    if (lctcd .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNCTDC', cnctdc)
        ifdo = ifdo + 1
        coef(ifdo) = -1.d0
        vect(ifdo) = cnctdc
    endif
!
! --- FORCES DE LIAISON_UNILATER DANS CNFEXT
!
    if (lunil) then
        call nmchex(veasse, 'VEASSE', 'CNUNIL', cnunil)
        ifdo = ifdo + 1
        coef(ifdo) = -1.d0
        vect(ifdo) = cnunil
    endif
!
! --- CHARGEMENTS DES TERMES INERTIE DANS CNFEXT
!
    ifdo = ifdo + 1
    coef(ifdo) = 1.d0
    vect(ifdo) = foiner
!
! --- VECTEUR RESULTANT DES FORCES EXTERNES
!
    if (ifdo .gt. 20) then
        ASSERT(.false.)
    endif
    do 10 n = 1, ifdo
        call vtaxpy(coef(n), vect(n), cnfext)
 10 end do
!
! --- INITIALISATIONS POUR FORCES INTERNES
!
    ifdo = 0
    call vtzero(cnfint)
    call nmchex(veasse, 'VEASSE', 'CNFINT', fintco)
    call ndynkk(sddyna, 'OLDP_CNFINT', fintpr)
!
! --- AJOUT FORCES INTERNES
! --- ON VEUT:
!          (1-THETA)*FINTPR +  (THETA)*FINTCO
!
    lkrenk = ndynlo(sddyna,'KRENK')
!
    if (lkrenk) then
        kappa = ndynre(sddyna,'KAPPA')
        coeext = (1.d0 - (kappa/2.d0))
        coeex2 = kappa/2.d0
    endif
    ifdo = ifdo + 1
    coef(ifdo) = coeext
    vect(ifdo) = fintpr
    ifdo = ifdo + 1
    coef(ifdo) = coeex2
    vect(ifdo) = fintco
!
! --- VECTEUR RESULTANT DES FORCES INTERNES
!
    if (ifdo .gt. 20) then
        ASSERT(.false.)
    endif
    do 13 n = 1, ifdo
        call vtaxpy(coef(n), vect(n), cnfint)
 13 end do
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        call nmdebg('VECT', cnfext, ifm)
        call nmdebg('VECT', cnfint, ifm)
    endif
!
    call jedema()
end subroutine
