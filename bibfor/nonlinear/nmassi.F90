subroutine nmassi(modele, numedd, lischa, fonact, sddyna,&
                  valinc, veelem, veasse, cndonn, matass)
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
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmacfi.h"
#include "asterfort/nmacva.h"
#include "asterfort/nmbudi.h"
#include "asterfort/nmchex.h"
#include "asterfort/utmess.h"
#include "asterfort/vtaxpy.h"
    integer :: fonact(*)
    character(len=19) :: sddyna, lischa
    character(len=24) :: numedd, modele
    character(len=19) :: valinc(*)
    character(len=19) :: veasse(*), veelem(*)
    character(len=19) :: cndonn, matass
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DU SECOND MEMBRE POUR LE CALCUL DE L'ACCELERATION INITIALE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  LISCHA : LISTE DES CHARGES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  SDDYNA : SD DYNAMIQUE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! OUT CNDONN : SECOND MEMBRE CALCULE
! IN  MATASS : SD MATRICE ASSEMBLEE
!
!
!
!
    integer :: ifm, niv
    integer :: i, nbvec, nbcoef
    character(len=19) :: vebudi
    character(len=19) :: cnffdo, cndfdo, cnfvdo
    parameter    (nbcoef=8)
    real(kind=8) :: coef(nbcoef)
    character(len=19) :: vect(nbcoef)
    character(len=19) :: cnfnod, cnbudi, depmoi
    aster_logical :: londe, llapl
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
! --- FONCTIONNALITES ACTIVEES
!
    londe = ndynlo(sddyna,'ONDE_PLANE')
    llapl = isfonc(fonact,'LAPLACE')
    if (londe .or. llapl) then
        call utmess('A', 'MECANONLINE_23')
    endif
!
! --- INITIALISATIONS
!
    cnffdo = '&&CNCHAR.FFDO'
    cndfdo = '&&CNCHAR.DFDO'
    cnfvdo = '&&CNCHAR.FVDO'
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
!
! --- CALCUL DU VECTEUR DES FORCES FIXES
!
    call nmacfi(fonact, veasse, cnffdo, cndfdo)
!
! --- CALCUL DU VECTEUR DES FORCES VARIABLES
!
    call nmacva(veasse, cnfvdo)
!
! --- FORCES NODALES
!
    call nmchex(veasse, 'VEASSE', 'CNFNOD', cnfnod)
!
! --- CONDITIONS DE DIRICHLET B.U
!
    call nmchex(veasse, 'VEASSE', 'CNBUDI', cnbudi)
    call nmchex(veelem, 'VEELEM', 'CNBUDI', vebudi)
    call nmbudi(modele, numedd, lischa, depmoi, vebudi,&
                cnbudi, matass)
!
! --- VALEURS POUR SOMME DES FORCES
!
    nbvec = 5
    coef(1) = 1.d0
    coef(2) = 1.d0
    coef(3) = -1.d0
    coef(4) = -1.d0
    coef(5) = 1.d0
    vect(1) = cnffdo
    vect(2) = cnfvdo
    vect(3) = cnfnod
    vect(4) = cnbudi
    vect(5) = cndfdo
!
! --- FORCES DONNEES
!
    if (nbvec .gt. nbcoef) then
        ASSERT(.false.)
    endif
    do 10 i = 1, nbvec
        call vtaxpy(coef(i), vect(i), cndonn)
 10 end do
!
    call jedema()
end subroutine
