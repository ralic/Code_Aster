subroutine nmassd(modele, numedd, lischa, fonact, depest,&
                  veasse, matass, cnpilo, cndonn)
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
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmbudi.h"
#include "asterfort/nmchex.h"
#include "asterfort/vtaxpy.h"
    integer :: fonact(*)
    character(len=19) :: lischa
    character(len=24) :: modele, numedd
    character(len=19) :: depest
    character(len=19) :: veasse(*)
    character(len=19) :: cnpilo, cndonn, matass
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DU SECOND MEMBRE POUR LA PREDICTION - DEPLACEMENT DONNE OU
! EXTRAPOLE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  LISCHA : SD L_CHARGES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  DEPEST : DEPLACEMENT ESTIME (PAR DEPL_CALC OU EXTROPLATION)
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  MATASS : SD MATRICE ASSEMBLEE
! OUT CNPILO : VECTEUR ASSEMBLE DES FORCES PILOTEES
! OUT CNDONN : VECTEUR ASSEMBLE DES FORCES DONNEES
!
!
!
!
    integer :: ifm, niv
    integer :: nbcoef, i, nbvec
    parameter   (nbcoef=3)
    real(kind=8) :: coef(nbcoef)
    character(len=19) :: vect(nbcoef)
    character(len=19) :: vebest
    character(len=19) :: cnbest, cndido, cndidi, cndipi
    aster_logical :: ldidi
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
    vebest = '&&NMASSD.VEBEST'
    cnbest = '&&NMASSD.CNBEST'
!
! --- FONCTIONNALITES ACTIVEES
!
    ldidi = isfonc(fonact,'DIDI')
!
! --- DEPLACEMENT IMPOSES
!
    call nmchex(veasse, 'VEASSE', 'CNDIDO', cndido)
    call nmchex(veasse, 'VEASSE', 'CNDIDI', cndidi)
!
! --- DEPLACEMENT PILOTES
!
    call nmchex(veasse, 'VEASSE', 'CNDIPI', cndipi)
!
! --- CONDITIONS DE DIRICHLET B.U
!
    call nmbudi(modele, numedd, lischa, depest, vebest,&
                cnbest, matass)
!
! --- VALEURS POUR SOMME DES FORCES
!
    nbvec = 2
    coef(1) = 1.d0
    coef(2) = -1.d0
    vect(1) = cndido
    vect(2) = cnbest
    if (ldidi) then
        nbvec = nbvec+1
        vect(nbvec) = cndidi
        coef(nbvec) = 1.d0
    endif
!
! --- CHARGEMENT FIXE
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
    cnpilo = cndipi
!
! --- NETTOYAGE
!
    call detrsd('VECT_ELEM', vebest)
    call detrsd('CHAMP', cnbest)
!
    call jedema()
!
end subroutine
