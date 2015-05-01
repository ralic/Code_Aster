subroutine nmchap(valinc, solalg, meelem, veelem, veasse,&
                  measse)
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
    implicit     none
#include "asterfort/infdbg.h"
#include "asterfort/nmch1p.h"
#include "asterfort/nmch2p.h"
#include "asterfort/nmch3p.h"
#include "asterfort/nmch4p.h"
#include "asterfort/nmch5p.h"
#include "asterfort/nmch6p.h"
    character(len=19) :: veelem(*), meelem(*)
    character(len=19) :: veasse(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DES VARIABLES CHAPEAUX
!
! ----------------------------------------------------------------------
!
!
! ON NE FAIT QUE REMPLIR LES VARIABLES CHAPEAUX AVEC LES NOMS
! LA CREATION DES SD EST FAITE PRINCIPALEMENT DANS NMCRCH
!
! OUT VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! OUT SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! OUT MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! OUT VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION VARIABLES CHAPEAUX'
    endif
!
! --- VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
!
    call nmch1p(valinc)
!
! --- VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
    call nmch2p(solalg)
!
! --- VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
!
    call nmch3p(meelem)
!
! --- VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
!
    call nmch4p(veelem)
!
! --- VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
    call nmch5p(veasse)
!
! --- VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
!
    call nmch6p(measse)
!
end subroutine
