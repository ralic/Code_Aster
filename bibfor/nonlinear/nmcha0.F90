subroutine nmcha0(tychap, tyvarz, novarz, vachap)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/assert.h"
#include "asterfort/nmchai.h"
    character(len=19) :: vachap(*)
    character(len=6) :: tychap
    character(len=*) :: tyvarz, novarz
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CREATION D'UNE VARIABLE CHAPEAU
!
! ----------------------------------------------------------------------
!
!
! OUT VACHAP : VARIABLE CHAPEAU
! IN  TYCHAP : TYPE DE VARIABLE CHAPEAU
!                MEELEM - NOMS DES MATR_ELEM
!                MEASSE - NOMS DES MATR_ASSE
!                VEELEM - NOMS DES VECT_ELEM
!                VEASSE - NOMS DES VECT_ASSE
!                SOLALG - NOMS DES CHAM_NO SOLUTIONS
!                VALINC - VALEURS SOLUTION INCREMENTALE
! IN  TYVARI : TYPE DE LA VARIABLE
!               SI 'ALLINI' ALORS INITIALISE A BLANC
! IN  NOVARI : NOM DE LA VARIABLE
!
! ----------------------------------------------------------------------
!
    character(len=19) :: k19bla
    integer :: i, nbvar
    integer :: index
    character(len=6) :: tyvari
    character(len=19) :: novari
!
! ----------------------------------------------------------------------
!
    k19bla = ' '
    tyvari = tyvarz
    novari = novarz
!
    call nmchai(tychap, 'LONMAX', nbvar)
!
    if (tyvari .eq. 'ALLINI') then
        do 12 i = 1, nbvar
            vachap(i) = k19bla
12      continue
    else
        call nmchai(tychap, tyvari, index)
        if ((index.le.0) .or. (index.gt.nbvar)) then
            ASSERT(.false.)
        else
            vachap(index) = novari
        endif
!
    endif
!
end subroutine
