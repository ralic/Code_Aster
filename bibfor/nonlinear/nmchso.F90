subroutine nmchso(chapin, tychaz, typsoz, nomvaz, chapou)
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
#include "asterfort/nmchai.h"
    character(len=19) :: chapin(*), chapou(*)
    character(len=*) :: tychaz, typsoz
    character(len=*) :: nomvaz
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! RECOPIE UN VECTEUR CHAPEAU EN CHANGEANT EVENTUELLEMENT UN NOM DE
! VARIABLE
!
! ----------------------------------------------------------------------
!
!
! IN  CHAPIN : VARIABLE CHAPEAU ENTRANTE
! IN  TYCHAP : TYPE DE VARIABLE CHAPEAU
!                MEELEM - NOMS DES MATR_ELEM
!                MEASSE - NOMS DES MATR_ASSE
!                VEELEM - NOMS DES VECT_ELEM
!                VEASSE - NOMS DES VECT_ASSE
!                SOLALG - NOMS DES CHAM_NO SOLUTION
!                VALINC - VALEURS SOLUTION INCREMENTALE
! IN  TYPSOL : TYPE DE VARIABLE A REMPLACER
!              ' ' SI PAS DE CHANGEMENT
! IN  NOMVAR : NOM DE LA VARIABLE
! OUT CHAPOU : VARIABLE CHAPEAU SORTANTE
!
! ----------------------------------------------------------------------
!
    integer :: index, i, nmax
    character(len=19) :: chtemp, nomvar
    character(len=6) :: tychap, typsol
!
! ----------------------------------------------------------------------
!
    nomvar = nomvaz
    tychap = tychaz
    typsol = typsoz
    call nmchai(tychap, 'LONMAX', nmax)
    if (typsol .ne. ' ') then
        call nmchai(tychap, typsol, index)
    endif
!
! --- INITIALISATION DES NOMS
!
!     -- PARFOIS CHAPOU ET CHAPIN SONT IDENTIQUES (MEMES TABLEAUX)
!        CELA PROVOQUE UN MESSAGE DE VALGRIND.
!        POUR EVITER CE PROBLEME, ON RECOPIE EN 2 TEMPS
!
    do 11 i = 1, nmax
        chtemp = chapin(i)
        chapou(i) = chtemp
11  end do
!
! --- REMPLACEMENT
!
    if (typsol .ne. ' ') then
        chapou(index) = nomvar
    endif
!
end subroutine
