subroutine nmsuiy(sdimpr, valr, isuiv)
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
    implicit     none
    include 'asterfort/impfoi.h'
    include 'asterfort/nmimcr.h'
    character(len=24) :: sdimpr
    real(kind=8) :: valr
    integer :: isuiv
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (SUIVI_DDL - UTILITAIRE)
!
! ECRITURE DANS LE TABLEAU DE CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  VALR   : VALEUR A ECRIRE DANS LE TABLEAU
! I/O ISUIV  : NUMERO COURANT DU SUIVI_DDL
!
! ----------------------------------------------------------------------
!
    character(len=9) :: typcol
    character(len=1) :: indsui
!
! ----------------------------------------------------------------------
!
!
!
! --- AFFICHAGE DANS LE TABLEAU
!
    call impfoi(0, 1, isuiv, indsui)
    typcol = 'SUIVDDL'//indsui
    call nmimcr(sdimpr, typcol, valr, .true.)
!
! --- SUIVI_DDL SUIVANT
!
    isuiv = isuiv + 1
!
end subroutine
