subroutine rc32f3(nbsigr, nocc, saltij, nupass)
    implicit   none
    integer :: nbsigr, nocc(*), nupass
    real(kind=8) :: saltij(*)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     MISE A ZERO DES LIGNES ET COLONNES DANS SALT POUR LA
!       SITUATION DE PASSAGE SI NOCC = 0
!
!     ------------------------------------------------------------------
    integer :: k, l
!     ------------------------------------------------------------------
!
    if (nocc(nupass) .eq. 0) then
        do 30 k = 1, nbsigr
            saltij(nbsigr*(k-1)+nupass) = 0.d0
30      continue
        do 32 l = 1, nbsigr
            saltij(nbsigr*(nupass-1)+l) = 0.d0
32      continue
    endif
!
end subroutine
