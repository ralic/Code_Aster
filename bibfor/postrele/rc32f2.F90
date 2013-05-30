subroutine rc32f2(nbsigr, nocc, saltij, isk, isl,&
                  nk, nl, n0)
    implicit   none
    integer :: nbsigr, nocc(*), isk, isl, nk, nl, n0
    real(kind=8) :: saltij(*)
!     ------------------------------------------------------------------
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
!     MISE A ZERO DES LIGNES ET COLONNES DANS SALT POUR LES
!     SITUATION K ET L SI NOCC = 0
!
!     ------------------------------------------------------------------
    integer :: k, l
!     ------------------------------------------------------------------
!
    nocc(isl) = nl - n0
    if (nocc(isl) .eq. 0) then
        do 30 k = 1, nbsigr
            saltij(nbsigr*(k-1)+isl) = 0.d0
30      continue
        do 32 l = 1, nbsigr
            saltij(nbsigr*(isl-1)+l) = 0.d0
32      continue
    endif
!
    nocc(isk) = nk - n0
    if (nocc(isk) .eq. 0) then
        do 50 k = 1, nbsigr
            saltij(nbsigr*(k-1)+isk) = 0.d0
50      continue
        do 52 l = 1, nbsigr
            saltij(nbsigr*(isk-1)+l) = 0.d0
52      continue
    endif
!
end subroutine
