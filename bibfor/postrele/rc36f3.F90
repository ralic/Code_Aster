subroutine rc36f3(nbsigr, nocc, saltij, nupass)
    implicit   none
    integer :: nbsigr, nocc(*), nupass
    real(kind=8) :: saltij(*)
!     ------------------------------------------------------------------
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
!
!     MISE A ZERO DES LIGNES ET COLONNES DANS SALT POUR LA
!       SITUATION DE PASSAGE SI NOCC = 0
!
!     ------------------------------------------------------------------
    integer :: k, l, i1
    logical :: colona, colonb, lignea, ligneb
!     ------------------------------------------------------------------
!
    colona = .false.
    colonb = .false.
    lignea = .false.
    ligneb = .false.
!
    if (nocc(2*(nupass-1)+1) .eq. 0) colona = .true.
    if (nocc(2*(nupass-1)+2) .eq. 0) colonb = .true.
    if (nocc(2*(nupass-1)+1) .eq. 0) lignea = .true.
    if (nocc(2*(nupass-1)+2) .eq. 0) ligneb = .true.
!
    if (colona) then
        do 30 k = 1, nbsigr
            i1 = 4*nbsigr*(k-1)
            saltij(i1+4*(nupass-1)+1) = 0.d0
            saltij(i1+4*(nupass-1)+2) = 0.d0
30      continue
        i1 = 4*nbsigr*(nupass-1)
        do 32 l = 1, nbsigr
            saltij(i1+4*(l-1)+1) = 0.d0
            saltij(i1+4*(l-1)+3) = 0.d0
32      continue
    endif
!
    if (colonb) then
        do 40 k = 1, nbsigr
            i1 = 4*nbsigr*(k-1)
            saltij(i1+4*(nupass-1)+3) = 0.d0
            saltij(i1+4*(nupass-1)+4) = 0.d0
40      continue
        i1 = 4*nbsigr*(nupass-1)
        do 42 l = 1, nbsigr
            saltij(i1+4*(l-1)+2) = 0.d0
            saltij(i1+4*(l-1)+4) = 0.d0
42      continue
    endif
!
    if (lignea) then
        do 50 k = 1, nbsigr
            i1 = 4*nbsigr*(k-1)
            saltij(i1+4*(nupass-1)+1) = 0.d0
            saltij(i1+4*(nupass-1)+2) = 0.d0
50      continue
        i1 = 4*nbsigr*(nupass-1)
        do 52 l = 1, nbsigr
            saltij(i1+4*(l-1)+1) = 0.d0
            saltij(i1+4*(l-1)+3) = 0.d0
52      continue
    endif
!
    if (ligneb) then
        do 60 k = 1, nbsigr
            i1 = 4*nbsigr*(k-1)
            saltij(i1+4*(nupass-1)+3) = 0.d0
            saltij(i1+4*(nupass-1)+4) = 0.d0
60      continue
        i1 = 4*nbsigr*(nupass-1)
        do 62 l = 1, nbsigr
            saltij(i1+4*(l-1)+2) = 0.d0
            saltij(i1+4*(l-1)+4) = 0.d0
62      continue
    endif
!
end subroutine
