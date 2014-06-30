subroutine i2rgel(epsi, s, r, f, sl,&
                  r1l, r2l, f1l, f2l, adr)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!********************************************************************
!
!       RANGEMENT DU REEL S DANS LE TABLEAU SL
!       ET DE R ET F DANS LES AUTRES TABLEAUX SUIVANT LES CAS DE
!       FIGURE (I.E. SUIVANT QUE S EST OU N' EST PAS DEJA LA)
!
!       ADR : INDICE DU PREMIER ELEMENT LIBRE DANS SL
!
!       INVARIANT DE LA ROUTINE :
!
!           SL(1)<SL(2)< ... <SL(ADR-1)
!           SL(I) LIBRE (EN FAIT -1.0) POUR I = ADR, ... ,DIM_SL
!           SL(I) <-- ABSCISSE SUIVANT AB DU IEME POINT,LOCAL,
!                     D' INTERSECTION POUR I = 1, ..., ADR
!
!*******************************************************************
!
    integer :: adr
    real(kind=8) :: s, r, sl(*), r1l(*), r2l(*), epsi
    integer :: f, f1l(*), f2l(*)
!
    logical(kind=1) :: trouve, dejala
    integer :: i, j
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    trouve = .false.
    dejala = .false.
    i = 1
    j = 1
!
10  continue
    if ((.not. trouve) .and. (i .lt. adr)) then
!
        if (abs(sl(i)-s) .lt. epsi) then
!
            trouve = .true.
            dejala = .true.
!
        else
!
            if (sl(i) .lt. s) then
!
                i = i + 1
!
            else
!
                trouve = .true.
!
            endif
!
        endif
!
        goto 10
!
    endif
!
    if (dejala) then
!
        r2l(i) = r
        f2l(i) = f
!
    else
!
        do 20, j = adr, i+1, -1
!
        sl (j) = sl(j-1)
        r1l(j) = r1l(j-1)
        r2l(j) = r2l(j-1)
        f1l(j) = f1l(j-1)
        f2l(j) = f2l(j-1)
!
20      continue
!
        sl (i) = s
        r1l(i) = r
        r2l(i) = -1.0d0
        f1l(i) = f
        f2l(i) = 0
!
        adr = adr + 1
!
    endif
!
end subroutine
