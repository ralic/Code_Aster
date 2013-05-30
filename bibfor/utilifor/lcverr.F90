subroutine lcverr(dy, ddy, nr, typ, err)
    implicit none
!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------
!       MODULE DE CALCUL DE L'ERREUR DE CONVERGENCE
!       IN  DY     :    VECTEUR SOLUTION
!           DDY    :    VECTEUR CORRECTION SUR LA SOLUTION
!           NR     :    DIMENSION DE DY DDY
!           TYP    :    TYPE D'ERREUR A CALCULER
!                               0 = IDDYI/DYII     < EPS (POUR TOUT I)
!                               1 = IIDDYII/IIDYII < EPS
!                               2 = IIDDYI/DYIII   < EPS
!       OUT ERR    :    VECTEUR ERREUR
!       ----------------------------------------------------------------
    include 'asterc/r8prem.h'
    include 'asterfort/lcnrvn.h'
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    parameter       ( zero = 0.d0   )
    integer :: n, nd, nr, typ
    real(kind=8) :: dy(*), ddy(*)
    real(kind=8) :: err(*), e(50)
!       ----------------------------------------------------------------
    common /tdim/   n , nd
!       ----------------------------------------------------------------
!
!       ERREUR(I) =  !DDYI/DYI! < EPS
!
    if (typ .eq. 0) then
        err(1)=0.d0
        do 1 i = 1, nr
!                IF(DY(I).EQ.ZERO) THEN
            if (abs(dy(i)) .lt. r8prem()) then
                err(i) = abs(ddy(i))
            else
                err(i) = abs(ddy(i) / dy(i))
            endif
            err(1)=max(err(1),err(i))
 1      continue
!
!       ERREUR = !!DDY!!/!!DY!! < EPS
!
    else if (typ .eq. 1) then
        call lcnrvn(nr, ddy, e(1))
        call lcnrvn(nr, dy, e(2))
        if (e(2) .eq. zero) then
            err(1) = e(1)
        else
            err(1) = e(1) / e(2)
        endif
!
!       ERREUR = !!DDYI/DYI!! < EPS
!
    else if (typ .eq. 2) then
        do 2 i = 1, nr
            if (abs(dy(i)) .lt. r8prem()) then
                e(i) = ddy(i)
            else
                e(i) = ddy(i) / dy(i)
            endif
 2      continue
        call lcnrvn(nr, e, err(1))
!
    endif
!
end subroutine
