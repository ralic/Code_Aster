subroutine impfor(unite, long, prec, valr, chaine)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
    integer :: unite
    integer :: long
    integer :: prec
    real(kind=8) :: valr
    character(len=*) :: chaine
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (AFFICHAGE - UTILITAIRE)
!
! IMPRESSION D'UN REEL SUR UNE UNITE LOGICIELLE
!
! ----------------------------------------------------------------------
!
!
! IN  UNITE  : UNITE LOGICIELLE D'IMPRESSION
! IN  LONG   : LONGUEUR D'AFFICHAGE DU NOMBRE
! IN  PRECI  : PRECISION EVENTUELLE D'UN NOMBRE REEL
! IN  VALR   : VALEUR REELLE A AFFICHER
! OUT CHAINE : CHAINE DE SORTIE SI UNITE = 0
!
! ----------------------------------------------------------------------
!
    character(len=8) :: for8
    character(len=9) :: for9
    character(len=1) :: for1
    integer :: lonfor
!
! ----------------------------------------------------------------------
!
!
    if ((prec.lt.1) .or. (prec.gt.9)) then
        prec = 5
    endif
!
    if (valr .eq. r8vide()) then
        if (unite .ne. 0) then
            write(unite,'(A)') ' '
        else
            write(chaine,'(A)') ' '
        endif
        goto 99
    endif
!
    if (long .le. 9) then
        lonfor = 8
        for8(1:4) = '(1PE'
        write(for1,'(I1)') long
        for8(5:5) = for1
        for8(6:6) = '.'
        write(for1,'(I1)') prec
        for8(7:7) = for1
        for8(8:8) = ')'
    else if (long.le.19) then
        lonfor = 9
        for9(1:4) = '(1PE'
        for9(5:5) = '1'
        write(for1,'(I1)') long-10
        for9(6:6) = for1
        for9(7:7) = '.'
        write(for1,'(I1)') prec
        for9(8:8) = for1
        for9(9:9) = ')'
    else
        call assert(.false.)
    endif
!
!
    if (unite .ne. 0) then
        if (lonfor .eq. 8) then
            write(unite,for8) valr
        else if (lonfor.eq.9) then
            write(unite,for9) valr
        else
            call assert(.false.)
        endif
    else
        if (lonfor .eq. 8) then
            write(chaine,for8) valr
        else if (lonfor.eq.9) then
            write(chaine,for9) valr
        else
            call assert(.false.)
        endif
    endif
!
99  continue
!
end subroutine
