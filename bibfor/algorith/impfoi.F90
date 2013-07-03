subroutine impfoi(unite, long, vali, chaine)
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
#include "asterfort/assert.h"
    integer :: unite
    integer :: long
    integer :: vali
    character(len=*) :: chaine
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (AFFICHAGE - UTILITAIRE)
!
! IMPRESSION D'UN ENTIER SUR UNE UNITE LOGICIELLE
!
! ----------------------------------------------------------------------
!
!
! IN  UNITE  : UNITE LOGICIELLE D'IMPRESSION
! IN  LONG   : LONGUEUR D'AFFICHAGE DU NOMBRE
! IN  VALI   : VALEUR ENTIERE A AFFICHER
! OUT CHAINE : CHAINE DE SORTIE SI UNITE = 0
!
! ----------------------------------------------------------------------
!
    character(len=4) :: for4
    character(len=5) :: for5
    character(len=1) :: for1
    integer :: lonfor
!
! ----------------------------------------------------------------------
!
!
    if (long .le. 9) then
        lonfor = 4
        for4(1:2) = '(I'
        write(for1,'(I1)') long
        for4(3:3) = for1
        for4(4:4) = ')'
    else if (long.le.19) then
        lonfor = 5
        for5(1:2) = '(I'
        for5(3:3) = '1'
        write(for1,'(I1)') long-10
        for5(4:4) = for1
        for5(5:5) = ')'
    else
        call assert(.false.)
    endif
!
    if (unite .ne. 0) then
        if (lonfor .eq. 4) then
            write(unite,for4) vali
        else if (lonfor.eq.5) then
            write(unite,for5) vali
        else
            call assert(.false.)
        endif
    else
        if (lonfor .eq. 4) then
            write(chaine,for4) vali
        else if (lonfor.eq.5) then
            write(chaine,for5) vali
        else
            call assert(.false.)
        endif
    endif
!
!
end subroutine
