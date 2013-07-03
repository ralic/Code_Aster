subroutine eiinit(nomte, iu, il, it)
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
!
    implicit none
#include "asterfort/assert.h"
    character(len=16) :: nomte
    integer :: iu(3, 18), il(3, 9), it(18)
! ----------------------------------------------------------------------
!            DECALAGE D'INDICE POUR LES ELEMENTS D'INTERFACE
! ----------------------------------------------------------------------
! IN  NOMTE  NOM DE L'ELEMENT FINI
! OUT IU     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
! OUT IL     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE
! OUT IT     DECALAGE D'INDICE POUR ACCEDER A LA TEMPERATURE
! ----------------------------------------------------------------------
    integer :: n
    integer :: uh20(16), lh20(4)
    integer :: up15(12), lp15(3)
    integer :: uq8(6), lq8(2)
! ----------------------------------------------------------------------
    data uh20 /1,2,3,4,9,10,11,12,5,6,7,8,17,18,19,20/
    data lh20 /13,14,15,16/
    data up15 /1,2,3,7,8,9,4,5,6,13,14,15/
    data lp15 /10,11,12/
    data uq8  /1,2,5,4,3,7/
    data lq8  /8,6/
! ----------------------------------------------------------------------
!
    if ((nomte.eq.'MEEI_HEXA20') .or. (nomte.eq.'MEEI_HEXS20')) then
        do 10 n = 1, 16
            iu(1,n) = 1 + (uh20(n)-1)*3
            iu(2,n) = 2 + (uh20(n)-1)*3
            iu(3,n) = 3 + (uh20(n)-1)*3
10      continue
!
        do 20 n = 1, 4
            il(1,n) = 1 + (lh20(n)-1)*3
            il(2,n) = 2 + (lh20(n)-1)*3
            il(3,n) = 3 + (lh20(n)-1)*3
20      continue
!
        do 30 n = 1, 16
            it(n) = uh20(n)
30      continue
!
!
        else if ((nomte.eq.'MEEI_PENTA15') .or.(nomte.eq.'MEEI_PENTS15'))&
    then
        do 110 n = 1, 12
            iu(1,n) = 1 + (up15(n)-1)*3
            iu(2,n) = 2 + (up15(n)-1)*3
            iu(3,n) = 3 + (up15(n)-1)*3
110      continue
!
        do 120 n = 1, 3
            il(1,n) = 1 + (lp15(n)-1)*3
            il(2,n) = 2 + (lp15(n)-1)*3
            il(3,n) = 3 + (lp15(n)-1)*3
120      continue
!
        do 130 n = 1, 12
            it(n) = up15(n)
130      continue
!
        else if ((nomte.eq.'EIPLQU8') .or.(nomte.eq.'EIPLQS8') .or.(&
    nomte.eq.'EIAXQU8') .or.(nomte.eq.'EIAXQS8')) then
!
        do 210 n = 1, 6
            iu(1,n) = 1 + (uq8(n)-1)*2
            iu(2,n) = 2 + (uq8(n)-1)*2
210      continue
!
        do 220 n = 1, 2
            il(1,n) = 1 + (lq8(n)-1)*2
            il(2,n) = 2 + (lq8(n)-1)*2
220      continue
!
        do 230 n = 1, 6
            it(n) = uq8(n)
230      continue
!
    else
        call assert(.false.)
    endif
!
end subroutine
