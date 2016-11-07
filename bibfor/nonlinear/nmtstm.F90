subroutine nmtstm(compor, imatri, matsym)
! ======================================================================
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterf_types.h"
#include "asterfort/jevech.h"
    character(len=16) :: compor(*)
    aster_logical :: matsym
    integer :: imatri
!
! ----------------------------------------------------------------------
!    RENVOIE EN FONCTION DU COMPORTEMENT LE TYPE DE MATRICE
!    SYMETRIQUE OU NON SYMETRIQUE SOUHAITE
! ----------------------------------------------------------------------
!
    matsym=.true.
    if (compor(1)(1:7) .eq. 'KIT_DDI') then
        if (compor(8)(1:10) .eq. 'BETON_UMLV') then
            matsym = .false.
        endif
    endif
!
    if (compor(1)(1:12) .eq. 'MOHR_COULOMB') then
        matsym = .false.
    endif
!
    if (compor(1)(1:16) .eq. 'ENDO_ORTH_BETON') then
        matsym = .false.
    endif
!
    if (compor(1)(1:3) .eq. 'CJS') then
        matsym = .false.
    endif
!
    if (compor(1)(1:6) .eq. 'HUJEUX') then
        matsym = .false.
    endif
!
    if (compor(1)(1:6) .eq. 'LAIGLE') then
        matsym = .false.
    endif
!
    if (compor(1)(1:4) .eq. 'LETK') then
        matsym = .false.
    endif
!
    if (compor(1)(1:3) .eq. 'LKR') then
        matsym = .false.
    endif
!
    if (compor(1)(1:10) .eq. 'HOEK_BROWN') then
        matsym = .false.
    endif
!
    if (compor(1)(1:10) .eq. 'DRUCK_PRAG_N_A') then
        matsym = .false.
    endif
!
    if (compor(1)(1:6) .eq. 'MAZARS') then
        matsym = .false.
    endif
!
    if (compor(3) .eq. 'SIMO_MIEHE') matsym = .false.
!
    if (matsym) then
        call jevech('PMATUUR', 'E', imatri)
    else
        call jevech('PMATUNS', 'E', imatri)
    endif
end subroutine
