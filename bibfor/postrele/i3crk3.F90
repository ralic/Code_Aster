subroutine i3crk3(typk, face, crf, crk)
    implicit none
!
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: typk, face
    real(kind=8) :: crf(*), crk(*)
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     PASSAGE DES COORDO_REF_FACE AUX COORDO_REF_MAILLE
!     ------------------------------------------------------------------
! IN  TYPK   : I : TYPE DE LA MAILLE EI : TRETRA/PENTA/HEXA
! IN  FACE   : I : NUMERO DE LA FACE
! IN  CRF    : R : TABLE (1..2) DES COORDO_REF_FACE
! OUT CRK    : R : TABLE (1..3) DES COORDO_REF_MAILLE
!     ------------------------------------------------------------------
!
    real(kind=8) :: zero, un, unsur2, unsur4, unsur3, r1, r2
!
!=======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    zero = 0.00d0
    unsur4 = 0.25d0
    unsur2 = 0.50d0
    un = 1.00d0
    unsur3 = un/3.0d0
    r1 = crf(1)
    r2 = crf(2)
    if (typk .eq. 1) then
        if (face .eq. 0) then
            crk(1) = unsur4
            crk(2) = unsur4
            crk(3) = unsur4
        else if (face .eq. 1) then
            crk(1) = r2
            crk(2) = r1
            crk(3) = zero
        else if (face .eq. 2) then
            crk(2) = r2
            crk(3) = r1
            crk(1) = zero
        else if (face .eq. 3) then
            crk(3) = r2
            crk(1) = r1
            crk(2) = zero
        else if (face .eq. 4) then
            crk(3) = r2
            crk(2) = r1
            crk(1) = un - r1 -r2
        else
            call u2mesk('F', 'INTEMAIL_6', 1, 'TETRA')
        endif
    else if (typk .eq. 2) then
        if (face .eq. 0) then
            crk(1) = unsur3
            crk(2) = unsur3
            crk(3) = zero
        else if (face .eq. 1) then
            crk(1) = r2
            crk(2) = r1
            crk(3) = -un
        else if (face .eq. 2) then
            crk(1) = zero
            crk(2) = (un + r2)*unsur2
            crk(3) = r1
        else if (face .eq. 3) then
            crk(2) = zero
            crk(1) = (un + r1)*unsur2
            crk(3) = r2
        else if (face .eq. 4) then
            crk(1) = r1
            crk(2) = r2
            crk(3) = un
        else if (face .eq. 5) then
            crk(1) = (un - r1)*unsur2
            crk(2) = (un + r1)*unsur2
            crk(3) = r2
        else
            call u2mesk('F', 'INTEMAIL_6', 1, 'PENTA')
        endif
    else if (typk .eq. 3) then
        if (face .eq. 0) then
            crk(1) = zero
            crk(2) = zero
            crk(3) = zero
        else if (face .eq. 1) then
            crk(2) = r1
            crk(1) = r2
            crk(3) = -un
        else if (face .eq. 2) then
            crk(3) = r1
            crk(2) = r2
            crk(1) = -un
        else if (face .eq. 3) then
            crk(1) = r1
            crk(3) = r2
            crk(2) = -un
        else if (face .eq. 4) then
            crk(1) = r1
            crk(2) = r2
            crk(3) = un
        else if (face .eq. 5) then
            crk(2) = r1
            crk(3) = r2
            crk(1) = un
        else if (face .eq. 6) then
            crk(1) = -r1
            crk(3) = r2
            crk(2) = un
        else
            call u2mesk('F', 'INTEMAIL_6', 1, 'HEXA')
        endif
    else
        call u2mess('F', 'INTEMAIL_7')
    endif
end subroutine
