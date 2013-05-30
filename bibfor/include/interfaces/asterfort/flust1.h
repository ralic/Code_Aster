!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
    subroutine flust1(melflu, typflu, base, nuor, amor,&
                      amoc, freq, masg, fact, vite,&
                      nbm, calcul, npv, nivpar, nivdef)
        character(len=19) :: melflu
        character(len=8) :: typflu
        character(len=8) :: base
        integer :: nuor(*)
        real(kind=8) :: amor(*)
        real(kind=8) :: amoc(*)
        real(kind=8) :: freq(*)
        real(kind=8) :: masg(*)
        real(kind=8) :: fact(*)
        real(kind=8) :: vite(*)
        integer :: nbm
        logical :: calcul(2)
        integer :: npv
        integer :: nivpar
        integer :: nivdef
    end subroutine flust1
end interface
