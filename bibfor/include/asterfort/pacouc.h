!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine pacouc(typflu, vecr1, vecr2, vite, vecr3,&
                      masg, freq, amor, nbno, indic,&
                      nbpv, w, veci1, vecr4, vecr5,&
                      ier)
        character(len=8) :: typflu
        real(kind=8) :: vecr1(*)
        real(kind=8) :: vecr2(*)
        real(kind=8) :: vite(*)
        real(kind=8) :: vecr3(*)
        real(kind=8) :: masg(*)
        real(kind=8) :: freq(*)
        real(kind=8) :: amor(*)
        integer :: nbno
        integer :: indic
        integer :: nbpv
        real(kind=8) :: w(*)
        integer :: veci1(*)
        real(kind=8) :: vecr4(*)
        real(kind=8) :: vecr5(*)
        integer :: ier
    end subroutine pacouc
end interface
