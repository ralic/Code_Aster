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
    subroutine coupla(np1, nbm, indic, tpfl, veci1,&
                      vgap, vecr4, vecr1, vecr2, vecr5,&
                      vecr3, masg, puls, locflc, amflu0,&
                      amfluc, xsi0)
        integer :: np1
        integer :: nbm
        integer :: indic
        character(len=8) :: tpfl
        integer :: veci1(*)
        real(kind=8) :: vgap
        real(kind=8) :: vecr4(*)
        real(kind=8) :: vecr1(*)
        real(kind=8) :: vecr2(*)
        real(kind=8) :: vecr5(*)
        real(kind=8) :: vecr3(*)
        real(kind=8) :: masg(*)
        real(kind=8) :: puls(*)
        logical(kind=1) :: locflc(*)
        real(kind=8) :: amflu0(np1, *)
        real(kind=8) :: amfluc(np1, *)
        real(kind=8) :: xsi0(*)
    end subroutine coupla
end interface
