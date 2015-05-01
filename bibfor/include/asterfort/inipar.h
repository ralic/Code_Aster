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
    subroutine inipar(np1, nbm, nbnl, testc, cmod0,&
                      cmodca, kmod0, kmodca, amor, amor0,&
                      puls, puls0, acc, vit, dep,&
                      acc0, vit0, dep0, accg, vitg,&
                      depg, accg0, vitg0, depg0, tconf1,&
                      ftest0, tconf2, ftest)
        integer :: np1
        integer :: nbm
        integer :: nbnl
        integer :: testc
        real(kind=8) :: cmod0(np1, *)
        real(kind=8) :: cmodca(np1, *)
        real(kind=8) :: kmod0(np1, *)
        real(kind=8) :: kmodca(np1, *)
        real(kind=8) :: amor(*)
        real(kind=8) :: amor0(*)
        real(kind=8) :: puls(*)
        real(kind=8) :: puls0(*)
        real(kind=8) :: acc(3, *)
        real(kind=8) :: vit(3, *)
        real(kind=8) :: dep(3, *)
        real(kind=8) :: acc0(3, *)
        real(kind=8) :: vit0(3, *)
        real(kind=8) :: dep0(3, *)
        real(kind=8) :: accg(*)
        real(kind=8) :: vitg(*)
        real(kind=8) :: depg(*)
        real(kind=8) :: accg0(*)
        real(kind=8) :: vitg0(*)
        real(kind=8) :: depg0(*)
        real(kind=8) :: tconf1(4, *)
        real(kind=8) :: ftest0
        real(kind=8) :: tconf2(4, *)
        real(kind=8) :: ftest
    end subroutine inipar
end interface
