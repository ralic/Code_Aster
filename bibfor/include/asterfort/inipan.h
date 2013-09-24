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
    subroutine inipan(np1, nbm, cmod0, kmod0, cmod,&
                      kmod, amor0, puls0, amor, puls,&
                      fnlmod, fexmod, fmod00)
        integer :: np1
        integer :: nbm
        real(kind=8) :: cmod0(np1, *)
        real(kind=8) :: kmod0(np1, *)
        real(kind=8) :: cmod(np1, *)
        real(kind=8) :: kmod(np1, *)
        real(kind=8) :: amor0(*)
        real(kind=8) :: puls0(*)
        real(kind=8) :: amor(*)
        real(kind=8) :: puls(*)
        real(kind=8) :: fnlmod(*)
        real(kind=8) :: fexmod(*)
        real(kind=8) :: fmod00(*)
    end subroutine inipan
end interface
