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
    subroutine rc3601(ig, iocs, seisme, npass, ima,&
                      ipt, nbm, adrm, c, k,&
                      cara, nommat, snmax, samax, utot,&
                      sm, factus)
        integer :: ig
        integer :: iocs
        logical(kind=1) :: seisme
        integer :: npass
        integer :: ima
        integer :: ipt
        integer :: nbm
        integer :: adrm(*)
        real(kind=8) :: c(*)
        real(kind=8) :: k(*)
        real(kind=8) :: cara(*)
        character(len=8) :: nommat
        real(kind=8) :: snmax
        real(kind=8) :: samax
        real(kind=8) :: utot
        real(kind=8) :: sm
        real(kind=8) :: factus(*)
    end subroutine rc3601
end interface
