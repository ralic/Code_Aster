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
    subroutine rcvalb(fami, kpg, ksp, poum, jmat,&
                      nomat, phenom, nbpar, nompar, valpar,&
                      nbres, nomres, valres, codret, iarret)
        integer :: nbres
        integer :: nbpar
        character(*) :: fami
        integer :: kpg
        integer :: ksp
        character(*) :: poum
        integer :: jmat
        character(*) :: nomat
        character(*) :: phenom
        character(*) :: nompar(nbpar)
        real(kind=8) :: valpar(nbpar)
        character(*) :: nomres(nbres)
        real(kind=8) :: valres(nbres)
        integer :: codret(nbres)
        integer :: iarret
    end subroutine rcvalb
end interface
