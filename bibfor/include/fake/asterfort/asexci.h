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
    subroutine asexci(masse, parmod, amort, nbmode, corfre,&
                      impr, ndir, monoap, muapde, kspect,&
                      kasysp, nbsup, nsupp, knoeu)
        integer :: nbmode
        character(*) :: masse
        real(kind=8) :: parmod(nbmode, *)
        real(kind=8) :: amort(*)
        logical :: corfre
        integer :: impr
        integer :: ndir(*)
        logical :: monoap
        logical :: muapde
        character(*) :: kspect
        character(*) :: kasysp
        integer :: nbsup
        integer :: nsupp(*)
        character(*) :: knoeu
    end subroutine asexci
end interface
