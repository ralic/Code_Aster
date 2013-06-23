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
    subroutine ircecl(ifi, nbel, ligrel, nbgrel, longr,&
                      ncmpmx, vale, nomcmp, nomel, loc,&
                      celd, connex, point, nomnos, nbcmpt,&
                      nucmpu, nbnot, numnoe, nbmat, nummai,&
                      lsup, borsup, linf, borinf, lmax,&
                      lmin, lcor, ndim, coor, nolili,&
                      formr, ncmpv, nucmp)
        integer :: ifi
        integer :: nbel
        integer :: ligrel(*)
        integer :: nbgrel
        integer :: longr(*)
        integer :: ncmpmx
        complex(kind=8) :: vale(*)
        character(*) :: nomcmp(*)
        character(*) :: nomel(*)
        character(*) :: loc
        integer :: celd(*)
        integer :: connex(*)
        integer :: point(*)
        character(*) :: nomnos(*)
        integer :: nbcmpt
        integer :: nucmpu(*)
        integer :: nbnot
        integer :: numnoe(*)
        integer :: nbmat
        integer :: nummai(*)
        logical :: lsup
        real(kind=8) :: borsup
        logical :: linf
        real(kind=8) :: borinf
        logical :: lmax
        logical :: lmin
        logical :: lcor
        integer :: ndim
        real(kind=8) :: coor(*)
        character(len=19) :: nolili
        character(*) :: formr
        integer :: ncmpv
        integer :: nucmp(*)
    end subroutine ircecl
end interface
