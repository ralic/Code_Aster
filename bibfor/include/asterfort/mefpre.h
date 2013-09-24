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
    subroutine mefpre(ndim, alpha, z, cf, dh,&
                      vit, rho, pstat, dpstat, dvit,&
                      itypg, zg, hg, axg, pm,&
                      xig, afluid, cdg, cfg, vitg,&
                      rhog)
        integer :: ndim(14)
        real(kind=8) :: alpha
        real(kind=8) :: z(*)
        real(kind=8) :: cf(*)
        real(kind=8) :: dh
        real(kind=8) :: vit(*)
        real(kind=8) :: rho(*)
        real(kind=8) :: pstat(*)
        real(kind=8) :: dpstat(*)
        real(kind=8) :: dvit(*)
        integer :: itypg(*)
        real(kind=8) :: zg(*)
        real(kind=8) :: hg(*)
        real(kind=8) :: axg(*)
        real(kind=8) :: pm
        real(kind=8) :: xig(*)
        real(kind=8) :: afluid
        real(kind=8) :: cdg(*)
        real(kind=8) :: cfg(*)
        real(kind=8) :: vitg(*)
        real(kind=8) :: rhog(*)
    end subroutine mefpre
end interface
