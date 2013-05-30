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
    subroutine mefmat(ndim, numgrp, nbz, nbgrp, nbmod,&
                      matma, dcent, cp, cf, vit,&
                      rho, pstat, dpstat, rint, phix,&
                      phiy, z, matm, matr, mata,&
                      itypg, axg, zg, rhog, vitg,&
                      cdg, cpg)
        integer :: nbmod
        integer :: nbgrp
        integer :: nbz
        integer :: ndim(14)
        integer :: numgrp(*)
        real(kind=8) :: matma(*)
        real(kind=8) :: dcent(*)
        real(kind=8) :: cp(*)
        real(kind=8) :: cf(*)
        real(kind=8) :: vit(0:*)
        real(kind=8) :: rho(0:*)
        real(kind=8) :: pstat(*)
        real(kind=8) :: dpstat(*)
        real(kind=8) :: rint(*)
        real(kind=8) :: phix(nbz*nbgrp, nbmod)
        real(kind=8) :: phiy(nbz*nbgrp, nbmod)
        real(kind=8) :: z(*)
        real(kind=8) :: matm(nbmod, nbmod)
        real(kind=8) :: matr(nbmod, nbmod)
        real(kind=8) :: mata(nbmod, nbmod)
        integer :: itypg(*)
        real(kind=8) :: axg(*)
        real(kind=8) :: zg(*)
        real(kind=8) :: rhog(*)
        real(kind=8) :: vitg(*)
        real(kind=8) :: cdg(*)
        real(kind=8) :: cpg(*)
    end subroutine mefmat
end interface
