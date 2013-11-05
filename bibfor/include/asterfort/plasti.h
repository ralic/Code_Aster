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
    subroutine plasti(fami, kpg, ksp, typmod, imat,&
                      comp, crit, timed, timef, tempd,&
                      tempf, tref, epsdt, depst, sigd,&
                      vind, opt, angmas, sigf, vinf,&
                      dsde, icomp, nvi, tampon, irteti)
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=8) :: typmod(*)
        integer :: imat
        character(len=16) :: comp(*)
        real(kind=8) :: crit(*)
        real(kind=8) :: timed
        real(kind=8) :: timef
        real(kind=8) :: tempd
        real(kind=8) :: tempf
        real(kind=8) :: tref
        real(kind=8) :: epsdt(9)
        real(kind=8) :: depst(9)
        real(kind=8) :: sigd(6)
        real(kind=8) :: vind(*)
        character(len=16) :: opt
        real(kind=8) :: angmas(3)
        real(kind=8) :: sigf(6)
        real(kind=8) :: vinf(*)
        real(kind=8) :: dsde(6, *)
        integer :: icomp
        integer :: nvi
        real(kind=8) :: tampon(*)
        integer :: irteti
    end subroutine plasti
end interface
