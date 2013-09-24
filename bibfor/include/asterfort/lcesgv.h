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
    subroutine lcesgv(fami, kpg, ksp, neps, typmod, option, mat, lccrma, lcesga, epsm,&
                      deps, vim, fige, itemax, precvg,&
                      sig, vip, dsidep, iret)
        interface
        subroutine lccrma(mat, fami, kpg, ksp, poum)
            integer,intent(in) :: mat, kpg, ksp
            character(len=1),intent(in):: poum
            character(len=*),intent(in) :: fami
        end subroutine lccrma

        subroutine lcesga(mode, eps, gameps, dgamde, itemax, precvg, iret)
            integer,intent(in) :: mode, itemax
            real(kind=8),intent(in) :: eps(6), precvg
            integer,intent(out):: iret
            real(kind=8),intent(out):: gameps, dgamde(6)
        end subroutine lcesga
        end interface

        logical :: fige
        character(len=8) :: typmod(*)
        character(len=16) :: option
        character(len=*) :: fami
        integer :: neps, mat, iret, kpg, ksp, itemax
        real(kind=8) :: epsm(neps), deps(neps), vim(*), precvg
        real(kind=8) :: vip(*), sig(neps), dsidep(neps, neps)
    end subroutine lcesgv
end interface 
