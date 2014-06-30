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
    subroutine piesgv(neps, tau, mat, lccrma, vim,&
                      epsm, epsp, epsd, typmod, lcesga,&
                      etamin, etamax, lcesbo, copilo)
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

        subroutine lcesbo(ep0, ep1, l0, l1, etamin, etamax, vide, etam, etap)
        real(kind=8),intent(in) :: ep0(6),ep1(6),l0,l1,etamin,etamax   
        logical(kind=1), intent(out)    :: vide     
        real(kind=8),intent(out):: etam,etap     
        end subroutine lcesbo
        end interface

        character(len=8),intent(in) :: typmod(*)
        integer,intent(in)      :: neps, mat
        real(kind=8),intent(in) :: tau, epsm(neps), epsd(neps), epsp(neps), etamin, etamax,vim(3)
        real(kind=8),intent(out):: copilo(2,*)
    end subroutine piesgv
end interface 
