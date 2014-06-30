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
    subroutine prelog(ndim, lgpg, vim, gn, lamb,&
                      logl, fm, fp, epsml, deps,&
                      tn, resi, iret)
        integer :: lgpg
        integer :: ndim
        real(kind=8) :: vim(lgpg)
        real(kind=8) :: gn(3, 3)
        real(kind=8) :: lamb(3)
        real(kind=8) :: logl(3)
        real(kind=8) :: fm(3, 3)
        real(kind=8) :: fp(3, 3)
        real(kind=8) :: epsml(6)
        real(kind=8) :: deps(6)
        real(kind=8) :: tn(6)
        logical(kind=1) :: resi
        integer :: iret
    end subroutine prelog
end interface
