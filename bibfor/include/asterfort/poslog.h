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
    subroutine poslog(resi, rigi, tn, tp, fm,&
                      lgpg, vip, ndim, fp, g,&
                      dtde, sigm, cplan, fami, mate,&
                      instp, angmas, gn, lamb, logl,&
                      sigp, dsidep, pk2m, pk2, codret)
        integer :: ndim
        integer :: lgpg
        logical(kind=1) :: resi
        logical(kind=1) :: rigi
        real(kind=8) :: tn(6)
        real(kind=8) :: tp(6)
        real(kind=8) :: fm(3, 3)
        real(kind=8) :: vip(lgpg)
        real(kind=8) :: fp(3, 3)
        integer :: g
        real(kind=8) :: dtde(6, 6)
        real(kind=8) :: sigm(2*ndim)
        logical(kind=1) :: cplan
        character(len=*) :: fami
        integer :: mate
        real(kind=8) :: instp
        real(kind=8) :: angmas(*)
        real(kind=8) :: gn(3, 3)
        real(kind=8) :: lamb(3)
        real(kind=8) :: logl(3)
        real(kind=8) :: sigp(2*ndim)
        real(kind=8) :: dsidep(6, 6)
        real(kind=8) :: pk2m(6)
        real(kind=8) :: pk2(6)
        integer :: codret
    end subroutine poslog
end interface
