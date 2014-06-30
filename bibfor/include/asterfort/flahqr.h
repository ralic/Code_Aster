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
    subroutine flahqr(wantt, wantz, n, ilo, ihi,&
                      h, ldh, wr, wi, iloz,&
                      ihiz, z, ldz, info)
        integer :: ldz
        integer :: ldh
        logical(kind=1) :: wantt
        logical(kind=1) :: wantz
        integer :: n
        integer :: ilo
        integer :: ihi
        real(kind=8) :: h(ldh, *)
        real(kind=8) :: wr(*)
        real(kind=8) :: wi(*)
        integer :: iloz
        integer :: ihiz
        real(kind=8) :: z(ldz, *)
        integer :: info
    end subroutine flahqr
end interface
