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
    subroutine ftrsen(job, compq, select, n, t,&
                      ldt, q, ldq, wr, wi,&
                      m, s, sep, work, lwork,&
                      iwork, liwork, info)
        integer :: ldq
        integer :: ldt
        character(len=1) :: job
        character(len=1) :: compq
        logical(kind=1) :: select(*)
        integer :: n
        real(kind=8) :: t(ldt, *)
        real(kind=8) :: q(ldq, *)
        real(kind=8) :: wr(*)
        real(kind=8) :: wi(*)
        integer :: m
        real(kind=8) :: s
        real(kind=8) :: sep
        real(kind=8) :: work(*)
        integer :: lwork
        integer :: iwork(*)
        integer :: liwork
        integer :: info
    end subroutine ftrsen
end interface
