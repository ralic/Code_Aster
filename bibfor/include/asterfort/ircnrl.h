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
    subroutine ircnrl(ifi, nbno, prno, nueq, nec,&
                      dg, ncmpmx, vale, nomcmp, nomnoe,&
                      lcor, ndim, coor, numnoe, nbcmpt,&
                      nucmpu, lsup, borsup, linf, borinf,&
                      lmax, lmin, formr)
        integer :: ifi
        integer :: nbno
        integer :: prno(*)
        integer :: nueq(*)
        integer :: nec
        integer :: dg(*)
        integer :: ncmpmx
        real(kind=8) :: vale(*)
        character(len=*) :: nomcmp(*)
        character(len=*) :: nomnoe(*)
        logical(kind=1) :: lcor
        integer :: ndim
        real(kind=8) :: coor(*)
        integer :: numnoe(*)
        integer :: nbcmpt
        integer :: nucmpu(*)
        logical(kind=1) :: lsup
        real(kind=8) :: borsup
        logical(kind=1) :: linf
        real(kind=8) :: borinf
        logical(kind=1) :: lmax
        logical(kind=1) :: lmin
        character(len=*) :: formr
    end subroutine ircnrl
end interface
