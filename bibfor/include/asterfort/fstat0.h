!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine fstat0(nbpt, fn, offset, fnmoyt, fnmoyc,&
                      fnrmst, fnrmsc, fnmax, fnmin, fmaxmo,&
                      fminmo, nbmaxr, nbminr)
        integer :: nbpt
        real(kind=8) :: fn(*)
        real(kind=8) :: offset
        real(kind=8) :: fnmoyt
        real(kind=8) :: fnmoyc
        real(kind=8) :: fnrmst
        real(kind=8) :: fnrmsc
        real(kind=8) :: fnmax
        real(kind=8) :: fnmin
        real(kind=8) :: fmaxmo
        real(kind=8) :: fminmo
        integer :: nbmaxr
        integer :: nbminr
    end subroutine fstat0
end interface
