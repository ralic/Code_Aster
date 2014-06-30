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
    subroutine xmrema(jcesd, jcesv, jcesl, noma, ndim,&
                      ifise, defico, izone, alias, mmait,&
                      amait, nmait, statue, geom, nummin,&
                      nummae, ifamin, ifacee, jeumin, t1min,&
                      t2min, ximin, yimin, projin, stamin,&
                      ifism)
        integer :: jcesd(10)
        integer :: jcesv(10)
        integer :: jcesl(10)
        character(len=8) :: noma
        integer :: ndim
        integer :: ifise
        character(len=24) :: defico
        integer :: izone
        character(len=8) :: alias
        integer :: mmait
        integer :: amait
        integer :: nmait
        integer :: statue
        real(kind=8) :: geom(3)
        integer :: nummin
        integer :: nummae
        integer :: ifamin
        integer :: ifacee
        real(kind=8) :: jeumin
        real(kind=8) :: t1min(3)
        real(kind=8) :: t2min(3)
        real(kind=8) :: ximin
        real(kind=8) :: yimin
        logical(kind=1) :: projin
        integer :: stamin
        integer :: ifism
    end subroutine xmrema
end interface
