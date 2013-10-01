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
    subroutine mdarch(isto1, ipas, disc, dt, nbmode,&
                      typcal, nbsym, nomsym, depger, vitger,&
                      accger, depstr, vitstr, accstr, depgec,&
                      vitgec, accgec, depstc, vitstc, accstc,&
                      passto, iorsto, discst)
        integer :: isto1
        integer :: ipas
        real(kind=8) :: disc
        real(kind=8) :: dt
        integer :: nbmode
        character(len=4) :: typcal
        integer :: nbsym
        character(len=4) :: nomsym(*)
        real(kind=8) :: depger(*)
        real(kind=8) :: vitger(*)
        real(kind=8) :: accger(*)
        real(kind=8) :: depstr(*)
        real(kind=8) :: vitstr(*)
        real(kind=8) :: accstr(*)
        complex(kind=8) :: depgec(*)
        complex(kind=8) :: vitgec(*)
        complex(kind=8) :: accgec(*)
        complex(kind=8) :: depstc(*)
        complex(kind=8) :: vitstc(*)
        complex(kind=8) :: accstc(*)
        real(kind=8) :: passto(*)
        integer :: iorsto(*)
        real(kind=8) :: discst(*)
    end subroutine mdarch
end interface
