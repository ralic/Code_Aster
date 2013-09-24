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
    subroutine vafcar(tpgz, mclfz, nmobjz, npo, ndi,&
                      nco, nca, nba, nma, ngb,&
                      nmb, nutyel, ntyele, car, ncar,&
                      ivr, kioc, ier)
        character(len=*) :: tpgz
        character(len=*) :: mclfz
        character(len=*) :: nmobjz
        integer :: npo
        integer :: ndi
        integer :: nco
        integer :: nca
        integer :: nba
        integer :: nma
        integer :: ngb
        integer :: nmb
        integer :: nutyel
        integer :: ntyele(*)
        character(len=*) :: car(*)
        integer :: ncar
        integer :: ivr(*)
        character(len=6) :: kioc
        integer :: ier
    end subroutine vafcar
end interface
