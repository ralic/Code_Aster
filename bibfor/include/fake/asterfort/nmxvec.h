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
    subroutine nmxvec(modelz, mate, carele, compor, carcri,&
                      sdtime, sddisc, sddyna, numins, valinc,&
                      solalg, lischa, comref, resoco, resocu,&
                      numedd, parcon, veelem, veasse, measse,&
                      nbvect, ltypve, lcalve, loptve, lassve)
        character(*) :: modelz
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: compor
        character(len=24) :: carcri
        character(len=24) :: sdtime
        character(len=19) :: sddisc
        character(len=19) :: sddyna
        integer :: numins
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: lischa
        character(len=24) :: comref
        character(len=24) :: resoco
        character(len=24) :: resocu
        character(len=24) :: numedd
        real(kind=8) :: parcon(*)
        character(len=19) :: veelem(*)
        character(len=19) :: veasse(*)
        character(len=19) :: measse(*)
        integer :: nbvect
        character(len=6) :: ltypve(20)
        logical :: lcalve(20)
        character(len=16) :: loptve(20)
        logical :: lassve(20)
    end subroutine nmxvec
end interface
