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
    subroutine i2imac(epsi, conec, coord, typ, nbm,&
                      numail, xc, yc, r, alfinf,&
                      alfsup, nbseg, sgtor, sgtex, mail1,&
                      mail2, facor, facex, paror, parex)
        real(kind=8) :: epsi
        character(len=24) :: conec
        character(len=24) :: coord
        character(len=24) :: typ
        integer :: nbm
        integer :: numail(*)
        real(kind=8) :: xc
        real(kind=8) :: yc
        real(kind=8) :: r
        real(kind=8) :: alfinf
        real(kind=8) :: alfsup
        integer :: nbseg
        real(kind=8) :: sgtor(*)
        real(kind=8) :: sgtex(*)
        integer :: mail1(*)
        integer :: mail2(*)
        integer :: facor(*)
        integer :: facex(*)
        real(kind=8) :: paror(*)
        real(kind=8) :: parex(*)
    end subroutine i2imac
end interface
