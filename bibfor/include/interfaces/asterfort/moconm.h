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
    subroutine moconm(dir, sigb, siga, hh, nlit,&
                      om, rr, nufsup, nufinf, nufsd1,&
                      nufid1, nufsd2, nufid2, prec)
        integer :: nlit
        character(len=1) :: dir
        real(kind=8) :: sigb
        real(kind=8) :: siga(nlit)
        real(kind=8) :: hh
        real(kind=8) :: om(nlit)
        real(kind=8) :: rr(nlit)
        character(len=8) :: nufsup
        character(len=8) :: nufinf
        character(len=8) :: nufsd1
        character(len=8) :: nufid1
        character(len=8) :: nufsd2
        character(len=8) :: nufid2
        real(kind=8) :: prec
    end subroutine moconm
end interface
