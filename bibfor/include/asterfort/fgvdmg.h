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
    subroutine fgvdmg(nomsym, nomsd, nommat, nomnap, nomfon,&
                      mexpic, mcompt, mdomag, nbord, nbpt,&
                      ntcmp, nbcmp, numcmp, impr, vdomag)
        character(len=16) :: nomsym
        character(len=19) :: nomsd
        character(len=8) :: nommat
        character(len=8) :: nomnap
        character(len=8) :: nomfon
        character(len=*) :: mexpic
        character(len=*) :: mcompt
        character(len=*) :: mdomag
        integer :: nbord
        integer :: nbpt
        integer :: ntcmp
        integer :: nbcmp
        integer :: numcmp(*)
        integer :: impr
        real(kind=8) :: vdomag(*)
    end subroutine fgvdmg
end interface
