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
    subroutine irmmf1(fid, nomamd, typent, nbrent, nbgrou,&
                      nomgen, nufaen, nomast, prefix, typgeo,&
                      nomtyp, nmatyp, infmed, nivinf, ifm)
        integer :: nbrent
        integer :: fid
        character(len=*) :: nomamd
        integer :: typent
        integer :: nbgrou
        character(len=24) :: nomgen(*)
        integer :: nufaen(nbrent)
        character(len=8) :: nomast
        character(len=6) :: prefix
        integer :: typgeo(*)
        character(len=8) :: nomtyp(*)
        integer :: nmatyp(*)
        integer :: infmed
        integer :: nivinf
        integer :: ifm
    end subroutine irmmf1
end interface
