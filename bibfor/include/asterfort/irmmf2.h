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
    subroutine irmmf2(fid, nomamd, typent, nbrent, nbgrou,&
                      nomgen, nbec, nomast, prefix, typgeo,&
                      nomtyp, nmatyp, nufaen, nufacr, nogrfa,&
                      nofaex, tabaux, infmed, nivinf, ifm)
        integer :: nbgrou
        integer :: nbrent
        integer :: fid
        character(*) :: nomamd
        integer :: typent
        character(len=24) :: nomgen(*)
        integer :: nbec
        character(len=8) :: nomast
        character(len=6) :: prefix
        integer :: typgeo(*)
        character(len=8) :: nomtyp(*)
        integer :: nmatyp(*)
        integer :: nufaen(nbrent)
        integer :: nufacr(nbrent)
        character(len=80) :: nogrfa(nbgrou)
        character(*) :: nofaex(*)
        integer :: tabaux(*)
        integer :: infmed
        integer :: nivinf
        integer :: ifm
    end subroutine irmmf2
end interface
