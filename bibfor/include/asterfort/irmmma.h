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
    subroutine irmmma(fid, nomamd, nbmail, connex, point,&
                      typma, nommai, prefix, nbtyp, typgeo,&
                      nomtyp, nnotyp, renumd, nmatyp, infmed,&
                      modnum, nuanom)
        integer, parameter :: ntymax=69
        integer :: fid
        character(len=*) :: nomamd
        integer :: nbmail
        integer :: connex(*)
        integer :: point(*)
        integer :: typma(*)
        character(len=8) :: nommai(*)
        character(len=6) :: prefix
        integer :: nbtyp
        integer :: typgeo(*)
        character(len=8) :: nomtyp(*)
        integer :: nnotyp(*)
        integer :: renumd(*)
        integer :: nmatyp(*)
        integer :: infmed
        integer :: modnum(ntymax)
        integer :: nuanom(ntymax, *)
    end subroutine irmmma
end interface
