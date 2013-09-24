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
    subroutine lrmmfa(fid, nomamd, nbnoeu, nbmail, grpnoe,&
                      gpptnn, grpmai, gpptnm, nbgrno, nbgrma,&
                      typgeo, nomtyp, nmatyp, prefix, infmed,&
                      vecgrm, nbcgrm)
        integer :: fid
        character(len=*) :: nomamd
        integer :: nbnoeu
        integer :: nbmail
        character(len=24) :: grpnoe
        character(len=24) :: gpptnn
        character(len=24) :: grpmai
        character(len=24) :: gpptnm
        integer :: nbgrno
        integer :: nbgrma
        integer :: typgeo(69)
        character(len=8) :: nomtyp(69)
        integer :: nmatyp(69)
        character(len=6) :: prefix
        integer :: infmed
        character(len=24) :: vecgrm
        integer :: nbcgrm
    end subroutine lrmmfa
end interface
