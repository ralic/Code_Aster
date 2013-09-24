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
    subroutine rvcpnc(mcf, iocc, nch19, gd, typegd,&
                      nbcpc, nlscpc, nomojb, repere, option,&
                      quant, codir, dir, iret)
        character(len=*) :: mcf
        integer :: iocc
        character(len=19) :: nch19
        integer :: gd
        character(len=4) :: typegd
        integer :: nbcpc
        character(len=24) :: nlscpc
        character(len=24) :: nomojb
        character(len=8) :: repere
        character(len=16) :: option
        character(len=24) :: quant
        integer :: codir
        real(kind=8) :: dir(*)
        integer :: iret
    end subroutine rvcpnc
end interface
