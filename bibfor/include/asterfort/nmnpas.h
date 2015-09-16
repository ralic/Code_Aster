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
! aslint: disable=W1504
!
interface
    subroutine nmnpas(modele  , noma  , mate  , carele, fonact ,&
                      ds_print, sddisc, sdsuiv, sddyna, sdnume ,&
                      sdstat  , sdtime, numedd, numins, defico ,&
                      resoco  , valinc, solalg, solveu, ds_conv,&
                      lischa)
        use NonLin_Datastructure_type
        character(len=24) :: modele
        character(len=8) :: noma
        character(len=24) :: mate
        character(len=24) :: carele
        integer :: fonact(*)
        type(NL_DS_Print), intent(inout) :: ds_print
        character(len=19) :: sddisc
        character(len=24) :: sdsuiv
        character(len=19) :: sddyna
        character(len=19) :: sdnume
        character(len=24) :: sdstat
        character(len=24) :: sdtime
        character(len=24) :: numedd
        integer :: numins
        character(len=24) :: defico
        character(len=24) :: resoco
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: solveu
        type(NL_DS_Conv), intent(inout) :: ds_conv
        character(len=19), intent(in) :: lischa
    end subroutine nmnpas
end interface
