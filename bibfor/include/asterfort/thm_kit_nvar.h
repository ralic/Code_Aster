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
    subroutine thm_kit_nvar(rela_thmc, rela_hydr, rela_meca, rela_ther, nb_vari_thmc, &
                            nb_vari_hydr, nb_vari_meca, nb_vari_ther)
        character(len=16), intent(in) :: rela_thmc
        character(len=16), intent(in) :: rela_hydr
        character(len=16), intent(in) :: rela_meca
        character(len=16), intent(in) :: rela_ther
        integer, intent(out) :: nb_vari_thmc
        integer, intent(out) :: nb_vari_hydr
        integer, intent(out) :: nb_vari_meca
        integer, intent(out) :: nb_vari_ther
    end subroutine thm_kit_nvar
end interface
