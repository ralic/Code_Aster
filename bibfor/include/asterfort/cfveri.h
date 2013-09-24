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
    subroutine cfveri(noma, defico, resoco, newgeo, sdappa,&
                      npt, jeux, loca, enti, zone,instan)
        character(len=8), intent(in) :: noma
        character(len=24), intent(in) :: defico
        character(len=24), intent(in) :: resoco
        character(len=19), intent(in) :: newgeo
        character(len=19), intent(in) :: sdappa
        character(len=24), intent(in) :: jeux
        character(len=24), intent(in) :: loca
        character(len=24), intent(in) :: enti
        character(len=24), intent(in) :: zone
        integer, intent(in) :: npt
        real(kind=8), intent(in) :: instan
    end subroutine cfveri
end interface
