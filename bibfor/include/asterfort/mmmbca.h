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
    subroutine mmmbca(noma  , sddyna, iterat, defico, resoco, &
                      sdstat, valinc, solalg, ctcsta, mmcvca,&
                      instan)
        character(len=8), intent(in) :: noma
        character(len=19), intent(in) :: sddyna
        integer, intent(in) :: iterat
        character(len=24), intent(in) :: defico
        character(len=24), intent(in) :: resoco
        character(len=24), intent(in) :: sdstat
        character(len=19), intent(in) :: valinc(*)
        character(len=19), intent(in) :: solalg(*)
        integer, intent(out) :: ctcsta
        logical(kind=1), intent(out) :: mmcvca
        real(kind=8), intent(in) :: instan
    end subroutine mmmbca
end interface
