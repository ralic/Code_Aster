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
    subroutine utmess(typ, idmess, nk, valk, sk, &
                      ni, vali, si, nr, valr, &
                      sr, num_except, fname)
        character(len=*), intent(in) :: typ
        character(len=*), intent(in) :: idmess
        integer, intent(in), optional :: nk
        character(len=*), intent(in), optional, target :: valk(*)
        character(len=*), intent(in), optional :: sk
        integer, intent(in), optional :: ni
        integer, intent(in), optional, target :: vali(*)
        integer, intent(in), optional :: si
        integer, intent(in), optional :: nr
        real(kind=8), intent(in), optional, target :: valr(*)
        real(kind=8), intent(in), optional :: sr
        integer, intent(in), optional :: num_except
        character(len=*), optional :: fname
    end subroutine utmess
end interface
