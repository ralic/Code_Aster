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
    subroutine utites(label1, label2, type, nbref, refi,&
                      refr, refc, vali, valr, valc,&
                      epsi, crit, ific, llab, ssigne,&
                      ignore, compare)
        integer, intent(in) :: nbref
        character(len=*), intent(in) :: label1
        character(len=*), intent(in) :: label2
        character(len=*), intent(in) :: type
        integer, intent(in) :: refi(nbref)
        real(kind=8), intent(in) :: refr(nbref)
        complex(kind=8), intent(in) :: refc(nbref)
        integer, intent(in) :: vali
        real(kind=8), intent(in) :: valr
        complex(kind=8), intent(in) :: valc
        real(kind=8), intent(in) :: epsi
        character(len=*), intent(in) :: crit
        integer, intent(in) :: ific
        logical, intent(in) :: llab
        character(len=*), intent(in) :: ssigne
        logical, intent(in), optional :: ignore
        real(kind=8), intent(in), optional :: compare
    end subroutine utites
end interface
