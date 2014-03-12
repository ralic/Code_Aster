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
    subroutine elg_resoud(matas1, matpre, chcine, nsecm, chsecm, chsolu,&
                          base, rsolu, csolu, criter, prepos,&
                          istop, iret)
    character(len=19), intent(in) :: matas1
    character(len=*), intent(in) :: matpre
    character(len=*), intent(in) :: chcine
    integer, intent(in) :: nsecm
    character(len=*), intent(in) :: chsecm
    character(len=*), intent(in) :: chsolu
    character(len=*), intent(in) :: base
    real(kind=8), intent(inout) :: rsolu(*)
    complex(kind=8), intent(inout) :: csolu(*)
    character(len=*), intent(in) :: criter
    logical, intent(in) :: prepos
    integer, intent(in) :: istop
    integer, intent(out) :: iret
    end subroutine elg_resoud
end interface
