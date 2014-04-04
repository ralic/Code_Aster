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
subroutine noligr(noma,ligrz, igrel, numel, nb, li,&
                  lk, code, irepe, inema, nbno,&
                  typlaz,jlgns)
    character(len=8),intent(in) :: noma
    character(len=*),intent(in) :: ligrz
    integer,intent(in) :: igrel
    integer,intent(in) :: numel
    integer,intent(in) :: nb
    integer,intent(in) :: li(*)
    character(len=*),intent(in) :: lk(*)
    integer,intent(in) :: code
    integer,intent(in) :: irepe
    integer,intent(inout) :: inema
    integer,intent(inout) :: nbno(*)
    character(len=*),intent(in) :: typlaz
    integer,intent(in) :: jlgns
end subroutine noligr
end interface
