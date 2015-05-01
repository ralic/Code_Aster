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
    subroutine vprecu(modes, nomsy, nbvect, lposi, nomvec,&
                      nbpara, nopara, nomvai, nomvar, nomvak,&
                      neq, nbmode, typmod, nbpari, nbparr,&
                      nbpark)
        character(len=*), intent(in) :: modes
        character(len=*), intent(in) :: nomsy
        integer, intent(in) :: nbvect
        integer, intent(in) :: lposi(*)
        character(len=*), intent(in) :: nomvec
        integer, intent(in) :: nbpara
        character(len=*), intent(in) :: nopara
        character(len=*), intent(in) :: nomvai
        character(len=*), intent(in) :: nomvar
        character(len=*), intent(in) :: nomvak
        integer, intent(out) :: neq
        integer, intent(out) :: nbmode
        character(len=*), intent(out) :: typmod
        integer, intent(out) :: nbpari
        integer, intent(out) :: nbparr
        integer, intent(out) :: nbpark
    end subroutine vprecu
end interface
