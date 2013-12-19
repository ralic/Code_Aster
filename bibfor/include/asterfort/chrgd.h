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
    subroutine chrgd(nbcmp, jcesd, jcesl, jcesv, imai,&
                     ipt, isp, type_gd, rc, p,&
                     permvec)
!
        integer, intent(in) :: nbcmp, jcesd, jcesl, jcesv, imai, ipt
        integer, intent(in) :: isp
        character(len=*), intent(in) :: type_gd
        character, intent(in) :: rc
        real(kind=8), dimension(:, :), intent(in) :: p
        integer, dimension(:), intent(in), optional :: permvec
!
    end subroutine 
end interface 
