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
    subroutine calirg(noma, nbno, list_node, tran,  cent, &
                      l_angl_naut, angl_naut, geom2, l_rota, matr_rota)
        character(len=8), intent(in) :: noma
        integer, intent(in) :: nbno
        character(len=24), intent(in) :: list_node
        logical, intent(in) :: l_angl_naut
        real(kind=8), intent(in) :: angl_naut(3)
        real(kind=8), intent(in) :: cent(3)
        real(kind=8), intent(in) :: tran(3)
        character(len=*) :: geom2
        logical, intent(out) :: l_rota
        real(kind=8), intent(out) :: matr_rota(3, 3)
    end subroutine calirg
end interface
