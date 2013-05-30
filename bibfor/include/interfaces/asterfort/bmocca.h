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
    subroutine bmocca(umoy, geom, cf0, mcf0, fsvr,&
                      nbm, vicoq, torco, tcoef, s1,&
                      s2, b)
        integer :: nbm
        real(kind=8) :: umoy
        real(kind=8) :: geom(9)
        real(kind=8) :: cf0
        real(kind=8) :: mcf0
        real(kind=8) :: fsvr(7)
        integer :: vicoq(nbm)
        real(kind=8) :: torco(4, nbm)
        real(kind=8) :: tcoef(10, nbm)
        real(kind=8) :: s1
        real(kind=8) :: s2
        complex(kind=8) :: b(nbm, nbm)
    end subroutine bmocca
end interface
