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
    subroutine calcpj(nbmat, mater, gamp, evp, sigd,&
                      sige, epssig, invare, gamps, evps,&
                      invars, b)
        integer :: nbmat
        real(kind=8) :: mater(nbmat, 2)
        real(kind=8) :: gamp
        real(kind=8) :: evp
        real(kind=8) :: sigd(6)
        real(kind=8) :: sige(6)
        real(kind=8) :: epssig
        real(kind=8) :: invare
        real(kind=8) :: gamps
        real(kind=8) :: evps
        real(kind=8) :: invars
        real(kind=8) :: b
    end subroutine calcpj
end interface
