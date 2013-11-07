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
    subroutine jacobi(nbvec, nperm, tol, toldyn, ar,&
                      br, vecpro, valpro, valaux, nitjac,&
                      type, iordre)
        integer :: nbvec
        integer :: nperm
        real(kind=8) :: tol
        real(kind=8) :: toldyn
        real(kind=8) :: ar(nbvec)
        real(kind=8) :: br(nbvec)
        real(kind=8) :: vecpro(nbvec, nbvec)
        real(kind=8) :: valpro(nbvec)
        real(kind=8) :: valaux(nbvec)
        integer :: nitjac
        integer :: type
        integer :: iordre
    end subroutine jacobi
end interface
