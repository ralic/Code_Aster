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
    subroutine eifono(ndim, axi, nno1, nno2, npg,&
                      wref, vff1, vff2, dffr2, geom,&
                      ang, iu, im, sigp, vect)
        integer :: npg
        integer :: nno2
        integer :: nno1
        integer :: ndim
        logical :: axi
        real(kind=8) :: wref(npg)
        real(kind=8) :: vff1(nno1, npg)
        real(kind=8) :: vff2(nno2, npg)
        real(kind=8) :: dffr2(ndim-1, nno2, npg)
        real(kind=8) :: geom(ndim, nno2)
        real(kind=8) :: ang(*)
        integer :: iu(3, 18)
        integer :: im(3, 9)
        real(kind=8) :: sigp(2*ndim, npg)
        real(kind=8) :: vect(2*nno1*ndim+nno2*ndim)
    end subroutine eifono
end interface
