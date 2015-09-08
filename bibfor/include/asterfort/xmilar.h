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
    subroutine xmilar(ndim, ndime, elrefp, geom,  pinref,&
                    ia, ib, im, ip, ksia, ksib, milara, milarb,&
                    pintt, pmitt)
        character(len=8) :: elrefp
        integer :: ndim
        integer :: ndime
        real(kind=8) :: geom(*)
        real(kind=8) :: pinref(*)
        integer :: ia
        integer :: ib
        integer :: im
        integer :: ip
        real(kind=8) :: ksia(ndime)
        real(kind=8) :: ksib(ndime)
        real(kind=8) :: milara(3)
        real(kind=8) :: milarb(3)
        real(kind=8) :: pintt(*)
        real(kind=8) :: pmitt(*)
    end subroutine xmilar
end interface
