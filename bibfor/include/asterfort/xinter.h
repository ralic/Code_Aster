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
    subroutine xinter(ndim, ndime, elrefp, geom, lsn, ia, ib,&
                      im, pintt, pmitt, lsna, lsnb, lsnm, inref, inter) 
        integer :: ndim
        integer :: ndime
        character(len=8) :: elrefp
        real(kind=8) :: geom(*)
        real(kind=8) :: lsn(*)
        real(kind=8) :: pintt(*)
        real(kind=8) :: pmitt(*)
        integer :: ia
        integer :: ib
        integer :: im
        real(kind=8) :: lsna
        real(kind=8) :: lsnb
        real(kind=8) :: lsnm
        real(kind=8) :: inref(3)
        real(kind=8) :: inter(3)
    end subroutine xinter
end interface
