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
    subroutine xmilfa(elrefp, ndim, ndime, geom, cnset, nnose, it,&
                      ainter, ip1, ip2, pm2, typma, pinref, pmiref,&
                      ksi, milfa, pintt, pmitt)
        character(len=8) :: elrefp
        character(len=8) :: typma
        integer :: ndim
        integer :: ndime
        integer :: nnose
        integer :: it
        integer :: ip1
        integer :: ip2
        integer :: pm2
        integer :: cnset(*)
        real(kind=8) :: pinref(*)
        real(kind=8) :: pmiref(*)
        real(kind=8) :: geom(*)
        real(kind=8) :: ainter(*)
        real(kind=8) :: ksi(ndime)
        real(kind=8) :: milfa(ndim)
        real(kind=8) :: pintt(*)
        real(kind=8) :: pmitt(*)
    end subroutine xmilfa
end interface 
