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
    subroutine irmhdf(ifi, ndim, nbnoeu, coordo, nbmail,&
                      connex, point, nomast, typma, titre,&
                      nbtitr, nbgrno, nomgno, nbgrma, nomgma,&
                      nommai, nomnoe, infmed)
        integer :: ifi
        integer :: ndim
        integer :: nbnoeu
        real(kind=8) :: coordo(*)
        integer :: nbmail
        integer :: connex(*)
        integer :: point(*)
        character(len=8) :: nomast
        integer :: typma(*)
        character(len=80) :: titre(*)
        integer :: nbtitr
        integer :: nbgrno
        character(len=24) :: nomgno(*)
        integer :: nbgrma
        character(len=24) :: nomgma(*)
        character(len=8) :: nommai(*)
        character(len=8) :: nomnoe(*)
        integer :: infmed
    end subroutine irmhdf
end interface
