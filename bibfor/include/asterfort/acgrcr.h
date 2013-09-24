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
    subroutine acgrcr(nbvec, jvectn, jvectu, jvectv, nbordr, &
                      kwork, sompgw, jrwork, tspaq, ipg, &
                      nommet,jvecno, jnorma, forcri,nompar,&
                      vanocr, respc,vnmax)
        integer :: nbvec
        integer :: jvectn
        integer :: jvectu
        integer :: jvectv 
        integer :: nbordr
        integer :: kwork
        integer :: sompgw
        integer :: jrwork
        integer :: tspaq
        integer :: ipg
        character(len=16) :: nommet
        integer :: jvecno
        integer :: jnorma
        character(len=16) :: forcri
        character(len=8) ::nompar(35)
        real(kind=8) ::vanocr(23)
        real(kind=8) :: respc(24)
        real(kind=8) :: vnmax(6)
    end subroutine acgrcr
end interface
