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
    subroutine acgrdo(jvectn, jvectu, jvectv, nbordr, ordini,&
                      kwork, sompgw, jrwork, tspaq, ipg,&
                      jvecpg, jdtaum, jresun, nommet, nommat,&
                      nomcri, vala, coefpa, nomfor, grdvie,&
                      forvie, valpar, vresu)
        integer :: nbordr
        integer :: jvectn
        integer :: jvectu
        integer :: jvectv
        integer :: ordini
        integer :: kwork
        integer :: sompgw
        integer :: jrwork
        integer :: tspaq
        integer :: ipg
        integer :: jvecpg
        integer :: jdtaum
        integer :: jresun
        character(len=16) :: nommet
        character(len=8) :: nommat
        character(len=16) :: nomcri
        real(kind=8) :: vala
        real(kind=8) :: coefpa
        character(len=16) :: nomfor
        character(len=8) :: grdvie
        character(len=16) :: forvie
        real(kind=8) :: valpar(22)
        real(kind=8) :: vresu(24)
    end subroutine acgrdo
end interface
