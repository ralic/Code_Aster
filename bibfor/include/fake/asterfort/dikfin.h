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
    subroutine dikfin(nbt, dnsdu, dnsdt, dmsdt, dnsdu2,&
                      dnsdt2, dmsdt2, ky, kz, krx,&
                      krz, klv, klv2)
        integer :: nbt
        real(kind=8) :: dnsdu
        real(kind=8) :: dnsdt
        real(kind=8) :: dmsdt
        real(kind=8) :: dnsdu2
        real(kind=8) :: dnsdt2
        real(kind=8) :: dmsdt2
        real(kind=8) :: ky
        real(kind=8) :: kz
        real(kind=8) :: krx
        real(kind=8) :: krz
        real(kind=8) :: klv(nbt)
        real(kind=8) :: klv2(nbt)
    end subroutine dikfin
end interface
