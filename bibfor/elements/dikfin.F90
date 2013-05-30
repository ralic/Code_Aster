subroutine dikfin(nbt, dnsdu, dnsdt, dmsdt, dnsdu2,&
                  dnsdt2, dmsdt2, ky, kz, krx,&
                  krz, klv, klv2)
! ----------------------------------------------------------------------
    implicit none
    include 'asterfort/r8inir.h'
    integer :: nbt
    real(kind=8) :: dnsdu, dnsdt, dmsdt, dnsdu2, dnsdt2, dmsdt2, ky, kz, krx
    real(kind=8) :: krz
    real(kind=8) :: klv(nbt), klv2(nbt)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CALCUL DES MATRICES KLV ET KLV2 (MATRICES TANGENTE ET SECANTE)
!     POUR LES ELEMENTS DE TYPE CORNIERE.
!
! ----------------------------------------------------------------------
!
! IN  : NBT    : NOMBRE DE VALEURS POUR LES DEMI-MATRICES
!       DNSDU  :
!       DMSDT  :
!       DNSDT  :
!       DNSDU2 :
!       DMSDT2 :
!       DNSDT2 :
!       KY,KZ,KRX,KRZ : RAIDEURS POUR LES DIRECTIONS DE
!                       COMPORTEMENT LINEAIRE
!
! OUT : KLV    :
!       KLV2   :
!
! ----------------------------------------------------------------------
! --- MISE A ZERO DE KLV ET KLV2
!
!-----------------------------------------------------------------------
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    zero = 0.d0
    call r8inir(nbt, zero, klv, 1)
    call r8inir(nbt, zero, klv2, 1)
!
! --- AFFECTATION DES TERMES NON NULS DE KLV
!
    klv(1) = dnsdu
    klv(11) = dnsdt
    klv(3) = ky
    klv(6) = kz
    klv(10) = krx
    klv(15) = dmsdt
    klv(21) = krz
    klv(22) = -dnsdu
    klv(56) = -dnsdt
    klv(26) = -dnsdt
    klv(30) = -ky
    klv(39) = -kz
    klv(49) = -krx
    klv(60) = -dmsdt
    klv(72) = -krz
    klv(28) = dnsdu
    klv(62) = dnsdt
    klv(36) = ky
    klv(45) = kz
    klv(55) = krx
    klv(66) = dmsdt
    klv(78) = krz
!
! --- AFFECTATION DES TERMES NON NULS DE KLV2
!
    klv2(1) = dnsdu2
    klv2(11) = dnsdt2
    klv2(3) = ky
    klv2(6) = kz
    klv2(10) = krx
    klv2(15) = dmsdt2
    klv2(21) = krz
    klv2(22) = -dnsdu2
    klv2(56) = -dnsdt2
    klv2(26) = -dnsdt2
    klv2(30) = -ky
    klv2(39) = -kz
    klv2(49) = -krx
    klv2(60) = -dmsdt2
    klv2(72) = -krz
    klv2(28) = dnsdu2
    klv2(62) = dnsdt2
    klv2(36) = ky
    klv2(45) = kz
    klv2(55) = krx
    klv2(66) = dmsdt2
    klv2(78) = krz
! ----------------------------------------------------------------------
!
end subroutine
