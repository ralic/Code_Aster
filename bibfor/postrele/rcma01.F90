subroutine rcma01(chmate, ima, ipt, nbm, adrm,&
                  vale)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: ima, ipt, nbm, adrm(*)
    real(kind=8) :: vale(*)
    character(len=24) :: chmate
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     RECUPERATION DES CARACTERISTIQUES MATERIAU POUR UNE MAILLE DONNEE
!
! IN  : CHMATE : CHAM_ELEM MATERIAU
! IN  : IMA    : NUMERO DE LA MAILLE
! IN  : IPT    : NUMERO DU NOEUD DE LA MAILLE
! IN  : NBM    : NB DE MAILLES AU NOEUD
! IN  : ADRM   : NUMERO DES MAILLES
! OUT : VALE   : CARACTERISTIQUES MATERIAU
!                VALE(1) = E     TEMPERATURE CALCUL
!                VALE(2) = E     TEMPERATURE AMBIANTE
!                VALE(3) = NU
!                VALE(4) = ALPHA
!                VALE(5) = E        A GAUCHE DU NOEUD A TEMP AMBIANTE
!                VALE(6) = E        A DROITE DU NOEUD A TEMP AMBIANTE
!                VALE(7) = ALPHA    A GAUCHE DU NOEUD
!                VALE(8) = ALPHA    A DROITE DU NOEUD
!                VALE(9) = E        MOYEN ENTRE LES 2 ZONES
!                VALE(10) = EC
!                VALE(11) = SM
!                VALE(12) = M
!                VALE(13) = N
!     ------------------------------------------------------------------
    integer :: vali(2)
    character(len=24) :: valk
!
    integer ::   jcesl, nbcmp, decma, decmb, icmp, iad, in, imb
    real(kind=8) :: ec, e, nu, alpha, ea, alphaa, eb, alphab
    real(kind=8), pointer :: cesv(:) => null()
    integer, pointer :: cesd(:) => null()
! DEB ------------------------------------------------------------------
    call jemarq()
!
! --- LE CHAMP MATERIAU
!
    call jeveuo(chmate(1:19)//'.CESV', 'L', vr=cesv)
    call jeveuo(chmate(1:19)//'.CESD', 'L', vi=cesd)
    call jeveuo(chmate(1:19)//'.CESL', 'L', jcesl)
    nbcmp = cesd(2)
    decma = cesd(5+4*(ima-1)+4)
!
! --- LE MATERIAU : E   TEMPERATURE CALCUL
!
    icmp = 1
    iad = decma + (ipt-1)*nbcmp + icmp
    if (.not. zl(jcesl-1+iad)) then
        vali (1) = ima
        vali (2) = ipt
        valk = 'E CALCUL'
        call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
    endif
    ec = cesv(iad)
!
! --- LE MATERIAU : E   TEMPERATURE AMBIANTE
!
    icmp = 2
    iad = decma + (ipt-1)*nbcmp + icmp
    if (.not. zl(jcesl-1+iad)) then
        vali (1) = ima
        vali (2) = ipt
        valk = 'E AMBIANT'
        call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
    endif
    e = cesv(iad)
!
! --- LE MATERIAU : NU
!
    icmp = 3
    iad = decma + (ipt-1)*nbcmp + icmp
    if (.not. zl(jcesl-1+iad)) then
        vali (1) = ima
        vali (2) = ipt
        valk = 'NU'
        call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
    endif
    nu = cesv(iad)
!
! --- LE MATERIAU : ALPHA
!
    icmp = 4
    iad = decma + (ipt-1)*nbcmp + icmp
    if (.not. zl(jcesl-1+iad)) then
        vali (1) = ima
        vali (2) = ipt
        valk = 'ALPHA'
        call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
    endif
    alpha = cesv(iad)
!
! --- TRAITEMENT DE LA DISCONTINUITE, A GAUCHE ET A DROITE DU NOEUD
!
    ea = 0.d0
    alphaa = 0.d0
    eb = 0.d0
    alphab = 0.d0
!
    if (nbm .eq. 1) then
        ea = e
        eb = e
        alphaa = alpha
        alphab = alpha
    else if (nbm .eq. 2) then
        ea = e
        alphaa = alpha
        do 104 in = 1, nbm
            if (adrm(in) .ne. ima) then
                imb = adrm(in)
                goto 106
            endif
104      continue
        call utmess('F', 'POSTRCCM_19')
106      continue
        decmb = cesd(5+4*(imb-1)+4)
        icmp = 2
        iad = decmb + (ipt-1)*nbcmp + icmp
        if (.not. zl(jcesl-1+iad)) then
            vali (1) = imb
            vali (2) = ipt
            valk = 'E_B'
            call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
        endif
        eb = cesv(iad)
        icmp = 4
        iad = decmb + (ipt-1)*nbcmp + icmp
        if (.not. zl(jcesl-1+iad)) then
            vali (1) = imb
            vali (2) = ipt
            valk = 'ALPHA_B'
            call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
        endif
        alphab = cesv(iad)
    endif
!
    vale(1) = ec
    vale(2) = e
    vale(3) = nu
    vale(4) = alpha
    vale(5) = ea
    vale(6) = eb
    vale(7) = alphaa
    vale(8) = alphab
    vale(9) = ( ea + eb ) / 2
!
! --- LE MATERIAU : E_REFE
!
    icmp = 5
    iad = decma + (ipt-1)*nbcmp + icmp
    if (.not. zl(jcesl-1+iad)) then
        vali (1) = ima
        vali (2) = ipt
        valk = 'E_REFE'
        call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
    endif
    vale(10) = cesv(iad)
!
! --- LE MATERIAU : SM
!
    icmp = 6
    iad = decma + (ipt-1)*nbcmp + icmp
    if (.not. zl(jcesl-1+iad)) then
        vali (1) = ima
        vali (2) = ipt
        valk = 'SM'
        call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
    endif
    vale(11) = cesv(iad)
!
! --- LE MATERIAU : M
!
    icmp = 7
    iad = decma + (ipt-1)*nbcmp + icmp
    if (.not. zl(jcesl-1+iad)) then
        vali (1) = ima
        vali (2) = ipt
        valk = 'M'
        call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
    endif
    vale(12) = cesv(iad)
!
! --- LE MATERIAU : N
!
    icmp = 8
    iad = decma + (ipt-1)*nbcmp + icmp
    if (.not. zl(jcesl-1+iad)) then
        vali (1) = ima
        vali (2) = ipt
        valk = 'N'
        call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
    endif
    vale(13) = cesv(iad)
!
!
! --- TYPE DE KE
!
    icmp = 9
    iad = decma + (ipt-1)*nbcmp + icmp
    if (.not. zl(jcesl-1+iad)) then
        vali (1) = ima
        vali (2) = ipt
        valk = 'TYPEKE'
        call utmess('F', 'POSTRCCM_18', sk=valk, ni=2, vali=vali)
    endif
    vale(14) = cesv(iad)
!
    call jedema()
end subroutine
