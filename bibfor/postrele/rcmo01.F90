subroutine rcmo01(chmome, ima, ipt, vale)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesg.h'
    integer :: ima, ipt
    real(kind=8) :: vale(*)
    character(len=24) :: chmome
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     RECUPERATION DES CARACTERISTIQUES MATERIAU POUR UNE MAILLE DONNEE
!
! IN  : CHMOME : CHAM_ELEM DE MOMENT RESULTANT
! IN  : IMA    : NUMERO DE LA MAILLE
! IN  : IPT    : NUMERO DU NOEUD DE LA MAILLE
! OUT : VALE   : MOMENT RESULTANT
!                VALE(1) = MX
!                VALE(2) = MY
!                VALE(3) = MZ
!     ------------------------------------------------------------------
    character(len=24) :: valk
!
    integer :: jcesv, jcesd, jcesl, nbcmp, decal, icmp, iad
    integer :: vali(2)
! DEB ------------------------------------------------------------------
    call jemarq()
!
! --- LE CHAMP MOMENT
!
    call jeveuo(chmome(1:19)//'.CESV', 'L', jcesv)
    call jeveuo(chmome(1:19)//'.CESD', 'L', jcesd)
    call jeveuo(chmome(1:19)//'.CESL', 'L', jcesl)
    nbcmp = zi(jcesd-1+2)
    decal = zi(jcesd-1+5+4*(ima-1)+4)
!
! --- LES VALEURS DES COMPOSANTES
!
    do 10 icmp = 1, 3
        iad = decal + (ipt-1)*nbcmp + icmp
        if (.not. zl(jcesl-1+iad)) then
            vali (1) = ima
            vali (2) = ipt
            valk = 'MOMENT'
            call u2mesg('F', 'POSTRCCM_18', 1, valk, 2,&
                        vali, 0, 0.d0)
        endif
        vale(icmp) = zr(jcesv-1+iad)
10  end do
!
    call jedema()
end subroutine
