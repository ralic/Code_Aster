subroutine rc36rs(nomres, noma, nbma, listma, chindi,&
                  chresu)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    integer :: nbma, listma(*)
    character(len=8) :: nomres, noma
    character(len=24) :: chindi, chresu
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
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!
!     TRANSFERT DE CHRESU DANS LA TABLE
!
!     ------------------------------------------------------------------
!
    integer :: ibid, jcesd, jcesv, im, ima, decal, nbcmp, nbpt, ipt, ino, jconx1
    integer :: jconx2, jcinv, jcind, npara, nbcin, decin, icmp, iad
    parameter   ( npara = 8 )
    real(kind=8) :: valer(5), type
    complex(kind=8) :: c16b
    character(len=8) :: valek(3), typara(npara)
    character(len=16) :: nopara(npara)
    character(len=24) :: connex, nommai, nomnoe
!     ------------------------------------------------------------------
    data nopara / 'MAILLE', 'TYPE_MAILLE', 'NOEUD' ,  'SM',&
     &              'SN_MAX', 'SN/3SM', 'SALT_MAX' , 'FACT_USAGE_CUMU' /
    data typara / 'K8'    , 'K8'    , 'K8'       , 'R' ,&
     &              'R'     , 'R'     , 'R'        , 'R' /
! DEB ------------------------------------------------------------------
!
    call tbcrsd(nomres, 'G')
    call tbajpa(nomres, npara, nopara, typara)
!
    nommai = noma//'.NOMMAI         '
    nomnoe = noma//'.NOMNOE         '
    connex = noma//'.CONNEX         '
    call jeveuo(connex, 'L', jconx1)
    call jeveuo(jexatr(connex, 'LONCUM'), 'L', jconx2)
!
! --- LE CHAMP INDICE DE CONTRAINTES
!
    call jeveuo(chindi(1:19)//'.CESV', 'L', jcinv)
    call jeveuo(chindi(1:19)//'.CESD', 'L', jcind)
    nbcin = zi(jcind-1+2)
!
! --- LE CHAM_ELEM RESULTAT
!
    call jeveuo(chresu(1:19)//'.CESD', 'L', jcesd)
    call jeveuo(chresu(1:19)//'.CESV', 'L', jcesv)
    nbcmp = zi(jcesd-1+2)
!
    do 10 im = 1, nbma
!
        ima = listma(im)
        call jenuno(jexnum(nommai, ima), valek(1))
!
        nbpt = zi(jcesd-1+5+4*(ima-1)+1)
        decal = zi(jcesd-1+5+4*(ima-1)+4)
        decin = zi(jcind-1+5+4*(ima-1)+4)
!
        do 20 ipt = 1, nbpt
!
            icmp = 7
            iad = decin + (ipt-1)*nbcin + icmp
            type = zr(jcinv-1+iad)
            if (type .eq. 0.d0) then
                valek(2) = '???'
            else if (type .eq. 10.d0) then
                valek(2) = 'DRO'
            else if (type .eq. 20.d0) then
                valek(2) = 'COU'
            else if (type .eq. 30.d0) then
                valek(2) = 'TRN'
            else if (type .eq. 40.d0) then
                valek(2) = 'TEE'
            endif
!
            ino = zi(jconx1-1+zi(jconx2+ima-1)+ipt-1)
            call jenuno(jexnum(nomnoe, ino), valek(3))
!
            do 30 icmp = 1, nbcmp
!
                iad = decal + (ipt-1)*nbcmp + icmp
                valer(icmp) = zr(jcesv-1+iad)
!
30          continue
!
            call tbajli(nomres, npara, nopara, ibid, valer,&
                        c16b, valek, 0)
!
20      continue
!
10  end do
!
end subroutine
