subroutine crcnct(base, nomch, mailla, gd, nbcmp,&
                  licmp, rcmp)
    implicit none
!
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
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterc/indik8.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    integer :: nbcmp
    character(len=*) :: base, nomch, mailla, gd
    character(len=*) :: licmp(nbcmp)
    real(kind=8) :: rcmp(nbcmp)
! ----------------------------------------------------------------------
!     BUT:
!     ----
!      CREER UN CHAM_NO A REPRESENTATION CONSTANTE D'UNE GRANDEUR
!      DONNEE AVEC DES COMPOSANTES IDENTIQUES SUR TOUS LES NOEUDS.
!
!     ENTREES:
!     --------
!     BASE  (K1)  : BASE OU L'ON VEUT CREER LE CHAM_NO
!     MAILLA(K8)  : NOM DU MAILLAGE SUR LEQUEL ON VEUT CREER LE CHAM_NO
!     GD    (K8)  : NOM DE LA GRANDEUR ASSICIEE AU CHAMP
!     NBCMP (I)   : NOMBRE DE CMPS DE LICMP(*)
!     LICMP (LK8) : LISTE DES CMPS QUE L'ON VEUT SUR TOUS LES NOEUDS
!     RCMP  (LR)  : VALEURS DES CMPS QUE L'ON VEUT EN TOUS LES NOEUDS
!
!     SORTIES:
!     --------
!      LE CHAM_NO NOMCH EST CREE.
!
! ----------------------------------------------------------------------
    character(len=19) :: ch19
    character(len=8) :: maill2, gd2, nocmp
    character(len=1) :: bas2
    character(len=4) :: tysca
    character(len=24) :: valk(2)
!
    character(len=8) :: kbid
!
!
!
!-----------------------------------------------------------------------
    integer :: iadesc, iancmp, iarefe, iavale, ibid, icmp, iec
    integer :: ied, igd, iiec, ino, itrou, nbcmp2, nbno
    integer :: nec
!-----------------------------------------------------------------------
    call jemarq()
    gd2= gd
    maill2=mailla
    ch19=nomch
    bas2=base
!
!
!     VERIFICATION DES ARGUMENTS D'APPEL :
!     ------------------------------------
    call jenonu(jexnom('&CATA.GD.NOMGD', gd2), igd)
    if (igd .eq. 0) call u2mesk('F', 'CALCULEL2_21', 1, gd2)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', igd), 'L', iancmp)
    call jelira(jexnum('&CATA.GD.NOMCMP', igd), 'LONMAX', nbcmp2, kbid)
    do 1, icmp=1,nbcmp
    nocmp=licmp(icmp)
    itrou=indik8(zk8(iancmp),nocmp,1,nbcmp2)
    if (itrou .eq. 0) then
        valk(1) = nocmp
        valk(2) = gd2
        call u2mesk('F', 'CALCULEL2_22', 2, valk)
    endif
    1 end do
    call dismoi('F', 'NB_EC', gd2, 'GRANDEUR', nec,&
                kbid, ied)
    call dismoi('F', 'TYPE_SCA', gd2, 'GRANDEUR', ibid,&
                tysca, ied)
    if (tysca(1:1) .ne. 'R') call u2mesk('F', 'CALCULEL2_23', 1, gd2)
!
!
!     ALLOCATION DU CHAM_NO :
!     -----------------------
    call dismoi('F', 'NB_NO_MAILLA', maill2, 'MAILLAGE', nbno,&
                kbid, ied)
    call wkvect(ch19//'.VALE', bas2//' V R', nbcmp*nbno, iavale)
    call wkvect(ch19//'.DESC', bas2//' V I', nec+2, iadesc)
    call wkvect(ch19//'.REFE', bas2//' V K24', 4, iarefe)
!
!     OBJET: .REFE
!     ------------
    zk24(iarefe-1+1)=maill2
!
!     OBJET: .DESC
!     ------------
    call jeecra(ch19//'.DESC', 'DOCU', ibid, 'CHNO')
    zi(iadesc-1+1)=igd
    zi(iadesc-1+2)=-nbcmp
    do 2, icmp=1,nbcmp
    nocmp=licmp(icmp)
    itrou=indik8(zk8(iancmp),nocmp,1,nbcmp2)
    iec=(itrou-1)/30 +1
    iiec=itrou-(iec-1)*30
    zi(iadesc-1+2+iec)=ior(zi(iadesc-1+2+iec),2**iiec)
    2 end do
!
!     OBJET: .VALE
!     ------------
    do 3, icmp=1,nbcmp
    do 4, ino=1,nbno
    zr(iavale-1+(ino-1)*nbcmp+icmp)=rcmp(icmp)
 4  continue
    3 end do
!
!
    call jedema()
end subroutine
