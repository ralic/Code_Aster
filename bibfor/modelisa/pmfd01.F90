subroutine pmfd01(noma, carele, vnbfib, vpoint, vcarfi,&
                  vnbfig, cesdec, ngmxel)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     COMMANDE AFFE_CARA_ELEM
!       FABRICATION DE 2 CHAM_ELEM_S/'ELEM' :
!          - CARELE//'.CANBSP'
!          - CARELE//'.CAFIBR'
!
!       TRAITEMENT DES MOTS CLES AFFE_SECT ET AFFE_FIBRE
!       TRANSFORMATION DES OBJETS VNBFIB,VPOINT,VCARFI
!       + PRISE EN COMPTE DE CESDEC
!
! ----------------------------------------------------------------------
    implicit none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterfort/assert.h'
    include 'asterfort/cescel.h'
    include 'asterfort/cescre.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/cesfus.h'
    include 'asterfort/codent.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: noma, carele, modele
    character(len=24) :: vnbfib, vpoint, vcarfi, vnbfig
    character(len=19) :: cesdec
    integer :: jnbfib, jpoint, jcarfi, jces1d, jces1v, jces1l, jnbfig
    integer :: iad, icmp, ncarfi, point, ima, ibid, nbma
    integer :: nb1, ispt, nncp, ifib, ig, nbgf, nbfig, nug, ipos, iret
    character(len=1) :: kbid, ki1
    character(len=2) :: ki2
    character(len=19) :: ces1, lichs(2), ces3, ligrmo, cel
    logical :: lcumul(2), exipmf
    real(kind=8) :: lcoefr(2)
    character(len=8) :: licmp(2+99)
    complex(kind=8) :: cbid
    integer :: ngmxel, nbcp, jsp
    integer :: iarg
!
!     ------------------------------------------------------------------
!
    call jemarq()
    nbcp=2+ngmxel
!
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                kbid, ibid)
    call getvid(' ', 'MODELE', 0, iarg, 1,&
                modele, ibid)
    ligrmo = modele//'.MODELE'
!
    call getfac('MULTIFIBRE', nb1)
!
    exipmf = (nb1.gt.0)
!
!     1. IL N'EXISTE PAS D'ELEMENTS PMF :
!     -------------------------------------------
    if (.not.exipmf) then
        cel = carele//'.CANBSP'
        call cescel(cesdec, ligrmo, 'TOU_INI_ELEM', ' ', 'NON',&
                    nncp, 'G', cel, 'A', iret)
        if (iret .eq. 0) goto 50
        call u2mesk('F', 'CALCULEL_6', 1, modele)
!
    endif
!
!
!     2. IL EXISTE DES ELEMENTS PMF :
!     --------------------------------------
    call jeveuo(vnbfib, 'L', jnbfib)
    call jeveuo(vpoint, 'L', jpoint)
    call jeveuo(vcarfi, 'L', jcarfi)
    call jeveuo(vnbfig, 'L', jnbfig)
!
!     2.1. CREATION DU CHAMP CARELE//'.CANBSP' :
!     --------------------------------------
    ces1 = '&&PMFD01.CES1'
    licmp(1)='NBFIBR'
    licmp(2)='NBGRFI'
    if (ngmxel .le. 9) then
        do 2 ig = 1, ngmxel
            call codent(ig, 'G', ki1)
            licmp(2+ig)='NUG'//ki1
 2      continue
    else if (ngmxel.le.99) then
        do 3 ig = 1, 9
            call codent(ig, 'G', ki1)
            licmp(2+ig)='NUG'//ki1
 3      continue
        do 4 ig = 10, ngmxel
            call codent(ig, 'G', ki2)
            licmp(2+ig)='NUG'//ki2
 4      continue
    endif
    call cescre('V', ces1, 'ELEM', noma, 'NBSP_I',&
                nbcp, licmp, -1, -1, -nbcp)
    call jeveuo(ces1//'.CESD', 'L', jces1d)
    call jeveuo(ces1//'.CESL', 'E', jces1l)
    call jeveuo(ces1//'.CESV', 'E', jces1v)
    do 10,ima = 1,nbma
    do 12 icmp = 1, nbcp
        call cesexi('C', jces1d, jces1l, ima, 1,&
                    1, icmp, iad)
        call assert(iad.lt.0)
        zl(jces1l-1-iad) = .true.
        zi(jces1v-1-iad) = zi(jnbfib-1+(ima-1)*nbcp+icmp)
12  continue
    10 end do
!
! --- 2.1.2. FUSION DE CES1 AVEC CESDEC :
!     --------------------------------------
    lichs(1) = ces1
    lichs(2) = cesdec
    lcumul(1) = .true.
    lcumul(2) = .true.
    lcoefr(1) = 1.d0
    lcoefr(2) = 1.d0
    ces3 = '&&PMFD01.CES3'
    call cesfus(2, lichs, lcumul, lcoefr, cbid,&
                .false., 'V', ces3)
    call detrsd('CHAM_ELEM_S', ces1)
!
    cel = carele//'.CANBSP'
    call cescel(ces3, ligrmo, 'TOU_INI_ELEM', ' ', 'NON',&
                nncp, 'G', cel, 'F', ibid)
    call detrsd('CHAM_ELEM_S', ces3)
!
!
!     2.2. CREATION DU CHAMP CARELE//'.CAFIBR' :
!     -----------------------------------------
!     NCARFI = NOMBRE DE CARACTERISTIQUES PAR FIBRE
    ncarfi = 3
!
! on fait un vecteur uniquement avec les nb de fibres
    call wkvect('&&PMFD01.NBSP', 'V V I', nbma, jsp)
    do 70 ima = 1, nbma
        zi(jsp-1+ima)=zi(jnbfib+(ima-1)*nbcp)
70  end do
!
    licmp(1)='XG'
    licmp(2)='YG'
    licmp(3)='AIRE'
    call cescre('V', ces1, 'ELEM', noma, 'CAFI_R',&
                3, licmp, -1, zi(jsp), -3)
    call jeveuo(ces1//'.CESD', 'L', jces1d)
    call jeveuo(ces1//'.CESL', 'E', jces1l)
    call jeveuo(ces1//'.CESV', 'E', jces1v)
    do 40,ima = 1,nbma
    ipos=jnbfib+(ima-1)*nbcp
    nbgf=zi(ipos+1)
    ispt=0
    do 37 ig = 1, nbgf
        nug = zi(ipos+1+ig)
        nbfig = zi(jnbfig-1+nug)
        point = zi(jpoint-1+nug)
        do 35,ifib = 1,nbfig
        ispt=ispt+1
        do 30,icmp = 1,ncarfi
        call cesexi('C', jces1d, jces1l, ima, 1,&
                    ispt, icmp, iad)
!              CALL ASSERT(IAD.LT.0)
        zl(jces1l-1-iad) = .true.
        zr(jces1v-1-iad)=zr(jcarfi-1+point-1+(ifib-1)*&
                    ncarfi+icmp)
30      continue
35      continue
37  continue
    40 end do
!
    cel = carele//'.CAFIBR'
    call cescel(ces1, ligrmo, 'TOU_INI_ELEM', ' ', 'NON',&
                nncp, 'G', cel, 'F', ibid)
    call detrsd('CHAM_ELEM_S', ces1)
!
!
50  continue
    call jedema()
end subroutine
