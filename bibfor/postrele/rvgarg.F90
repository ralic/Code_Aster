subroutine rvgarg(nxdnom, nxdnum, nvchef, nvcodo, nxdvar)
    implicit   none
!
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jeimpo.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/numek8.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utcmp2.h'
    include 'asterfort/utncmp.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: nxdnom, nxdnum, nvchef, nvcodo, nxdvar
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!      SAISIE ET PREPA VERIF COHERENCE DES ARGUMENTS D 'APPEL DE OP0051
!          1. VERIFICATION D' EXISTENCE DES OJB MIS EN JEU
!          2. VERIFICATION DE LEGALITE DES CMP MISES EN JEU
!          3. VERIFICATION DE COHERENCE SUR LES MAILLAGES SOUS-JACENTS
!     CES VERIFICATIONS SERONT EFFECTUEES PAR RVCOHE
!     SAISIE DES CMP MISES EN JEU ET DES OPERATION DEMANDEE
!     ------------------------------------------------------------------
! OUT NXDNOM : K : XD V K8 NOM DES CMP UTILISATEURS (OC(I) = OCCUR(I))
! OUT NXDNUM : K : XD V K8 NUM DES CMP UTILISATEURS (OC(I) = OCCUR(I))
! OUT NVCHEF : K : S V K24 NOM D' UN CHAMP EFFECTIFS(TYPE CHAM_GD)
! OUT NVCODO : K : S V I   CODE OPERATION POST
!     ------------------------------------------------------------------
!
    character(len=80) :: text80, text1
    character(len=24) :: naux24, kordre, nomobj
    character(len=19) :: nchp19
    character(len=16) :: nchsym
    character(len=8) :: k8b, nresu, nchgd, granch, nomcp(50)
    character(len=4) :: typech
    character(len=1) :: k1bid
    logical :: existe
    integer :: anomcp, anumcp, ancpu1, ancpu2, adesc, acpgd, avchef
    integer :: n1, n2, i, iocc, gd, n3, adr, nbelp, nbinv, ibid, ie, avcodo
    integer :: nbpost, nbchgd, nbcpgd, nbcmp, nbresu, nbtcp, nbsom
    integer :: ifr, j, jordr, jxvar, n4, nbc, nbnc, numecp(50)
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    integer :: iarg
!
!=================== CORPS DE LA ROUTINE =============================
!
    call jemarq()
    ifr = iunifi('RESULTAT')
!
    rbid = 1.0d-6
    cbid = dcmplx(rbid,rbid)
    call getfac('ACTION', nbpost)
    call jecrec(nxdnom, 'V V K8', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpost)
    call jecrec(nxdvar, 'V V I ', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpost)
    call jecrec(nxdnum, 'V V I ', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpost)
    call wkvect(nvchef, 'V V K24', nbpost, avchef)
    call wkvect(nvcodo, 'V V I', nbpost, avcodo)
    kordre = '&&RVGARG.NUMEORDR'
    do 100, iocc = 1, nbpost, 1
    n1 = 0
    n2 = 0
    call getvtx('ACTION', 'OPERATION', iocc, iarg, 0,&
                k8b, n3)
    n3 = -n3
    call wkvect('&&RVGARG.NOM.OPERATION', 'V V K80', n3, adr)
    call getvtx('ACTION', 'OPERATION', iocc, iarg, n3,&
                zk80(adr), n4)
    if (n3 .eq. 1) then
        text1 = zk80(adr + 1-1)
        if (text1(1:1) .eq. 'E') then
            zi(avcodo + iocc-1) = 1
        else
            zi(avcodo + iocc-1) = 3
        endif
    else
        zi(avcodo + iocc-1) = 2
    endif
    call jedetr('&&RVGARG.NOM.OPERATION')
    call getvid('ACTION', 'RESULTAT', iocc, iarg, 0,&
                k8b, nbresu)
    call getvid('ACTION', 'CHAM_GD', iocc, iarg, 0,&
                k8b, nbchgd)
    nbresu = -nbresu
    nbchgd = -nbchgd
    if (nbresu .ne. 0) then
!        /* CAS D' UN RESULTAT COMPOSE */
        call getvid('ACTION', 'RESULTAT', iocc, iarg, 1,&
                    nresu, n1)
        call getvtx('ACTION', 'NOM_CHAM', iocc, iarg, 1,&
                    text80, n1)
        nchsym = text80(1:16)
        call jenonu(jexnom(nresu//'           .DESC', nchsym), n1)
        if (n1 .ne. 0) then
!           /* LE CHAMP SYMBOLIQUE EXISTE (POTENTIELLEMENT)*/
            call rsorac(nresu, 'LONUTI', ibid, rbid, k8b,&
                        cbid, rbid, 'RELATIF', n3, 1,&
                        ibid)
            if (n3 .gt. 0) then
                call wkvect(kordre, 'V V I', n3, jordr)
                call rsorac(nresu, 'TOUT_ORDRE', ibid, rbid, k8b,&
                            cbid, rbid, 'RELATIF', zi(jordr), n3,&
                            ibid)
                do 10 j = 1, n3
                    call rsexch(' ', nresu, nchsym, zi(jordr+j-1), naux24,&
                                n2)
                    if (n2 .eq. 0) goto 12
10              continue
12              continue
                call jedetr(kordre)
            else
                n2 = 1
            endif
        else
!           /* LE CHAMP SYMBOLIQUE N' EXISTE PAS */
            n2 = 1
            write(ifr,*)'CHAMP SYMBOLIQUE >',nchsym,'< NON '//&
                'AUTORISE POUR LE RESULTAT >',nresu,'<'
            write(ifr,*)'LES CHAMPS SYMBOLIQUES AUTORISES SONT :'
            call jeimpo(ifr, nresu//'           .DESC', ' ')
        endif
        if ((n1 .eq. 0) .or. (n2 .ne. 0)) then
!           /* ALTERNATIVE :                              */
!           /* LE CHAMPS SYMBOLIQUE EST ILEGAL OU         */
!           /* AUCUN CHAMP EFFECTIF ASSOCIE N' A ETE CREE */
            existe = .false.
        else
            existe = .true.
            nchp19 = naux24(1:19)
        endif
    else
!        /* CAS D'UN CHAMP_GD */
        call getvid('ACTION', 'CHAM_GD', iocc, iarg, 1,&
                    nchgd, n1)
        existe = .true.
        nchp19 = nchgd//'           '
    endif
    call jecroc(jexnum(nxdnom, iocc))
    call jecroc(jexnum(nxdnum, iocc))
!
    if (.not. existe) then
!        /* LE CHAMPS SYMBOLIQUE EST ILEGAL, OU                 */
!        /* IL EST LEGAL, MAIS IL N' ADMET AUCUN CHAMP EFFECTIF */
        call jeecra(jexnum(nxdnom, iocc), 'LONMAX', 1, ' ')
        call jeveuo(jexnum(nxdnom, iocc), 'E', anomcp)
        zk8(anomcp) = '&NOEXIST'
        call jeecra(jexnum(nxdnum, iocc), 'LONMAX', 1, ' ')
        call jeveuo(jexnum(nxdnum, iocc), 'E', anumcp)
        zi(anumcp) = 0
        zk24(avchef + iocc-1) = '&NONEXISTEOUNONCREE     '
    else
        zk24(avchef + iocc-1) = nchp19//'     '
        call dismoi('F', 'TYPE_CHAMP', nchp19, 'CHAMP', ibid,&
                    typech, ie)
        call dismoi('F', 'NOM_GD', nchp19, 'CHAMP', ibid,&
                    granch, ie)
        call jeexin(nchp19//'.DESC', ibid)
        if (ibid .gt. 0) then
            call jeveuo(nchp19//'.DESC', 'L', adesc)
        else
            call jeveuo(nchp19//'.CELD', 'L', adesc)
        endif
!
        gd = zi(adesc + 1-1)
        call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', nbcpgd, k1bid)
        call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', acpgd)
        call getvtx('ACTION', 'NOM_CMP', iocc, iarg, 0,&
                    k8b, nbcmp)
        call getvtx('ACTION', 'TOUT_CMP', iocc, iarg, 0,&
                    k8b, nbtcp)
        call getvtx('ACTION', 'INVARIANT', iocc, iarg, 0,&
                    k8b, nbinv)
        call getvtx('ACTION', 'ELEM_PRINCIPAUX', iocc, iarg, 0,&
                    k8b, nbelp)
        call getvtx('ACTION', 'RESULTANTE', iocc, iarg, 0,&
                    k8b, nbsom)
        nbcmp = -nbcmp
        nbtcp = -nbtcp
        nbinv = -nbinv
        nbelp = -nbelp
        nbsom = -nbsom
        if ((nbcmp .ne. 0) .or. (nbsom .ne. 0)) then
!           /* PASSAGE D' UNE OU DEUX LISTE DE NOM DE CMPS    */
!           /* MOT-CLE (NOM_CMP) OU (RESULTANTE ET/OU MOMENT) */
            if (nbcmp .ne. 0) then
                call wkvect('&&OP0051.NOMCMP.USER', 'V V K8', nbcmp, ancpu1)
                call getvtx('ACTION', 'NOM_CMP', iocc, iarg, nbcmp,&
                            zk8(ancpu1), n1)
            else
                if (typech .eq. 'ELNO' .and. granch .eq. 'VARI_R') then
                    call u2mess('F', 'POSTRELE_20')
                endif
                call getvtx('ACTION', 'RESULTANTE', iocc, iarg, 0,&
                            k8b, n1)
                call getvtx('ACTION', 'MOMENT', iocc, iarg, 0,&
                            k8b, n2)
                n1 = -n1
                n2 = -n2
                nbcmp = n1+n2
                call wkvect('&&OP0051.NOMCMP.USER', 'V V K8', nbcmp, ancpu1)
                call getvtx('ACTION', 'RESULTANTE', iocc, iarg, n1,&
                            zk8(ancpu1), n1)
                call getvtx('ACTION', 'MOMENT', iocc, iarg, n2,&
                            zk8( ancpu1+n1), n2)
            endif
            if (typech .eq. 'ELNO' .and. granch .eq. 'VARI_R') then
                call utcmp2(granch, 'ACTION', iocc, 50, nomcp,&
                            numecp, nbnc)
                call jeecra(jexnum(nxdvar, iocc), 'LONMAX', nbnc, ' ')
                call jeecra(jexnum(nxdvar, iocc), 'LONUTI', nbnc, ' ')
                call jeveuo(jexnum(nxdvar, iocc), 'E', jxvar)
                do 112 i = 1, nbnc
                    zi(jxvar+i-1) = numecp(i)
112              continue
                nbcmp = 1
                zk8(ancpu1) = 'VARI'
            else
                call jeecra(jexnum(nxdvar, iocc), 'LONMAX', nbcmp, ' ')
                call jeecra(jexnum(nxdvar, iocc), 'LONUTI', 0, ' ')
            endif
            call jeecra(jexnum(nxdnom, iocc), 'LONMAX', nbcmp, ' ')
            call jeveuo(jexnum(nxdnom, iocc), 'E', anomcp)
            call jeecra(jexnum(nxdnum, iocc), 'LONMAX', nbcmp, ' ')
            call jeveuo(jexnum(nxdnum, iocc), 'E', anumcp)
            do 110, i = 1, nbcmp, 1
            zk8(anomcp + i-1) = zk8(ancpu1 + i-1)
110          continue
            call numek8(zk8(acpgd), zk8(anomcp), nbcpgd, nbcmp, zi(anumcp))
            call jedetr('&&OP0051.NOMCMP.USER')
        else if (nbtcp .ne. 0) then
!
            nomobj = '&&OP0051.NOMCMP.USER'
            call utncmp(nchp19, nbc, nomobj)
            if (nbc .eq. 0) call u2mesi('F', 'POSTRELE_54', 1, iocc)
            call jeveuo(nomobj, 'L', ancpu2)
            call jeecra(jexnum(nxdnom, iocc), 'LONMAX', nbc, ' ')
            call jeveuo(jexnum(nxdnom, iocc), 'E', anomcp)
            call jeecra(jexnum(nxdnum, iocc), 'LONMAX', nbc, ' ')
            call jeveuo(jexnum(nxdnum, iocc), 'E', anumcp)
            do 120, i = 1, nbc, 1
            zk8(anomcp + i-1) = zk8(ancpu2 + i-1)
120          continue
            call numek8(zk8(acpgd), zk8(anomcp), nbcpgd, nbc, zi(anumcp))
            call jedetr(nomobj)
            if (typech .eq. 'ELNO' .and. granch .eq. 'VARI_R') then
                call jeecra(jexnum(nxdvar, iocc), 'LONMAX', nbc, ' ')
                call jeecra(jexnum(nxdvar, iocc), 'LONUTI', nbc, ' ')
                call jeveuo(jexnum(nxdvar, iocc), 'E', jxvar)
                zi(jxvar) = -1
            else
                call jeecra(jexnum(nxdvar, iocc), 'LONUTI', 0, ' ')
            endif
        else
!           /* PASSAGE DE CMPS IMPLICITES */
            call jeecra(jexnum(nxdnom, iocc), 'LONMAX', 1, ' ')
            call jeveuo(jexnum(nxdnom, iocc), 'E', anomcp)
            call jeecra(jexnum(nxdnum, iocc), 'LONMAX', 1, ' ')
            call jeveuo(jexnum(nxdnum, iocc), 'E', anumcp)
            zk8(anomcp) = 'IMPLICIT'
            zi (anumcp) = -1
        endif
    endif
    100 end do
    call jedema()
end subroutine
