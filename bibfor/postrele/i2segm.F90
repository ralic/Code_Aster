subroutine i2segm(nomail, nbpars, nbpara)
    implicit  none
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8dgrd.h'
    include 'asterfort/i2cpcx.h'
    include 'asterfort/i2imac.h'
    include 'asterfort/i2imas.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/padist.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utcono.h'
    include 'asterfort/wkvect.h'
    integer :: nbpars, nbpara
    character(len=8) :: nomail
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
! TOLE  CRP_20
!
!     OPERATEUR INTE_MAIL_2D,
!               MOTS CLES FACTEURS  "DEFI_SEGMENT"  ET  "DEFI_ARC"
!
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: atypcb, anomma, aasgt, absgt, acarc, ararc, asarc
    integer :: aorsgt, aexsgt, aparor, aparex, afacor, afacex, acnxor, acnxex
    integer :: amail1, amail2
    integer :: vorsgt, vexsgt, vparor, vparex, vfacor, vfacex, vcnxor, vcnxex
    integer :: vmail1, vmail2
    integer :: n1, n2, occa, occs, lcnx, lsgt, nbmail, nbsgt, nbcnx, nbsegm
    integer :: ifm, niv, occ, nbpart, nbm, im, ia, i
    integer :: nbseg, nbsor, jnuma
    integer :: numm1, numm2, ndim, vali(2)
    real(kind=8) :: point(2), poina(2), poinb(2), dgrd
    real(kind=8) :: epsi, xa, xb, xc, ya, yb, yc, r, alfinf, alfsup
    real(kind=8) :: epsi2, tole, dm, xrc1, xrc2, xex, xor, yex, yor
    character(len=1) :: k1b
    character(len=8) :: k8b, nomcrb, nomm1, nomm2, crit
    character(len=16) :: opera, typcrb, motcle(3), typmcl(2)
    character(len=24) :: conec, coord, type, nommai, lismai
    character(len=24) :: norsgt, nexsgt, nparor, nparex, ncnxor, ncnxex
    character(len=24) :: nmail1, nmail2, nfacor, nfacex, ntpcrb
    character(len=24) :: npasgt, npbsgt, npcarc, nrarc, nsarc, nnomma
    character(len=24) :: valk(7)
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
    dgrd = r8dgrd()
    occ = 0
    ndim = 2
!
    call infniv(ifm, niv)
!
    call getres(nomcrb, typcrb, opera)
    call getvr8(' ', 'PRECISION', 0, iarg, 1,&
                epsi, n2)
!
    conec = nomail//'.CONNEX         '
    coord = nomail//'.COORDO    .VALE'
    type = nomail//'.TYPMAIL        '
    nommai = nomail//'.NOMMAI         '
    call jelira(conec, 'NMAXOC', nbmail, k1b)
!
!     --- TRAITEMENT DES GROUP_MA ET MAILLE ---
!
    lismai = '&&OP0050.NUME_MAIL'
    call getvtx(' ', 'GROUP_MA', 0, iarg, 0,&
                k8b, n1)
    call getvtx(' ', 'MAILLE', 0, iarg, 0,&
                k8b, n2)
    if ((n1+n2) .eq. 0) then
        call wkvect(lismai, 'V V I', nbmail, jnuma)
        do 10,i = 1,nbmail,1
        zi(jnuma+i-1) = i
10      continue
    else
        motcle(1) = 'GROUP_MA'
        motcle(2) = 'MAILLE'
        typmcl(1) = 'GROUP_MA'
        typmcl(2) = 'MAILLE'
        call reliem(' ', nomail, 'NU_MAILLE', ' ', 1,&
                    2, motcle, typmcl, lismai, nbmail)
        call jeveuo(lismai, 'L', jnuma)
    endif
!
    norsgt = nomcrb//'.ORSGT'
    nexsgt = nomcrb//'.EXSGT'
    nparor = nomcrb//'.PAROR'
    nparex = nomcrb//'.PAREX'
    nmail1 = nomcrb//'.MAIL1'
    nmail2 = nomcrb//'.MAIL2'
    nfacor = nomcrb//'.FACOR'
    nfacex = nomcrb//'.FACEX'
    ncnxor = nomcrb//'.CNXOR'
    ncnxex = nomcrb//'.CNXEX'
    ntpcrb = nomcrb//'.TYPCOURBE'
    npasgt = nomcrb//'.XYASGT'
    npbsgt = nomcrb//'.XYBSGT'
    npcarc = nomcrb//'.XYCARC'
    nrarc = nomcrb//'.XRARC'
    nsarc = nomcrb//'.XSARC'
    nnomma = nomcrb//'.NOMMAIL'
!
    call wkvect(npasgt, 'G V R', 2* (nbpars+1), aasgt)
    call wkvect(npbsgt, 'G V R', 2* (nbpars+1), absgt)
    call wkvect(npcarc, 'G V R', 2* (nbpara+1), acarc)
    call wkvect(nsarc, 'G V R', 2* (nbpara+1), asarc)
    call wkvect(nrarc, 'G V R', nbpara+1, ararc)
!
!       /* CREATION DES VARIABLES TEMPORAIRES REPRESENTANT */
!       /*           L' INTERSECTION                       */
!
    call wkvect('&VINTERORSGT', 'V V R', 10000, vorsgt)
    call wkvect('&VINTEREXSGT', 'V V R', 10000, vexsgt)
    call wkvect('&VINTERPAROR', 'V V R', 10000, vparor)
    call wkvect('&VINTERPAREX', 'V V R', 10000, vparex)
    call wkvect('&VINTERMAIL1', 'V V I', 10000, vmail1)
    call wkvect('&VINTERMAIL2', 'V V I', 10000, vmail2)
    call wkvect('&VINTERFACOR', 'V V I', 10000, vfacor)
    call wkvect('&VINTERFACEX', 'V V I', 10000, vfacex)
    call wkvect('&VINTERCNXOR', 'V V I', 100, vcnxor)
    call wkvect('&VINTERCNXEX', 'V V I', 100, vcnxex)
!
!       /* CREATION DE LA STRUCTURE DE DONNEES TEMPORAIRE ASSOCIEE */
!       /*         AU CONCEPT PRODUIT                              */
!
    nbpart = nbpara + nbpars
    call jecrec('&ORSGT', 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    call jecrec('&EXSGT', 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    call jecrec('&PAROR', 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    call jecrec('&PAREX', 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    call jecrec('&MAIL1', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    call jecrec('&MAIL2', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    call jecrec('&FACOR', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    call jecrec('&FACEX', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    call jecrec('&CNXOR', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    call jecrec('&CNXEX', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    occa = 1
    occs = 1
30  continue
    if ((occs.le.nbpars) .or. (occa.le.nbpara)) then
        do 40,i = 1,10000,1
        zr(vorsgt+i-1) = -1.0d0
        zr(vexsgt+i-1) = -1.0d0
        zr(vparor+i-1) = -1.0d0
        zr(vparex+i-1) = -1.0d0
        zi(vmail1+i-1) = -1
        zi(vmail2+i-1) = -1
        zi(vfacor+i-1) = -1
        zi(vfacex+i-1) = -1
40      continue
!
!      /* CALCUL DE L' INTERSECTION */
!
        if (occs .le. nbpars) then
            motcle(1) = 'ORIGINE'
            motcle(2) = 'NOEUD_ORIG'
            motcle(3) = 'GROUP_NO_ORIG'
            call utcono('DEFI_SEGMENT', motcle, occs, nomail, ndim,&
                        point, n1)
            xa = point(1)
            ya = point(2)
            zr(aasgt+2*occs) = xa
            zr(aasgt+2*occs+1) = ya
!
            motcle(1) = 'EXTREMITE'
            motcle(2) = 'NOEUD_EXTR'
            motcle(3) = 'GROUP_NO_EXTR'
            call utcono('DEFI_SEGMENT', motcle, occs, nomail, ndim,&
                        point, n1)
            xb = point(1)
            yb = point(2)
            zr(absgt+2*occs) = xb
            zr(absgt+2*occs+1) = yb
!
            occs = occs + 1
            call i2imas(epsi, conec, coord, type, nbmail,&
                        zi(jnuma), xa, ya, xb, yb,&
                        nbsgt, zr(vorsgt), zr(vexsgt), zi(vmail1), zi(vmail2),&
                        zi(vfacor), zi(vfacex), zr(vparor), zr(vparex))
        else
!
            motcle(1) = 'CENTRE'
            motcle(2) = 'NOEUD_CENTRE'
            motcle(3) = 'GROUP_NO_CENTRE'
            call utcono('DEFI_ARC', motcle, occa, nomail, ndim,&
                        point, n1)
            xc = point(1)
            yc = point(2)
            zr(acarc+2*occa) = xc
            zr(acarc+2*occa+1) = yc
!
            call getvr8('DEFI_ARC', 'RAYON', occa, iarg, 0,&
                        point, n1)
            if (n1 .ne. 0) then
                call getvr8('DEFI_ARC', 'RAYON', occa, iarg, 1,&
                            point, n1)
                r = point(1)
                if (r .le. 0.d0) call u2mess('F', 'INTEMAIL_5')
                call getvr8('DEFI_ARC', 'SECTEUR', occa, iarg, 2,&
                            point, n1)
                alfinf = point(1)*dgrd
                alfsup = point(2)*dgrd
                if (( -180.0d0 .ge. alfinf ) .or. ( alfinf .ge. alfsup ) .or.&
                    ( alfsup .gt. 180.0d0 )) then
                    valk (1) = 'DEFI_ARC'
                    valk (2) = 'SECTEUR'
                    call u2mesg('F', 'INTEMAIL_19', 2, valk, 1,&
                                occa, 0, 0.d0)
                endif
            else
!
                motcle(1) = 'ORIGINE'
                motcle(2) = 'NOEUD_ORIG'
                motcle(3) = 'GROUP_NO_ORIG'
                call utcono('DEFI_ARC', motcle, occa, nomail, ndim,&
                            poina, n1)
!
                motcle(1) = 'EXTREMITE'
                motcle(2) = 'NOEUD_EXTR'
                motcle(3) = 'GROUP_NO_EXTR'
                call utcono('DEFI_ARC', motcle, occa, nomail, ndim,&
                            poinb, n1)
!
                call getvtx('DEFI_ARC', 'CRITERE', occa, iarg, 1,&
                            crit, n1)
                call getvr8('DEFI_ARC', 'PRECISION', occa, iarg, 1,&
                            epsi2, n1)
                xrc1 = padist(2,poina,point)
                xrc2 = padist(2,poinb,point)
                if (crit(1:4) .eq. 'RELA') then
!     ON UTILISE LA MOYENNE ARITHMETIQUE DES RAYONS COMME REFERENCE
                    dm = (xrc1+xrc2)/2.d0
                    tole = epsi2*dm
                else
                    tole = epsi2
                endif
                if (abs(xrc1-xrc2) .gt. tole) then
                    valk (1) = 'DEFI_ARC'
                    call u2mesg('F', 'INTEMAIL_20', 1, valk, 1,&
                                occa, 0, 0.d0)
                endif
                r = xrc1
                alfinf = atan2(poina(2)-point(2),poina(1)-point(1))
                alfsup = atan2(poinb(2)-point(2),poinb(1)-point(1))
            endif
            zr(ararc+occa) = r
            zr(asarc+2*occa) = alfinf
            zr(asarc+2*occa+1) = alfsup
            occa = occa + 1
            call i2imac(epsi, conec, coord, type, nbmail,&
                        zi(jnuma), xc, yc, r, alfinf,&
                        alfsup, nbsgt, zr(vorsgt), zr(vexsgt), zi(vmail1),&
                        zi(vmail2), zi(vfacor), zi(vfacex), zr(vparor), zr(vparex))
        endif
        if (nbsgt .gt. 0) then
            occ = occa + occs - 2
            call jecroc(jexnum('&ORSGT', occ))
            call jeecra(jexnum('&ORSGT', occ), 'LONMAX', nbsgt, ' ')
            call jeveuo(jexnum('&ORSGT', occ), 'E', aorsgt)
            call jecroc(jexnum('&EXSGT', occ))
            call jeecra(jexnum('&EXSGT', occ), 'LONMAX', nbsgt, ' ')
            call jeveuo(jexnum('&EXSGT', occ), 'E', aexsgt)
            call jecroc(jexnum('&PAROR', occ))
            call jeecra(jexnum('&PAROR', occ), 'LONMAX', nbsgt, ' ')
            call jeveuo(jexnum('&PAROR', occ), 'E', aparor)
            call jecroc(jexnum('&PAREX', occ))
            call jeecra(jexnum('&PAREX', occ), 'LONMAX', nbsgt, ' ')
            call jeveuo(jexnum('&PAREX', occ), 'E', aparex)
            call jecroc(jexnum('&MAIL1', occ))
            call jeecra(jexnum('&MAIL1', occ), 'LONMAX', nbsgt, ' ')
            call jeveuo(jexnum('&MAIL1', occ), 'E', amail1)
            call jecroc(jexnum('&MAIL2', occ))
            call jeecra(jexnum('&MAIL2', occ), 'LONMAX', nbsgt, ' ')
            call jeveuo(jexnum('&MAIL2', occ), 'E', amail2)
            call jecroc(jexnum('&FACOR', occ))
            call jeecra(jexnum('&FACOR', occ), 'LONMAX', nbsgt, ' ')
            call jeveuo(jexnum('&FACOR', occ), 'E', afacor)
            call jecroc(jexnum('&FACEX', occ))
            call jeecra(jexnum('&FACEX', occ), 'LONMAX', nbsgt, ' ')
            call jeveuo(jexnum('&FACEX', occ), 'E', afacex)
!
!        /* SAUVEGARDE DE L' INTERSECTION DANS CETTE STRUCTURE */
!
            if (nbsgt .gt. 10000) then
                nbsegm=nbsgt/10000+1
                vali(1) = occ
                vali(2) = nbsegm
                call u2mesg('F', 'INTEMAIL_33', 0, ' ', 2,&
                            vali, 0, 0.d0)
            endif
            do 50,i = 1,nbsgt,1
            zr(aorsgt+i-1) = zr(vorsgt+i-1)
            zr(aexsgt+i-1) = zr(vexsgt+i-1)
            zr(aparor+i-1) = zr(vparor+i-1)
            zr(aparex+i-1) = zr(vparex+i-1)
            zi(amail1+i-1) = zi(vmail1+i-1)
            zi(amail2+i-1) = zi(vmail2+i-1)
            zi(afacor+i-1) = zi(vfacor+i-1)
            zi(afacex+i-1) = zi(vfacex+i-1)
50          continue
            do 60,i = 1,100,1
            zi(vcnxor+i-1) = 0
            zi(vcnxex+i-1) = 0
60          continue
!
!        /* CALCUL DES COMPOSANTES CONNEXES */
!
            call i2cpcx(epsi, zr(aorsgt), zr(aexsgt), zi(vcnxor), zi( vcnxex),&
                        nbsgt, nbcnx)
!
!        /* CREATION DE LA STRUCTURE DU CONCEPT PRODUIT      */
!        /* ASSOCIEE AUX COMPOSANTES CONNEXES DE L' INTERSECTION*/
!
            call jecroc(jexnum('&CNXOR', occ))
            call jeecra(jexnum('&CNXOR', occ), 'LONMAX', nbcnx, ' ')
            call jeveuo(jexnum('&CNXOR', occ), 'E', acnxor)
            call jecroc(jexnum('&CNXEX', occ))
            call jeecra(jexnum('&CNXEX', occ), 'LONMAX', nbcnx, ' ')
            call jeveuo(jexnum('&CNXEX', occ), 'E', acnxex)
!
!        /* SAUVEGARDE DE L' INTERSECTION DANS CETTE STRUCTURE */
!
            do 70,i = 1,nbcnx,1
            zi(acnxor+i-1) = zi(vcnxor+i-1)
            zi(acnxex+i-1) = zi(vcnxex+i-1)
70          continue
        else
            occ = occa + occs - 2
            valk (1) = nomcrb
            call u2mesg('F', 'INTEMAIL_21', 1, valk, 1,&
                        occ, 0, 0.d0)
        endif
        goto 30
    endif
!
!        /* CREATION DE LA STRUCTURE DE DONNEES PRODUIT */
!
    call wkvect(ntpcrb, 'G V K8', 1, atypcb)
    zk8(atypcb) = 'SGTDARCC'
!
    call wkvect(nnomma, 'G V K8', 1, anomma)
    zk8(anomma) = nomail
!
    call jecrec(norsgt, 'G V R', 'NU', 'CONTIG', 'VARIABLE',&
                nbpart)
    call jecrec(nexsgt, 'G V R', 'NU', 'CONTIG', 'VARIABLE',&
                nbpart)
    call jecrec(nparor, 'G V R', 'NU', 'CONTIG', 'VARIABLE',&
                nbpart)
    call jecrec(nparex, 'G V R', 'NU', 'CONTIG', 'VARIABLE',&
                nbpart)
    call jecrec(nmail1, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbpart)
    call jecrec(nmail2, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbpart)
    call jecrec(nfacor, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbpart)
    call jecrec(nfacex, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbpart)
    call jecrec(ncnxor, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbpart)
    call jecrec(ncnxex, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbpart)
!
    lsgt = 0
    lcnx = 0
    nbpart = nbpara + nbpars
    do 80,occ = 1,nbpart,1
    call jelira(jexnum('&ORSGT', occ), 'LONMAX', n1, k1b)
    lsgt = lsgt + n1
    call jelira(jexnum('&CNXOR', occ), 'LONMAX', n1, k1b)
    lcnx = lcnx + n1
    80 end do
    call jeecra(norsgt, 'LONT', lsgt, ' ')
    call jeecra(nexsgt, 'LONT', lsgt, ' ')
    call jeecra(nparor, 'LONT', lsgt, ' ')
    call jeecra(nparex, 'LONT', lsgt, ' ')
    call jeecra(nmail1, 'LONT', lsgt, ' ')
    call jeecra(nmail2, 'LONT', lsgt, ' ')
    call jeecra(nfacor, 'LONT', lsgt, ' ')
    call jeecra(nfacex, 'LONT', lsgt, ' ')
    call jeecra(ncnxor, 'LONT', lcnx, ' ')
    call jeecra(ncnxex, 'LONT', lcnx, ' ')
!
!        /* RECOPIE DES RESULTATS DANS LA STRUCTURE PRODUIT */
!
    do 110,occ = 1,nbpart,1
    call jelira(jexnum('&ORSGT', occ), 'LONMAX', n1, k1b)
    call jeveuo(jexnum('&ORSGT', occ), 'E', vorsgt)
    call jeveuo(jexnum('&EXSGT', occ), 'E', vexsgt)
    call jeveuo(jexnum('&PAROR', occ), 'E', vparor)
    call jeveuo(jexnum('&PAREX', occ), 'E', vparex)
    call jeveuo(jexnum('&FACOR', occ), 'E', vfacor)
    call jeveuo(jexnum('&FACEX', occ), 'E', vfacex)
    call jeveuo(jexnum('&MAIL1', occ), 'E', vmail1)
    call jeveuo(jexnum('&MAIL2', occ), 'E', vmail2)
    call jecroc(jexnum(norsgt, occ))
    call jeecra(jexnum(norsgt, occ), 'LONMAX', n1, ' ')
    call jeveuo(jexnum(norsgt, occ), 'E', aorsgt)
    call jecroc(jexnum(nexsgt, occ))
    call jeecra(jexnum(nexsgt, occ), 'LONMAX', n1, ' ')
    call jeveuo(jexnum(nexsgt, occ), 'E', aexsgt)
    call jecroc(jexnum(nparor, occ))
    call jeecra(jexnum(nparor, occ), 'LONMAX', n1, ' ')
    call jeveuo(jexnum(nparor, occ), 'E', aparor)
    call jecroc(jexnum(nparex, occ))
    call jeecra(jexnum(nparex, occ), 'LONMAX', n1, ' ')
    call jeveuo(jexnum(nparex, occ), 'E', aparex)
    call jecroc(jexnum(nfacor, occ))
    call jeecra(jexnum(nfacor, occ), 'LONMAX', n1, ' ')
    call jeveuo(jexnum(nfacor, occ), 'E', afacor)
    call jecroc(jexnum(nfacex, occ))
    call jeecra(jexnum(nfacex, occ), 'LONMAX', n1, ' ')
    call jeveuo(jexnum(nfacex, occ), 'E', afacex)
    call jecroc(jexnum(nmail1, occ))
    call jeecra(jexnum(nmail1, occ), 'LONMAX', n1, ' ')
    call jeveuo(jexnum(nmail1, occ), 'E', amail1)
    call jecroc(jexnum(nmail2, occ))
    call jeecra(jexnum(nmail2, occ), 'LONMAX', n1, ' ')
    call jeveuo(jexnum(nmail2, occ), 'E', amail2)
    do 90,i = 1,n1,1
    zr(aorsgt+i-1) = zr(vorsgt+i-1)
    zr(aexsgt+i-1) = zr(vexsgt+i-1)
    zr(aparor+i-1) = zr(vparor+i-1)
    zr(aparex+i-1) = zr(vparex+i-1)
    zi(amail1+i-1) = zi(vmail1+i-1)
    zi(amail2+i-1) = zi(vmail2+i-1)
    zi(afacor+i-1) = zi(vfacor+i-1)
    zi(afacex+i-1) = zi(vfacex+i-1)
90  continue
    call jelira(jexnum('&CNXOR', occ), 'LONMAX', n1, k1b)
    call jeveuo(jexnum('&CNXOR', occ), 'E', vcnxor)
    call jeveuo(jexnum('&CNXEX', occ), 'E', vcnxex)
    call jecroc(jexnum(ncnxor, occ))
    call jeecra(jexnum(ncnxor, occ), 'LONMAX', n1, ' ')
    call jeveuo(jexnum(ncnxor, occ), 'E', acnxor)
    call jecroc(jexnum(ncnxex, occ))
    call jeecra(jexnum(ncnxex, occ), 'LONMAX', n1, ' ')
    call jeveuo(jexnum(ncnxex, occ), 'E', acnxex)
!
!        /* SAUVEGARDE DE L' INTERSECTION DANS CETTE STRUCTURE */
!
    do 100,i = 1,n1,1
    zi(acnxor+i-1) = zi(vcnxor+i-1)
    zi(acnxex+i-1) = zi(vcnxex+i-1)
100  continue
    110 end do
    call jedetr('&VINTERORSGT')
    call jedetr('&VINTEREXSGT')
    call jedetr('&VINTERPAROR')
    call jedetr('&VINTERPAREX')
    call jedetr('&VINTERMAIL1')
    call jedetr('&VINTERMAIL2')
    call jedetr('&VINTERFACOR')
    call jedetr('&VINTERFACEX')
    call jedetr('&VINTERCNXOR')
    call jedetr('&VINTERCNXEX')
    call jedetr('&ORSGT')
    call jedetr('&EXSGT')
    call jedetr('&PAROR')
    call jedetr('&PAREX')
    call jedetr('&MAIL1')
    call jedetr('&MAIL2')
    call jedetr('&FACOR')
    call jedetr('&FACEX')
    call jedetr('&CNXOR')
    call jedetr('&CNXEX')
    call jedetr(lismai)
!
    if (niv .ge. 2) then
        write (ifm,2000)
        write (ifm,2010) nbpart
        call jeveuo(npasgt, 'L', aasgt)
        call jeveuo(npbsgt, 'L', absgt)
        call jeveuo(npcarc, 'L', acarc)
        call jeveuo(nsarc, 'L', asarc)
        call jeveuo(nrarc, 'L', ararc)
        call jelira(npasgt, 'LONMAX', nbseg, k1b)
        if (nbseg .eq. 2) goto 140
        do 130,occ = 1,nbpars,1
        call jelira(jexnum(norsgt, occ), 'LONMAX', nbm, k1b)
        call jeveuo(jexnum(norsgt, occ), 'L', aorsgt)
        call jeveuo(jexnum(nexsgt, occ), 'L', aexsgt)
        call jeveuo(jexnum(nmail1, occ), 'L', amail1)
        call jeveuo(jexnum(nmail2, occ), 'L', amail2)
        call jeveuo(jexnum(nfacor, occ), 'L', afacor)
        call jeveuo(jexnum(nfacex, occ), 'L', afacex)
        call jeveuo(jexnum(ncnxor, occ), 'L', acnxor)
        call jelira(jexnum(ncnxor, occ), 'LONMAX', nbsor, k1b)
        write (ifm,2020) occ,nbsor
        write (ifm,2030) 0.d0,zr(aasgt+2*occ),zr(aasgt+2*occ+1)
        do 120,im = 1,nbm,1
        numm1 = zi(amail1+im-1)
        if (numm1 .eq. 0) goto 120
        call jenuno(jexnum(nommai, numm1), nomm1)
        numm2 = zi(amail2+im-1)
        if (numm2 .gt. 0) then
            call jenuno(jexnum(nommai, numm2), nomm2)
            write (ifm,2070) nomm1,zi(afacor-1+im),zr(aorsgt-&
                    1+im), nomm2
            write (ifm,2050) zi(afacex-1+im),zr(aexsgt-1+im)
        else
            write (ifm,2040) nomm1,zi(afacor-1+im),zr(aorsgt-&
                    1+im)
            write (ifm,2050) zi(afacex-1+im),zr(aexsgt-1+im)
        endif
120      continue
        write (ifm,2060) 1.d0,zr(absgt+2*occ),zr(absgt+2*occ+1)
130      continue
140      continue
        ia = 0
        do 160,occ = nbpars + 1,nbpart,1
        ia = ia + 1
        xor = zr(acarc+2*ia) + zr(ararc+ia)*cos(zr(asarc+2*ia))
        yor = zr(acarc+2*ia+1) + zr(ararc+ia)*sin(zr(asarc+2*ia))
        xex = zr(acarc+2*ia) + zr(ararc+ia)*cos(zr(asarc+2*ia+1))
        yex = zr(acarc+2*ia+1) + zr(ararc+ia)*sin(zr(asarc+2*ia+1) )
        call jelira(jexnum(norsgt, occ), 'LONMAX', nbm, k1b)
        call jeveuo(jexnum(norsgt, occ), 'L', aorsgt)
        call jeveuo(jexnum(nexsgt, occ), 'L', aexsgt)
        call jeveuo(jexnum(nmail1, occ), 'L', amail1)
        call jeveuo(jexnum(nmail2, occ), 'L', amail2)
        call jeveuo(jexnum(nfacor, occ), 'L', afacor)
        call jeveuo(jexnum(nfacex, occ), 'L', afacex)
        call jeveuo(jexnum(ncnxor, occ), 'L', acnxor)
        call jelira(jexnum(ncnxor, occ), 'LONMAX', nbsor, k1b)
        write (ifm,2120) occ,nbsor
        write (ifm,2030) zr(asarc+2*ia),xor,yor
        do 150,im = 1,nbm,1
        numm1 = zi(amail1+im-1)
        if (numm1 .eq. 0) goto 150
        call jenuno(jexnum(nommai, numm1), nomm1)
        write (ifm,2040) nomm1,zi(afacor-1+im),zr(aorsgt-1+im)
        write (ifm,2050) zi(afacex-1+im),zr(aexsgt-1+im)
150      continue
        write (ifm,2060) zr(asarc+2*ia+1),xex,yex
160      continue
    endif
!
    2000 format (/,1x,'COURBE DEFINIE A PARTIR DE SEGMENT(S) ET D''ARC(S)')
    2010 format (3x,'COURBE DEFINIE PAR ',i2,' CHEMIN(S)')
    2020 format (5x,'CHEMIN NUMERO ',i2,' DEFINI PAR "DEFI_SEGMENT" EN ',&
     &       i2,' SEGMENT(S)',/,34x,'ABSC_CURV')
    2030 format (1p,7x,'ORIGINE',18x,e12.5,2x,'POINT',1x,e12.5,1x,e12.5)
    2040 format (1p,7x,'MAILLE',1x,a8,1x,'COTE ',i1,3x,e12.5)
    2050 format (1p,7x,16x,'COTE ',i1,3x,e12.5)
    2060 format (1p,7x,'EXTREMITE',16x,e12.5,2x,'POINT',1x,e12.5,1x,e12.5)
    2070 format (1p,7x,'MAILLE',1x,a8,1x,'COTE ',i1,3x,e12.5,5x,'( ',a8,&
     &       ' )')
    2120 format (5x,'CHEMIN NUMERO ',i2,' DEFINI PAR "DEFI_ARC" EN ',i2,&
     &       ' SEGMENT(S)',/,34x,'ABSC_CURV')
!
    call jedema()
end subroutine
