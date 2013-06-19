subroutine aceaba(noma, nomo, lmax, nbarre, nbocc,&
                  mclf, nbtel, ntyele, ivr, ifm,&
                  jdlm)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/acedat.h'
    include 'asterfort/affbar.h'
    include 'asterfort/alcart.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nocart.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: lmax, nbarre, nbocc, nbtel, ifm, jdlm
    integer :: ntyele(*), ivr(*)
    character(len=8) :: noma, nomo
    character(len=*) :: mclf
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT BARRE
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
! IN  : LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
! IN  : NBARRE : NOMBRE DE BARRE DU MODELE
! IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE BARRE
! IN  : NBTEL  : NOMBRE TOTAL D'ELEMENT
! IN  : NTYELE : TABLEAU DES TYPES D'ELEMENTS
! IN  : IVR    : TABLEAU DES INDICES DE VERIFICATION
! IN  : JDLM   : ADRESSE DES MAILLES
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=1) :: k1bid
    character(len=6) :: kioc
    character(len=8) :: k8b, nomu, nommai, fcx
    character(len=16) :: k16b, sec, concep, cmd
    character(len=19) :: cartba, cartbg, cartbf, tabcar
    character(len=24) :: tmpnba, tmpvba, tmpnbg, tmpvbg, tmpgen, nomsec, typca
    character(len=24) :: tmpnbf, tmpvbf, tmpgef, modmai, mlggma, mlgnma
    character(len=16) :: vmessk(2)
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, idw, ier, iisec, ioc, isec, itabl
    integer :: itblp, itbnp, ivect, ixma, j, jcar, jcara
    integer :: jdcba, jdcbaf, jdcbg, jdge, jdgef, jdgm, jdls, jdls2
    integer :: jdme, jdvba, jdvbaf, jdvbg, jexp, jpara, jsect
    integer :: jtab, jvale, k, nbaaff, nbcar, nbcolo, nblign
    integer :: nbmagr, nbmail, nbo, nbval, ncar, ndim, nfcx
    integer :: ng, nm, nnosec, nsec, ntab, ntypse, nummai
    integer :: nutyel, nval
!-----------------------------------------------------------------------
    call jemarq()
    call getres(nomu, concep, cmd)
!
    call wkvect('&&ACEABA.TAB_PARA', 'V V I', 10, jpara)
    call acedat('BARRE', 0, zi(jpara), k16b, k8b,&
                k8b, k8b)
    ntypse = zi(jpara+1)
    nbo = zi(jpara+2)
    nbcar = zi(jpara+3)
    nbval = zi(jpara+4)
    ndim = zi(jpara+6) * ntypse
    call wkvect('&&ACEABA.TYP_SECT', 'V V K16', ntypse, jsect)
    call wkvect('&&ACEABA.EXPBAR', 'V V K8 ', nbo, jexp)
    call wkvect('&&ACEABA.TABBAR', 'V V K8 ', nbo, jtab)
    call wkvect('&&ACEABA.CARBAR', 'V V K8 ', ndim, jcar)
    call acedat('BARRE', 1, zi(jpara), zk16(jsect), zk8(jexp),&
                zk8(jtab), zk8(jcar))
    call wkvect('&&ACEABA.CARA', 'V V K8', nbcar, jcara)
    call wkvect('&&ACEABA.VALE', 'V V R8', nbval, jvale)
!
    modmai = nomo//'.MAILLE'
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
    ier = 0
    call jelira(mlgnma, 'NOMMAX', nbmail, k1bid)
    call jeexin(modmai, ixma)
    if (ixma .ne. 0) call jeveuo(modmai, 'L', jdme)
!
! --- CONSTRUCTION DES CARTES
    tmpgen = nomu//'.BARRE'
    cartba = nomu//'.CARGENBA'
    cartbg = nomu//'.CARGEOBA'
    tmpnba = cartba//'.NCMP'
    tmpvba = cartba//'.VALV'
    tmpnbg = cartbg//'.NCMP'
    tmpvbg = cartbg//'.VALV'
!
    tmpgef = nomu//'.VENT'
    cartbf = nomu//'.CVENTCXF'
    tmpnbf = cartbf//'.NCMP'
    tmpvbf = cartbf//'.VALV'
!
! --- CREATION D UN OBJET TAMPON (SURDIMENSIONNE A NBO*NBARRE)  :
    call jecrec(tmpgen, 'V V R', 'NO', 'CONTIG', 'CONSTANT',&
                nbarre)
    call jeecra(tmpgen, 'LONMAX', nbo, ' ')
    call jecrec(tmpgef, 'V V K8', 'NO', 'CONTIG', 'CONSTANT',&
                nbarre)
    call jeecra(tmpgef, 'LONMAX', 1, ' ')
    call wkvect('&&ACEABA.BARRE', 'V V K24', lmax, jdls)
    call wkvect('&&ACEABA.BARRE2', 'V V K8', lmax, jdls2)
!
! --- LECTURE ET STOCKAGE DES DONNEES  DANS L OBJET TAMPON
    do 10 ioc = 1, nbocc
        call codent(ioc, 'G', kioc)
        call getvem(noma, 'GROUP_MA', 'BARRE', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', 'BARRE', 'MAILLE', ioc,&
                    iarg, lmax, zk8( jdls2), nm)
        call getvtx('BARRE', 'SECTION', ioc, iarg, 1,&
                    sec, nsec)
        call getvid('BARRE', 'TABLE_CARA', ioc, iarg, 1,&
                    tabcar, ntab)
        if (ntab .eq. 1) then
            call getvtx('BARRE', 'NOM_SEC', ioc, iarg, 1,&
                        nomsec, nnosec)
            call assert(nnosec.eq.1)
            call jeveuo(tabcar//'.TBNP', 'L', itbnp)
!            NOMBRE DE CARACTERISTIQUES
            nbcolo = zi(itbnp)
!            ON RECHERCHE NOMSEC DANS LA 1ER COLONNE
            call jeveuo(tabcar//'.TBLP', 'L', itblp)
            typca=zk24(itblp+1)
            if (typca(1:2) .ne. 'K8' .and. typca(1:3) .ne. 'K24') then
                call u2mesk('F', 'MODELISA8_17', 1, tabcar)
            endif
            call jeveuo(zk24(itblp+2), 'L', itabl)
            nblign = zi(itbnp+1)
            if (typca .eq. 'K8') then
                do 95 i = 1, nblign
                    if (zk8(itabl-1+i) .eq. nomsec) then
                        iisec=i
                        goto 97
                    endif
95              continue
            else
                do 94 i = 1, nblign
                    if (zk24(itabl-1+i)(1:8) .eq. nomsec) then
                        iisec=i
                        goto 97
                    endif
94              continue
            endif
            vmessk(1)=tabcar
            vmessk(2)=nomsec
            call u2mesk('F', 'MODELISA8_18', 2, vmessk)
97          continue
!
            do 96 i = 1, nbcolo-1
                if (zk24(itblp+4*i+1) .ne. 'R') goto 96
                if (zk24(itblp+4*i) .ne. 'A') then
                    goto 96
                else
                    zk8(jcara) = zk24(itblp+4*i)
                    call jeveuo(zk24(itblp+4*i+2), 'L', ivect)
                    zr(jvale)=zr(ivect-1+iisec)
                    goto 98
                endif
96          continue
98          continue
        else
            call getvtx('BARRE', 'CARA', ioc, iarg, nbcar,&
                        zk8(jcara), ncar)
            call getvr8('BARRE', 'VALE', ioc, iarg, nbval,&
                        zr(jvale), nval)
            call assert(ncar.gt.0)
        endif
        fcx = '.'
        call getvid('BARRE', 'FCX', ioc, iarg, 1,&
                    fcx, nfcx)
!
        if (sec .eq. zk16(jsect )) isec = 0
        if (sec .eq. zk16(jsect+1)) isec = 1
        if (sec .eq. zk16(jsect+2)) isec = 2
!
! ---    "GROUP_MA" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DES
!                                                    GROUPES DE MAILLES
        if (ng .gt. 0) then
            do 40 i = 1, ng
                call jeveuo(jexnom(mlggma, zk24(jdls+i-1)), 'L', jdgm)
                call jelira(jexnom(mlggma, zk24(jdls+i-1)), 'LONUTI', nbmagr, k1bid)
                do 42 j = 1, nbmagr
                    nummai = zi(jdgm+j-1)
                    call jenuno(jexnum(mlgnma, nummai), nommai)
                    nutyel = zi(jdme+nummai-1)
                    do 44 k = 1, nbtel
                        if (nutyel .eq. ntyele(k)) then
                            call affbar(tmpgen, tmpgef, fcx, nommai, isec,&
                                        zk8(jcara), zr(jvale), zk8(jexp), nbo, kioc,&
                                        ier)
                            goto 42
                        endif
44                  continue
                    vmessk(1) = mclf
                    vmessk(2) = nommai
                    call u2mesk('F', 'MODELISA_8', 2, vmessk)
42              continue
40          continue
        endif
!
! ---    "MAILLE" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DE MAILLES
        if (nm .gt. 0) then
            do 50 i = 1, nm
                nommai = zk8(jdls2+i-1)
                call jenonu(jexnom(mlgnma, nommai), nummai)
                nutyel = zi(jdme+nummai-1)
                do 52 j = 1, nbtel
                    if (nutyel .eq. ntyele(j)) then
                        call affbar(tmpgen, tmpgef, fcx, nommai, isec,&
                                    zk8(jcara), zr(jvale), zk8(jexp), nbo, kioc,&
                                    ier)
                        goto 50
                    endif
52              continue
                vmessk(1) = mclf
                vmessk(2) = nommai
                call u2mesk('F', 'MODELISA_8', 2, vmessk)
50          continue
        endif
!
10  end do
    if (ier .ne. 0) then
        call u2mess('F', 'MODELISA_7')
    endif
!
    call jelira(tmpgen, 'NUTIOC', nbaaff, k1bid)
!
! --- IMPRESSION DES VALEURS AFFECTEES DANS LE TAMPON SI DEMANDE
    if (ivr(3) .eq. 1) then
! ---    IMPRESSION DES DONNEES GENERALES
        write(ifm,2000)
        do 64 i = 1, nbaaff
            call jenuno(jexnum(tmpgen, i), nommai)
            call jeveuo(jexnum(tmpgen, i), 'L', jdge)
            isec = nint(zr(jdge+nbo-1))
            write(ifm,2001)nommai,zr(jdge),isec
64      continue
! ---    IMPRESSION DES DONNEES GEOMETRIQUES
        idw = 0
        do 66 i = 1, nbaaff
            call jenuno(jexnum(tmpgen, i), nommai)
            call jeveuo(jexnum(tmpgen, i), 'L', jdge)
            isec = nint(zr(jdge+nbo-1))
            if (isec .eq. 1) then
                if (idw .eq. 0) then
                    write(ifm,2010)
                    idw = 1
                endif
                write(ifm,2012)nommai,(zr(jdge+j-1),j=2,5),isec
            else if (isec.eq.2) then
                if (idw .eq. 0) then
                    write(ifm,2020)
                    idw = 1
                endif
                write(ifm,2022)nommai,(zr(jdge+j-1),j=6,7),isec
            endif
            call jenuno(jexnum(tmpgef, i), nommai)
            call jeveuo(jexnum(tmpgef, i), 'L', jdgef)
            write(ifm,*) 'CX : ', zk8(jdgef)
66      continue
    endif
    2000  format(/,3x,&
     &  '<SECTION> VALEURS DE TYPE GENERALE AFFECTEES AUX BARRES'&
     &  ,//,3x,'MAILLE   A              TSEC')
    2001  format(3x,a8,1x,1pd12.5,1x,i6)
    2010  format(/,3x,&
     &  '<SECTION> VALEURS DE TYPE GEOMETRIQUE AFFECTEES AUX BARRES'&
     &  ,//,3x,'MAILLE   HY          HZ          EPY         EPZ',&
     &                  '            TSEC')
    2012  format(3x,a8,1x,4(1pd12.5,1x),i6)
    2020  format(/,3x,&
     &  '<SECTION> VALEURS DE TYPE GEOMETRIQUE AFFECTEES AUX BARRES'&
     &  ,//,3x,'MAILLE   R           EP             TSEC')
    2022  format(3x,a8,1x,2(1pd12.5,1x),i6)
!
! --- ALLOCATION DES CARTES
    call alcart('G', cartba, noma, 'CAGNBA')
    call alcart('G', cartbg, noma, 'CAGEBA')
    call jeveuo(tmpnba, 'E', jdcba)
    call jeveuo(tmpvba, 'E', jdvba)
    call jeveuo(tmpnbg, 'E', jdcbg)
    call jeveuo(tmpvbg, 'E', jdvbg)
    call jeveuo(tmpnbf, 'E', jdcbaf)
    call jeveuo(tmpvbf, 'E', jdvbaf)
!
! --- AFFECTATIONS DES DONNEES GENERALES
    zk8(jdcba) = zk8(jtab)
!     POUR LA CARTE DE VENT ==> FCXP
    zk8(jdcbaf) = 'FCXP'
    do 70 i = 1, nbaaff
        call jenuno(jexnum(tmpgen, i), nommai)
        call jenonu(jexnom(mlgnma, nommai), nummai)
        zi(jdlm+nummai-1) = -1
        call jeveuo(jexnum(tmpgen, i), 'L', jdge)
        zr(jdvba) = zr(jdge)
        call jeveuo(jexnum(tmpgef, i), 'L', jdgef)
        zk8(jdvbaf) = zk8(jdgef)
        call nocart(cartba, 3, ' ', 'NOM', 1,&
                    nommai, 0, ' ', 1)
        call nocart(cartbf, 3, ' ', 'NOM', 1,&
                    nommai, 0, ' ', 1)
70  end do
!
! --- AFFECTATIONS DONNEES GEOMETRIQUES (ON AFFECTE TOUTES LES CMPS)
    do 74 i = 1, 6
        zk8(jdcbg+i-1) = zk8(jtab+i)
74  end do
    do 76 j = 1, nbaaff
        call jenuno(jexnum(tmpgen, j), nommai)
        call jeveuo(jexnum(tmpgen, j), 'L', jdge)
        isec = nint(zr(jdge+nbo-1))
        if (isec .eq. 0) then
! ---       GENERALE
            do 78 i = 1, 6
                zr (jdvbg+i-1) = 0.d0
78          continue
            call nocart(cartbg, 3, ' ', 'NOM', 1,&
                        nommai, 0, ' ', 6)
        else
! ---       RECTANGLE OU CERCLE
            do 80 i = 1, 6
                zr (jdvbg+i-1) = zr(jdge+i)
80          continue
            call nocart(cartbg, 3, ' ', 'NOM', 1,&
                        nommai, 0, ' ', 6)
        endif
76  end do
!
! --- COMPACTAGE DES CARTES
!
! --- NETTOYAGE
    call jedetr('&&ACEABA.BARRE')
    call jedetr('&&ACEABA.BARRE2')
    call jedetr('&&ACEABA.TAB_PARA')
    call jedetr('&&ACEABA.TYP_SECT')
    call jedetr('&&ACEABA.EXPBAR')
    call jedetr('&&ACEABA.TABBAR')
    call jedetr('&&ACEABA.CARBAR')
    call jedetr('&&ACEABA.CARA')
    call jedetr('&&ACEABA.VALE')
    call jedetr(tmpgen)
    call jedetr(tmpgef)
    call jedetr(tmpnba)
    call jedetr(tmpvba)
    call jedetr(tmpnbg)
    call jedetr(tmpvbg)
!
    call jedema()
end subroutine
