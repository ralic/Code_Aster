subroutine tbexlr(nomta, listr, basout)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: nomta, listr, basout
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
!     TRANSFORMER UNE TABLE EN LISTR8 QUE SI CETTE TABLE EST
!                 "DIAGONALISABLE PAR BLOCS"
! ----------------------------------------------------------------------
! IN  : NOMTA  : NOM DE LA SD "TABLE".
! IN  : LISTR  : NOM DE LA SD "LISTR8" RESULTAT
! IN  : BASOUT : BASE DE CREATION DE "LISTR"
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: iret, nbpara, nblign, jtbnp, kpara, nbpr, nblg, ipar, ndim, jtblp
    integer :: i, j, jvale, jlogq, klign, nbvale, k1, kcol, klig, ideb1, ideb2
    integer :: ifin1, ifin2, nbcl, ivide, ilig, ibloc, jpas, jnbp, jbor, k, jcol
    integer :: jlig, klis, kcol1, kcol2
    character(len=1) :: base
    character(len=3) :: type
    character(len=19) :: nomtab, listr8
    character(len=24) :: nomjv, nomjvl
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = ' '
    nomtab = nomta
    call jeexin(nomtab//'.TBBA', iret)
    if (iret .eq. 0) then
        call u2mess('F', 'UTILITAI4_64')
    endif
    if (nomtab(18:19) .ne. '  ') then
        call u2mess('F', 'UTILITAI4_68')
    endif
    base = basout(1:1)
!
    call jeveuo(nomtab//'.TBNP', 'E', jtbnp)
    nbpara = zi(jtbnp )
    nblign = zi(jtbnp+1)
    if (nbpara .eq. 0) then
        call u2mess('F', 'UTILITAI4_65')
    endif
    if (nblign .eq. 0) then
        call u2mess('F', 'UTILITAI4_76')
    endif
!
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
!
!     --- ON NE RETIENT QUE LES PARAMETRES DE TYPE "I" ET "R" ---
!
    call wkvect('&&TBEXLR.NUME_PARA', 'V V I', nbpara, kpara)
    nbpr = 0
    do 10 i = 1, nbpara
        type = zk24(jtblp+4*(i-1)+1)
        if (type(1:1) .eq. 'I') then
            nbpr = nbpr + 1
            zi(kpara+nbpr-1) = i
        else if (type(1:1) .eq. 'R') then
            nbpr = nbpr + 1
            zi(kpara+nbpr-1) = i
        endif
10  end do
    if (nbpr .eq. 0) then
        call u2mess('F', 'UTILITAI4_81')
    endif
!
!     --- ON NE RETIENT QUE LES LIGNES NON VIDES ---
!
    call wkvect('&&TBEXLR.NUME_LIGN', 'V V I', nblign, klign)
    nblg = 0
    do 20 i = 1, nblign
        nbcl = 0
        do 22 j = 1, nbpr
            ipar = zi(kpara+j-1)
            nomjvl = zk24(jtblp+4*(ipar-1)+3)
            call jeveuo(nomjvl, 'L', jlogq)
            if (zi(jlogq+i-1) .eq. 1) nbcl = nbcl + 1
22      continue
        if (nbcl .ne. 0) then
            nblg = nblg + 1
            zi(klign+nblg-1) = i
        endif
20  end do
    if (nblg .eq. 0) then
        call u2mess('F', 'UTILITAI4_82')
    endif
!
!     --- RECHERCHE DE BLOCS ---
!
    nbvale = nbpr * nblg
    call wkvect('&&TBEXLR.VALE_R', 'V V R', nbvale, klis)
    call wkvect('&&TBEXLR.COLONN', 'V V I', nbpr, jcol)
    call wkvect('&&TBEXLR.LIGNES', 'V V I', nblg, jlig)
!
    ibloc = 1
    k1 = 0
    do 30 i = 1, nblg
        ilig = zi(klign+i-1)
        ideb1 = 0
        ifin1 = nbpr
        ivide = 0
        kcol1 = 0
        do 32 j = 1, nbpr
            ipar = zi(kpara+j-1)
            type = zk24(jtblp+4*(ipar-1)+1)
            nomjv = zk24(jtblp+4*(ipar-1)+2)
            nomjvl = zk24(jtblp+4*(ipar-1)+3)
            call jeveuo(nomjv, 'L', jvale)
            call jeveuo(nomjvl, 'L', jlogq)
            if (zi(jlogq+ilig-1) .eq. 1) then
                if (ideb1 .eq. 0) ideb1 = ipar
                kcol1 = kcol1 + 1
                if (ivide .eq. 1) then
                    call u2mess('F', 'UTILITAI4_83')
                endif
                if (type(1:1) .eq. 'I') then
                    k1 = k1 + 1
                    zr(klis+k1-1) = zi(jvale+ilig-1)
                else if (type(1:1) .eq. 'R') then
                    k1 = k1 + 1
                    zr(klis+k1-1) = zr(jvale+ilig-1)
                endif
            else
                if (ideb1 .ne. 0) then
                    ivide = 1
                    if (ifin1 .eq. nbpr) ifin1 = zi(kpara+j-1-1)
                endif
!               IF ( IFIN1 .EQ. 0 ) IFIN1 = ZI(KPARA+J-1-1)
            endif
32      continue
        if (i .eq. 1) then
            klig = 1
        else
            if (ideb1 .eq. ideb2 .and. ifin1 .eq. ifin2) then
                klig = klig + 1
            else
!              --- NOUVEAU BLOC ---
                zi(jcol+ibloc-1) = kcol2
                zi(jlig+ibloc-1) = klig
                ibloc = ibloc + 1
                klig = 1
            endif
        endif
        ideb2 = ideb1
        ifin2 = ifin1
        kcol2 = kcol1
30  end do
    zi(jcol+ibloc-1) = kcol2
    zi(jlig+ibloc-1) = klig
!
!     --- ON STOCKE ---
!
    nbvale = 1 + ( 2 * ibloc )
    do 40 i = 1, ibloc
        kcol = zi(jcol+i-1)
        klig = zi(jlig+i-1)
        nbvale = nbvale + ( kcol * klig )
40  end do
!
    listr8 = listr
    ndim = max( 1 , nbvale-1 )
    call wkvect(listr8//'.LPAS', base//' V R', ndim, jpas)
    call wkvect(listr8//'.NBPA', base//' V I', ndim, jnbp)
    call wkvect(listr8//'.BINT', base//' V R', nbvale, jbor)
    call wkvect(listr8//'.VALE', base//' V R', nbvale, jvale)
!
    zr(jvale) = ibloc
    j = 1
    k1 = 0
    do 50 i = 1, ibloc
        kcol = zi(jcol+i-1)
        j = j + 1
        zr(jvale+j-1) = kcol
        klig = zi(jlig+i-1)
        j = j + 1
        zr(jvale+j-1) = klig
        ndim = kcol * klig
        do 52 k = 1, ndim
            k1 = k1 + 1
            j = j + 1
            zr(jvale+j-1) = zr(klis+k1-1)
52      continue
50  end do
!
    do 4 i = 1, nbvale-1
        zr(jpas+i-1) = zr(jvale+i) - zr(jvale+i-1)
        zi(jnbp+i-1) = 1
        zr(jbor+i-1) = zr(jvale+i-1)
 4  end do
    zr(jbor+nbvale-1) = zr(jvale+nbvale-1)
!
    call jedetr('&&TBEXLR.NUME_PARA')
    call jedetr('&&TBEXLR.NUME_LIGN')
    call jedetr('&&TBEXLR.VALE_R')
    call jedetr('&&TBEXLR.COLONN')
    call jedetr('&&TBEXLR.LIGNES')
!
    call jedema()
end subroutine
