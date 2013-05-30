subroutine engttb(ific, nomsd, typtes, preci, formr)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/ismaem.h'
    include 'asterc/r8maem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/tbexip.h'
    integer :: ific
    character(len=8) :: typtes
    character(len=10) :: preci, formr
    character(len=19) :: nomsd
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     COMMANDE:  ENGENDRE_TEST
!                TRAITEMENT DES SD TABLE
!
! IN  : IFIC   : NUMERO D'UNITE IMPRESSION
! IN  : NOMSD : NOM D'UNE SD RESULTAT
! IN  : TYPTES : TYPE DU TEST = SOMM_ABS, SOMM
! IN  : PRECI  : PRECISION POUR LE TEST_TABLE
! IN  : FORMR  : FORMAT D'IMPRESSION DU CHAMP VALE REEL
! ----------------------------------------------------------------------
!
    integer :: nbpara, nblign, vali, ipar, lg, lg1, lg2, i, jvale, jvall, jtblp
    integer :: jtbnp
    real(kind=8) :: valr
    logical :: exist
    character(len=3) :: type
    character(len=16) :: nomsym
    character(len=90) :: form1, form2, form3
!     ------------------------------------------------------------------
!
    call jemarq()
!
    lg1 = lxlgut( formr )
    lg2 = lxlgut( typtes )
    form1 = '('' TYPE_TEST= '''''//typtes(1:lg2)// ''''', VALE_CALC= '', '&
            //formr(1:lg1)//', '' )'')'
    form2 = '( '' TYPE_TEST= '''''//typtes(1:lg2)// ''''', VALE_CALC_I = '', I9, '' )'' )'
!
    call jeveuo(nomsd//'.TBLP', 'L', jtblp)
    call jeveuo(nomsd//'.TBNP', 'L', jtbnp)
    nbpara = zi(jtbnp )
    nblign = zi(jtbnp+1)
!
    do 400 ipar = 1, nbpara
!
        nomsym = zk24(jtblp+4*(ipar-1))
        call tbexip(nomsd, nomsym, exist, type)
        if (.not. exist) goto 400
!
        lg = lxlgut( nomsym )
        call jeveuo(zk24(jtblp+4*(ipar-1)+2), 'L', jvale)
        call jeveuo(zk24(jtblp+4*(ipar-1)+3), 'L', jvall)
!
        form3 = '( ''TEST_TABLE(TABLE= '',A8,'', NOM_PARA= '''''// nomsym(1:lg )//''''', '' )'
!
        if (type .eq. 'I') then
!             -------------
            write(ific,form3) nomsd(1:8)
            write(ific,4020) preci
!
            if (typtes .eq. 'SOMM_ABS') then
                vali = 0
                do 410 i = 1, nblign
                    if (zi(jvall+i-1) .eq. 1) vali = vali+abs(zi(jvale+ i-1))
410              continue
            else if (typtes .eq. 'SOMM') then
                vali = 0
                do 412 i = 1, nblign
                    if (zi(jvall+i-1) .eq. 1) vali = vali + zi(jvale+i- 1)
412              continue
            else if (typtes .eq. 'MAX') then
                vali = -ismaem()
                do 414 i = 1, nblign
                    if (zi(jvall+i-1) .eq. 1) vali = max(vali,zi(jvale+ i-1))
414              continue
            else if (typtes .eq. 'MIN') then
                vali = ismaem()
                do 416 i = 1, nblign
                    if (zi(jvall+i-1) .eq. 1) vali = min(vali,zi(jvale+ i-1))
416              continue
            endif
            if (vali .eq. 0) write(ific,4010)
            write(ific,form2) vali
!
        else if (type .eq. 'R') then
!                 -------------
            write(ific,form3) nomsd(1:8)
            write(ific,4020) preci
!
            if (typtes .eq. 'SOMM_ABS') then
                valr = 0.d0
                do 420 i = 1, nblign
                    if (zi(jvall+i-1) .eq. 1) valr = valr+abs(zr(jvale+ i-1))
420              continue
            else if (typtes .eq. 'SOMM') then
                valr = 0.d0
                do 422 i = 1, nblign
                    if (zi(jvall+i-1) .eq. 1) valr = valr + zr(jvale+i- 1)
422              continue
            else if (typtes .eq. 'MAX') then
                valr = -r8maem()
                do 424 i = 1, nblign
                    if (zi(jvall+i-1) .eq. 1) valr = max(valr,zr(jvale+ i-1))
424              continue
            else if (typtes .eq. 'MIN') then
                valr = r8maem()
                do 426 i = 1, nblign
                    if (zi(jvall+i-1) .eq. 1) valr = min(valr,zr(jvale+ i-1))
426              continue
            endif
            if (abs(valr) .le. r8prem()) write(ific,4010)
            write(ific,form1) valr
        endif
400  end do
!
    call jedema()
!
    4010 format ('            CRITERE= ''ABSOLU'', ')
!
    4020 format ('            TOLE_MACHINE= ',a10,',')
!
end subroutine
