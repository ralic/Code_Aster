subroutine engtce(ific, chamel, typtes, preci, formr)
    implicit none
    include 'jeveux.h'
    include 'asterc/ismaem.h'
    include 'asterc/r8maem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxlgut.h'
    integer :: ific
    character(len=8) :: typtes
    character(len=10) :: preci, formr
    character(len=19) :: chamel
! ----------------------------------------------------------------------
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
!     COMMANDE:  ENGENDRE_TEST
!                TRAITEMENT DES SD CHAM_ELEM
!
! IN  : IFIC   : NUMERO D'UNITE IMPRESSION
! IN  : NOMSTR : NOM D'UNE SD RESULTAT
! IN  : TYPTES : TYPE DU TEST = SOMM_ABS, SOMM
! IN  : PRECI  : PRECISION POUR LE TEST_RESU
! IN  : FORMR  : FORMAT D'IMPRESSION DU CHAMP VALE REEL
! ----------------------------------------------------------------------
!
    integer :: ibid, vali, i, jvale, long, lg1, lg2
    real(kind=8) :: valr
    character(len=3) :: type
    character(len=8) :: k8b
    character(len=80) :: form1, form2
!     ------------------------------------------------------------------
!
    call jemarq()
!
    lg1 = lxlgut( formr )
    lg2 = lxlgut( typtes )
    form1 = '('' TYPE_TEST= '''''//typtes(1:lg2)// ''''', VALE_CALC= '', '&
            //formr(1:lg1)//','' ), '')'
    form2 = '( '' TYPE_TEST= '''''//typtes(1:lg2)// ''''', VALE_CALC_I = '', I9, '' ), '' )'
!
    write(ific,1000)
!
    call jeveuo(chamel//'.CELV', 'L', jvale)
    call jelira(chamel//'.CELV', 'LONMAX', long, k8b)
    call jelira(chamel//'.CELV', 'TYPE', ibid, type)
!
    write(ific,1010) chamel(1:8)
    write(ific,1020) preci
!
    if (type .eq. 'I') then
        if (typtes .eq. 'SOMM_ABS') then
            vali = 0
            do 110 i = 1, long
                vali = vali + abs(zi(jvale+i-1))
110          continue
        else if (typtes .eq. 'SOMM') then
            vali = 0
            do 112 i = 1, long
                vali = vali + zi(jvale+i-1)
112          continue
        else if (typtes .eq. 'MAX') then
            vali = -ismaem()
            do 114 i = 1, long
                vali = max( vali , zi(jvale+i-1) )
114          continue
        else if (typtes .eq. 'MIN') then
            vali = ismaem()
            do 116 i = 1, long
                vali = min( vali , zi(jvale+i-1) )
116          continue
        endif
        if (vali .eq. 0) write(ific,1022)
        write(ific,form2) vali
!
    else if (type .eq. 'R') then
        if (typtes .eq. 'SOMM_ABS') then
            valr = 0.d0
            do 120 i = 1, long
                valr = valr + abs(zr(jvale+i-1))
120          continue
        else if (typtes .eq. 'SOMM') then
            valr = 0.d0
            do 122 i = 1, long
                valr = valr + zr(jvale+i-1)
122          continue
        else if (typtes .eq. 'MAX') then
            valr = -r8maem()
            do 124 i = 1, long
                valr = max( valr , zr(jvale+i-1) )
124          continue
        else if (typtes .eq. 'MIN') then
            valr = r8maem()
            do 126 i = 1, long
                valr = min( valr , zr(jvale+i-1) )
126          continue
        endif
        if (abs(valr) .le. r8prem()) write(ific,1022)
        write(ific,form1) valr
    endif
!
    write(ific,1030)
!
    call jedema()
!
    1000 format ( 'TEST_RESU(CHAM_ELEM= ' )
!
    1010 format ('          _F( CHAM_GD= ',a8,', ' )
!
    1022 format ('              CRITERE= ''ABSOLU'', ')
    1020 format ('              TOLE_MACHINE= ',a10,',')
!
    1030 format ( '          )' )
!
end subroutine
