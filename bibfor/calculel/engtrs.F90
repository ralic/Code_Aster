subroutine engtrs(ific, nomsd, typtes, preci, formr)
    implicit none
#include "jeveux.h"
!
#include "asterc/ismaem.h"
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/wkvect.h"
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
!                TRAITEMENT DES SD RESULTAT
!
! IN  : IFIC   : NUMERO D'UNITE IMPRESSION
! IN  : NOMSD : NOM D'UNE SD RESULTAT
! IN  : TYPTES : TYPE DU TEST = SOMM_ABS, SOMM
! IN  : PRECI  : PRECISION POUR LE TEST_RESU
! ----------------------------------------------------------------------
!
    integer :: ibid, nbordt, vali, jordr, nbnosy, isy, iatach, lg, i, j, iord
    integer :: iret, jvale, long, lg1, lg2
    real(kind=8) :: r8b, valr
    complex(kind=8) :: c16b
    character(len=3) :: type
    character(len=8) :: k8b
    character(len=16) :: nomsym
    character(len=19) :: chextr
    character(len=90) :: form1, form2, form3
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
! --- NUMEROS D'ORDRE
!
    call rsorac(nomsd, 'LONUTI', ibid, r8b, k8b,&
                c16b, r8b, k8b, nbordt, 1,&
                ibid)
    call wkvect('&&ENGTRS.NUME_ORDRE', 'V V I', nbordt, jordr)
    call rsorac(nomsd, 'TOUT_ORDRE', ibid, r8b, k8b,&
                c16b, r8b, k8b, zi(jordr), nbordt,&
                ibid)
!
! --- NOMS SYMBOLIQUES
!
    call jelira(nomsd//'.DESC', 'NOMMAX', nbnosy)
    do 100 isy = 1, nbnosy
        call jenuno(jexnum(nomsd//'.DESC', isy), nomsym)
        call jenonu(jexnom(nomsd//'.DESC', nomsym), ibid)
        call jeveuo(jexnum(nomsd//'.TACH', ibid), 'L', iatach)
        lg = lxlgut( nomsym )
!
        form3 = '('' _F(RESULTAT= '',A8,'', NOM_CHAM= '''''// nomsym(1:lg)&
                //''''', NUME_ORDRE= '',I6,'','')'
!
        do 110 j = 1, nbordt
            iord = zi(jordr+j-1)
            if (zk24(iatach-1+j)(1:1) .ne. ' ') then
                call rsexch(' ', nomsd, nomsym, iord, chextr,&
                            ibid)
!
                call jeexin(chextr//'.VALE', iret)
                if (iret .ne. 0) then
                    call jeveuo(chextr//'.VALE', 'L', jvale)
                    call jelira(chextr//'.VALE', 'LONMAX', long)
                    if (long .eq. 0) goto 110
                    call jelira(chextr//'.VALE', 'TYPE', cval=type)
                    goto 120
                endif
                call jeexin(chextr//'.CELV', iret)
                if (iret .ne. 0) then
                    call jeveuo(chextr//'.CELV', 'L', jvale)
                    call jelira(chextr//'.CELV', 'LONMAX', long)
                    if (long .eq. 0) goto 110
                    call jelira(chextr//'.CELV', 'TYPE', cval=type)
                    goto 120
                endif
                goto 110
120              continue
!
                write(ific,form3) nomsd(1:8), iord
                write(ific,1020) preci
!
                if (type .eq. 'I') then
                    if (typtes .eq. 'SOMM_ABS') then
                        vali = 0
                        do 130 i = 1, long
                            vali = vali + abs(zi(jvale+i-1))
130                      continue
                    else if (typtes .eq. 'SOMM') then
                        vali = 0
                        do 132 i = 1, long
                            vali = vali + zi(jvale+i-1)
132                      continue
                    else if (typtes .eq. 'MAX') then
                        vali = -ismaem()
                        do 134 i = 1, long
                            vali = max( vali , zi(jvale+i-1) )
134                      continue
                    else if (typtes .eq. 'MIN') then
                        vali = ismaem()
                        do 136 i = 1, long
                            vali = min( vali , zi(jvale+i-1) )
136                      continue
                    endif
                    if (vali .eq. 0) write(ific,1010)
                    write(ific,form2) vali
!
                else if (type .eq. 'R') then
                    if (typtes .eq. 'SOMM_ABS') then
                        valr = 0.d0
                        do 140 i = 1, long
                            valr = valr + abs(zr(jvale+i-1))
140                      continue
                    else if (typtes .eq. 'SOMM') then
                        valr = 0.d0
                        do 142 i = 1, long
                            valr = valr + zr(jvale+i-1)
142                      continue
                    else if (typtes .eq. 'MAX') then
                        valr = -r8maem()
                        do 144 i = 1, long
                            valr = max( valr , zr(jvale+i-1) )
144                      continue
                    else if (typtes .eq. 'MIN') then
                        valr = r8maem()
                        do 146 i = 1, long
                            valr = min( valr , zr(jvale+i-1) )
146                      continue
                    endif
                    if (abs(valr) .le. r8prem()) write(ific,1010)
                    write(ific,form1) valr
                endif
!
            endif
110      continue
100  end do
!
    write(ific,1030)
!
    call jedetr('&&ENGTRS.NUME_ORDRE')
!
    call jedema()
!
    1000 format ( 'TEST_RESU(RESU=( ' )
    1010 format ('              CRITERE= ''ABSOLU'', ')
    1020 format ('              TOLE_MACHINE= ',a10,',')
    1030 format ( '          ),)' )
!
end subroutine
