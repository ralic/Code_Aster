subroutine irgene(iocc, resu, form, ifi, nbnosy,&
                  nosy, nbcmpg, cmpg, nbpara, para,&
                  nbordr, ordr, nbdisc, disc, nume,&
                  lhist)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/dismoi.h"
#include "asterfort/irpara.h"
#include "asterfort/irparb.h"
#include "asterfort/irvgen.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/titre2.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: cmpg(*), ordr(*), nume(*)
    real(kind=8) :: disc(*)
    character(len=*) :: resu, nosy(*), para(*), form
    aster_logical :: lhist
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
!
!     IMPRESSION D'UN CONCEPT GENERALISE
!     ------------------------------------------------------------------
    character(len=1) :: cecr
    character(len=16) :: typcon
    character(len=19) :: gene, noch19
    character(len=24) :: nomst, nuddl, basemo
    aster_logical :: lordr
    integer :: iocc, ifi, nbnosy, nbcmpg, nbpara, nbordr, i, im, iord, ibid
    integer :: iret, isy, itresu, jordr, jpara, jtitr, kdesc
    integer :: krefe, kvale, nbmode, nbtitr, npara, itcal, nbdisc
    integer, pointer :: desc(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
    nomst = '&&IRGENE.SOUS_TITRE.TITR'
!
!     --- QUEL TYPE DE CONCEPT  ---
!
    call gettco(resu, typcon)
!
!=======================================================================
!
!               --- IMPRESSION D'UNE SD VECT_ASSE_GENE ---
!
!=======================================================================
    if (typcon .eq. 'VECT_ASSE_GENE') then
!
        call irvgen(resu, ifi, nbcmpg, cmpg, lhist)
!
!=======================================================================
!
!         --- IMPRESSION D'UNE SD MODE_GENE ---
!
!=======================================================================
    else if (typcon .eq. 'MODE_GENE') then
        call irparb(resu, nbpara, para, '&&IRGENE.PARAMETRE', npara)
        call jeexin('&&IRGENE.PARAMETRE', iret)
        if (iret .gt. 0) then
            call jeveuo('&&IRGENE.PARAMETRE', 'E', jpara)
        else
            jpara=1
        endif
!
        cecr = 'L'
        do iord = 1, nbordr
            write(ifi,2000)
            call irpara(resu, form, ifi, 1, ordr(iord),&
                        npara, zk16( jpara), cecr)
            do isy = 1, nbnosy
                call rsexch(' ', resu, nosy(isy), ordr(iord), noch19,&
                            iret)
                if (iret .eq. 0) then
                    call titre2(resu, noch19, nomst, 'GENE', iocc,&
                                '(1PE12.5)')
                    write(ifi,2010)
                    call jeveuo(nomst, 'L', jtitr)
                    call jelira(nomst, 'LONMAX', nbtitr)
                    write(ifi,'(1X,A)') (zk80(jtitr+i-1),i=1,nbtitr)
                    call irvgen(noch19, ifi, nbcmpg, cmpg, lhist)
                endif
            end do
        end do
        call jedetr('&&IRGENE.PARAMETRE')
        call jeexin(nomst, iret)
        if (iret .ne. 0) call jedetr(nomst)
!
!=======================================================================
!
!             --- IMPRESSION D'UNE SD DYNA_GENE ---
!
!=======================================================================
        elseif ( ( typcon .eq. 'TRAN_GENE' ) .or. (typcon .eq.&
    'HARM_GENE') ) then
        gene = resu
        lordr = .false.
        call jeexin(gene//'.ORDR', iret)
        if (iret .ne. 0) then
            call jeveuo(gene//'.ORDR', 'L', jordr)
            lordr = .true.
        endif
        call jeveuo(gene//'.DESC', 'L', vi=desc)
        nbmode = desc(2)
        noch19 = '&&IRGENE_VECTEUR'
        call wkvect(noch19//'.DESC', 'V V I', 2, kdesc)
        call wkvect(noch19//'.REFE', 'V V K24', 2, krefe)
!
        if (desc(1) .eq. 4) then
            itcal = 1
        else
            itcal = 0
        endif
!
        if (itcal .eq. 1) then
!        --- CAS D'UNE SD HARM_GENE => VALEURS COMPLEXES
            call wkvect(noch19//'.VALE', 'V V C', nbmode, kvale)
        else
            call wkvect(noch19//'.VALE', 'V V R', nbmode, kvale)
        endif
!
        zi(kdesc+1) = nbmode
!
        call dismoi('NUME_DDL', gene(1:8), 'RESU_DYNA', repk=nuddl)
        call jeexin(nuddl(1:14)//'.NUME.DESC', iret)
        call dismoi('BASE_MODALE', gene(1:8), 'RESU_DYNA', repk=basemo, arret='C',&
                    ier=ibid)
!
!       -- TEST POUR LE CAS DE LA SOUS-STRUCTURATION : EXISTENCE DE NUME_DDL_GENE  --
        if ((iret .eq. 0)) nuddl = ' '
        zk24(krefe) = basemo
        zk24(krefe+1) = nuddl
!
        do i = 1, nbdisc
            iord = nume(i)
            write(ifi,2000)
            do isy = 1, nbnosy
                call jeexin(gene//'.'//nosy(isy)(1:4), iret)
                if (iret .eq. 0) goto 210
                write(ifi,2010)
                write(ifi,3010) nosy(isy)
                if (lordr) then
                    if (itcal .eq. 1) then
                        write(ifi,3021) zi(jordr+iord-1), disc(i)
                    else
                        write(ifi,3020) zi(jordr+iord-1), disc(i)
                    endif
                else
                    if (itcal .eq. 1) then
                        write(ifi,3021) iord, disc(i)
                    else
                        write(ifi,3020) iord, disc(i)
                    endif
                endif
                call jeveuo(gene//'.'//nosy(isy)(1:4), 'L', itresu)
                do im = 0, nbmode-1
                    if (itcal .eq. 1) then
                        zc(kvale+im) = zc(itresu+(iord-1)*nbmode+im)
                    else
                        zr(kvale+im) = zr(itresu+(iord-1)*nbmode+im)
                    endif
                end do
                call irvgen(noch19, ifi, nbcmpg, cmpg, lhist)
210             continue
            end do
        end do
        call jedetr(noch19//'.DESC')
        call jedetr(noch19//'.REFE')
        call jedetr(noch19//'.VALE')
!
    else
        call utmess('F', 'PREPOST2_51', sk=typcon)
    endif
!
!
    2000 format(/,1x,'======>')
    2010 format(/,1x,'------>')
    3010 format(' VECTEUR GENERALISE DE NOM SYMBOLIQUE  ',a)
    3020 format(1p,' NUMERO D''ORDRE: ',i8,' INSTANT: ',d12.5)
    3021 format(1p,' NUMERO D''ORDRE: ',i8,' FREQUENCE: ',d12.5)
!
    call jedema()
end subroutine
