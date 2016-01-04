subroutine rvaffe(mcf, iocc, sdlieu, sdeval, sdmail,&
                  typaff, quant, option, rep, nomtab,&
                  xnovar, ncheff, i1, isd)
    implicit   none
#include "jeveux.h"
!
#include "asterc/r8vide.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rvinfa.h"
#include "asterfort/rvtec0.h"
#include "asterfort/rvtecn.h"
#include "asterfort/wkvect.h"
    integer :: iocc, isd
    character(len=1) :: typaff
    character(len=16) :: ncheff
    character(len=19) :: sdeval, nomtab
    character(len=24) :: sdlieu, sdmail, xnovar
    character(len=*) :: mcf, rep, option, quant
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     AFFICHAGE EXTRACTION
!     ------------------------------------------------------------------
! IN  SDLIEU : K : SD DU LIEU TRAITEE
! IN  SDEVAL : K : SD DE L' EVALUATION DE LA QUANTITE SUR CE LIEU
! IN  SDMAIL : K : SD DES NOMS  MAILLES ACTIVES PAR NOEUD (CAS 'LSTN')
! IN  TYPAFF : K : 'N' --> PAR NOEUD, 'E' --> PAR ELEM
! IN  QUANT  : K : NOM DE LA QUANTITE TRAITEE
! IN  OPTION : K : NOM DE L' OPTION   TRAITEE
!     ------------------------------------------------------------------
    integer :: avale, apnbn, apadr, anocp, nbcp, ioc, aabsc, nbpt, nboc
    integer :: apnsp, apnco, acoor, nbco, nbsp, apnca, apnsa, i1
    integer :: i, deb, fin, adr1, ifm, anomnd, j, k, adri, deci, nbni, avaux
    integer :: l, lci, ln, lni, nbsi, niv
    integer :: lll, indic, indi1, indi2
    real(kind=8) ::  ax, s1, s2
    character(len=4) :: docul, docu
    character(len=16) :: oper
    character(len=24) :: nvale, npnbn, npadr, nabsc, nnocp
    character(len=24) :: npnca, npnsa, nvaux, npnco, npnsp
!======================================================================
    call jemarq()
    call infniv(ifm, niv)
    oper = 'EXTRACTION'
    if (niv .gt. 1) call rvinfa(ifm, mcf, iocc, quant, option,&
                                oper, rep(1:1))
    nvale = sdeval//'.VALE'
    npnbn = sdeval//'.PNBN'
    nnocp = sdeval//'.NOCP'
    npadr = sdeval//'.PADR'
    npnco = sdeval//'.PNCO'
    npnsp = sdeval//'.PNSP'
    nvaux = '&&RVAFFE.VECT.INTER.R'
    npnca = '&&RVAFFE.VECT.INTER.C'
    npnsa = '&&RVAFFE.VECT.INTER.S'
    nabsc = sdlieu(1:19)//'.ABSC'
    call jelira(sdlieu(1:19)//'.REFE', 'DOCU', cval=docul)
    call jeveuo(sdlieu(1:19)//'.DESC', 'L', anomnd)
    call jelira(nvale, 'DOCU', cval=docu)
    call jelira(nabsc, 'NMAXOC', nboc)
    call jelira(nnocp, 'LONMAX', nbcp)
    call jeveuo(nvale, 'L', avale)
    call jeveuo(npadr, 'L', apadr)
    call jeveuo(npnbn, 'L', apnbn)
    call jeveuo(npnsp, 'L', apnsp)
    call jeveuo(npnco, 'L', apnco)
    call jeveuo(nnocp, 'L', anocp)
    fin = 0
    do 100, ioc = 1, nboc, 1
    call jelira(jexnum(nabsc , ioc), 'LONMAX', nbpt)
    call jeveuo(jexnum(nabsc , ioc), 'L', aabsc)
    call jeveuo(jexnum(sdlieu(1:19)//'.COOR' , ioc), 'L', acoor)
    s1 = zr(aabsc + 1-1)
    s2 = zr(aabsc + nbpt-1)
    if (niv .gt. 1) then
        if (docul .eq. 'LSTN') then
            write(ifm,*)'CHEMIN DE NOEUDS'
        endif
    endif
    deb = fin + 1
    fin = deb + nbpt
    if (docu .eq. 'CHLM') then
        fin = fin - 2
    else if (docu .eq. 'CHNO') then
        fin = fin - 1
    endif
    adr1 = zi(apadr + deb-1)
    nbco = zi(apnco + deb-1)
    nbsp = zi(apnsp + deb-1)
    if (docu .eq. 'CHNO') then
        call rvtecn(zr(avale + adr1-1), zr(aabsc), zi(apnco + deb- 1), zi(apnsp + deb-1),&
                    zr(acoor), zk8(anocp), zk8(anomnd), nbcp, nbpt,&
                    docul, nomtab, iocc, xnovar, ncheff,&
                    i1, ioc, isd)
    else
        if (typaff .eq. 'E') then
            call rvtec0(zr(avale + adr1-1), zi(apnco+deb-1), zi(apnsp+deb-1), zr(aabsc),&
                        zr(acoor), zk8(anocp), zk8(anomnd), sdmail, nbpt,&
                        docul, nbcp, zi(apadr), nomtab,&
                        iocc, xnovar, ncheff, i1)
        else
            call wkvect(nvaux, 'V V R', nbco*nbsp*nbpt*nbcp, avaux)
            call wkvect(npnca, 'V V I', nbpt, apnca)
            call wkvect(npnsa, 'V V I', nbpt, apnsa)
            do 105, i = 1, nbpt-1, 1
            zi(apnca + i-1) = zi(apnco + i-1)
            zi(apnsa + i-1) = zi(apnsp + i-1)
105          continue
            if (docul .eq. 'LSTN') then
                zi(apnca + nbpt-1) = zi(apnco + nbpt-1)
                zi(apnsa + nbpt-1) = zi(apnsp + nbpt-1)
            else
                zi(apnca + nbpt-1) = zi(apnco + nbpt-2)
                zi(apnsa + nbpt-1) = zi(apnsp + nbpt-2)
            endif
            ln = nbcp*nbsp
            if (docul .eq. 'LSTN') then
                do 110, i = 1, nbpt, 1
                adri = zi(apadr + i-1)
                nbni = zi(apnbn + i-1)
                nbsi = zi(apnsp + i-1)
                deci = ln*nbco*(i-1)
                lni = nbcp*nbsi
                lci = lni*nbni
                do 120, j = 1, nbco, 1
                do 130, k = 1, nbsi*nbcp, 1
                ax = 0.0d0
                lll = 0
                do 140, l = 1, nbni, 1
                indic = adri-1+(j-1)*lci+(l-1)* lni+k-1
                if (zr(avale+indic) .eq. r8vide()) goto 140
                lll = lll + 1
                ax = ax + zr(avale+indic)
140              continue
                if (lll .eq. 0) then
                    zr(avaux+deci+(j-1)*ln + k-1) =&
                                    0.d0
                else
                    zr(avaux+deci+(j-1)*ln + k-1) =&
                                    ax/lll
                endif
130              continue
120              continue
110              continue
            else
                do 150, i = 1, nbpt, 1
                if (i .eq. 1) then
                    adri = zi(apadr+deb-1)
                    do 160, k = 1, nbco, 1
                    do 161, j = 1, ln, 1
                    indic = adri - 1 + 2*ln*(k-1) + j- 1
                    if (zr(avale+indic) .eq. r8vide()) then
                        zr(avaux+ln*(k-1)+j-1) = 0.d0
                    else
                        zr(avaux+ln*(k-1)+j-1) = zr( avale+indic)
                    endif
161                  continue
160                  continue
                else if (i .eq. nbpt) then
                    adri = zi(apadr +deb-1+ nbpt-2) + ln
                    do 170, k = 1, nbco, 1
                    do 171, j = 1, ln, 1
                    indic = adri - 1 + 2*ln*(k-1) + j- 1
                    if (zr(avale+indic) .eq. r8vide()) then
                        zr(avaux+((nbpt-1)*nbco+k-1)*&
                                        ln+j-1) = 0.d0
                    else
                        zr(avaux+((nbpt-1)*nbco+k-1)*&
                                        ln+j-1) = zr(avale+indic)
                    endif
171                  continue
170                  continue
                else
                    adri = zi(apadr +deb-1+ i-2)
                    do 180, k = 1, nbco, 1
                    do 181, j = 1, ln, 1
                    indi1 = adri-1+ln*(2*k-1)+j-1
                    indi2 = adri-1+ln*2*(k-1+nbco)+j- 1
                    if (zr(avale+indi1) .eq. r8vide() .and. zr(avale+indi2) .eq. r8vide()) then
                        zr(avaux+ln*(nbco*(i-1)+k-1)+&
                                        j-1) = 0.d0
                        elseif(zr(avale+indi1).eq.r8vide()&
                                    ) then
                        zr(avaux+ln*(nbco*(i-1)+k-1)+&
                                        j-1) = zr(avale+indi2)
                        elseif(zr(avale+indi2).eq.r8vide()&
                                    ) then
                        zr(avaux+ln*(nbco*(i-1)+k-1)+&
                                        j-1) = zr(avale+indi1)
                    else
                        zr(avaux+ln*(nbco*(i-1)+k-1)+&
                                        j-1) = 0.5d0*(zr(avale+indi1)+&
                                        zr(avale+indi2))
                    endif
181                  continue
180                  continue
                endif
150              continue
            endif
            call rvtecn(zr(avaux), zr(aabsc), zi(apnca), zi(apnsa), zr(acoor),&
                        zk8(anocp), zk8(anomnd), nbcp, nbpt, docul,&
                        nomtab, iocc, xnovar, ncheff, i1,&
                        ioc, isd)
            call jedetr(nvaux)
            call jedetr(npnca)
            call jedetr(npnsa)
        endif
    endif
    100 end do
    call jedema()
end subroutine
