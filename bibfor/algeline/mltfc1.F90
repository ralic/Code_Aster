subroutine mltfc1(nbloc, ncbloc, decal, supnd, fils,&
                  frere, seq, lgsn, lfront, adress,&
                  local, adpile, nbass, pile, lgpile,&
                  adper, t1, t2, factol, factou,&
                  typsym, ad, eps, ier, nbb,&
                  cl, cu, diag)
!
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     VERSION MODIFIEE POUR L' APPEL A DGEMV (PRODUITS MATRICE-VECTEUR)
!     LE STOCKAGE DES COLONNES DE LA FACTORISEE EST MODIFIE, ET AINSI
!      ADPER LES COLONNES FORMENT UN BLOC RECTANGULAIRE
!
! aslint: disable=W1304,W1504
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mlnflm.h"
#include "asterfort/mlnfmj.h"
#include "asterfort/mltaff.h"
#include "asterfort/mltafp.h"
#include "asterfort/mltf21.h"
#include "asterfort/mltflm.h"
#include "asterfort/mltfmj.h"
    integer :: pmin, nbb
    parameter (pmin=10)
    integer :: nbloc, ncbloc(*), decal(*)
    integer :: lgsn(*), lfront(*), lgpile, typsym
    integer(kind=4) :: local(*)
    integer :: nbass(*), adpile(*), fils(*), supnd(*)
    integer :: adress(*), frere(*), seq(*), ad(*), ier
    real(kind=8) :: pile(*), eps, cl(nbb, nbb, *), cu(nbb, nbb, *), diag(*)
    character(len=24) :: factol, factou
!
    real(kind=8) :: t1(*), t2(*)
    integer :: adper(*), ifacl, ifacu, lmatf
    integer :: itemp, i, j, isnd, sni, sn, n, m, p, nl, nc, mem, adfacl, ib, nb
    integer :: lm1, lm2, adfacu, long, iad, adfac0, adfac
!
    call jemarq()
    itemp = 1
    mem = 0
    isnd = 0
    do 10 i = 1, lgpile
        pile(i) = 0.d0
10  end do
!
    do 70 ib = 1, nbloc
        call jeveuo(jexnum(factol, ib), 'E', ifacl)
        if (typsym .eq. 0) then
            call jeveuo(jexnum(factou, ib), 'E', ifacu)
        endif
        adfac0=ifacl-1
        do 60 nc = 1, ncbloc(ib)
            isnd = isnd + 1
            sni = seq(isnd)
            long=adress(sni+1)-adress(sni)
            iad = supnd(sni)
            p = lgsn(sni)
!
!
            m = lfront(sni)
            n = m + p
            lmatf = (m* (m+1))/2
!
            lm1 = lmatf
            if (typsym .eq. 0) lmatf = 2*lmatf
!         CHANGTPOUR L' APPEL A DGEMV
            do 19 i = 1, p
                adper(i) = (i-1)*n+i
19          continue
            do 20 i = p, n - 1
                adper(i+1) = 1 + (n+ (n-i+1))*i/2
20          continue
            sn = fils(sni)
            do 30 j = 1, lmatf
                pile(itemp+j-1) = 0.d0
30          continue
            adfacl = ifacl - 1 + decal(sni)
            if (typsym .eq. 0) adfacu = ifacu - 1 + decal(sni)
40          continue
!     DO WHILE (SN.NE.0)
            if (sn .ne. 0) then
                nl = lgsn(sn)
                nb = nbass(sn)
                lm2 = (lfront(sn)* (lfront(sn)+1))/2
                call mltafp(lfront(sn), nb, adper, zr(adfacl), pile( adpile(sn)),&
                            local(adress(sn)+nl))
                call mltaff(lfront(sn), nb, adper, pile(itemp), pile( adpile(sn)),&
                            local(adress(sn)+nl), p)
                if (typsym .eq. 0) then
                    call mltafp(lfront(sn), nb, adper, zr(adfacu), pile(adpile(sn)+lm2),&
                                local(adress(sn)+nl))
                    call mltaff(lfront(sn), nb, adper, pile(itemp+lm1), pile(adpile(sn)+lm2),&
                                local(adress(sn)+nl), p)
                endif
                sn = frere(sn)
                goto 40
!     FIN DO WHILE
            endif
            if (p .le. pmin .and. typsym .ne. 0) then
                call mltf21(p, zr(adfacl), pile(itemp), n, t1,&
                            t2, eps, ier)
                if (ier .ne. 0) goto 9999
            else
                if (typsym .eq. 0) then
                    call mlnflm(nbb, n, p, zr(adfacl), zr(adfacu),&
                                adper, t1, t2, ad, eps,&
                                ier, cl, cu)
                    if (ier .ne. 0) goto 9999
                    call mlnfmj(nbb, n, p, zr(adfacl), zr(adfacu),&
                                pile( itemp), pile(itemp+lm1), adper, t1, t2,&
                                cl, cu)
                else
                    call mltflm(nbb, n, p, zr(adfacl), adper,&
                                t1, ad, eps, ier, cl)
                    if (ier .ne. 0) goto 9999
                    call mltfmj(nbb, n, p, zr(adfacl), pile(itemp),&
                                adper, t1, cl)
                endif
            endif
            if (fils(sni) .ne. 0) then
                mem = max(mem, (itemp+lmatf-1))
                do 50 j = 1, lmatf
                    pile(adpile(fils(sni))+j-1) = pile(itemp+j-1)
50              continue
                adpile(sni) = adpile(fils(sni))
                itemp = adpile(sni) + lmatf
            else
                adpile(sni) = itemp
                itemp = itemp + lmatf
            endif
            mem = max(mem,itemp)
!
            do 51,i=1,p
            adfac=adfac0+long*(i-1)+i
            diag(iad+i-1) = zr(adfac)
51          continue
            adfac0=adfac0+long*lgsn(sni)
60      continue
!
        call jelibe(jexnum(factol, ib))
        if (typsym .eq. 0) call jelibe(jexnum(factou, ib))
70  end do
9999  continue
    if (ier .ne. 0) ier =ier +supnd(sni)-1
    call jedema()
end subroutine
