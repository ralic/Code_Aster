subroutine coplas(tempa, k1a, k1b, k1c, matrev, &
                  lrev, deklag, prodef, oridef, profil, &
                  kal, kbl, kcl, dkma, dkmb, &
                  dkmc, k1acp, k1bcp, k1ccp)
!
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    real(kind=8) :: tempa, kal, kbl, kcl, k1a
    real(kind=8) :: k1b, k1c, lrev, deklag, dkma
    real(kind=8) :: dkmb, dkmc, k1acp, k1bcp, k1ccp, prodef
    character(len=8) :: matrev, oridef
    character(len=12) :: profil
! ======================================================================
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : AJOUT DE CORRECTION PLASTIQUE AU CALCUL DES FACTEURS -------
! ------- : D'INTENSITE DE CONTRAINTES ---------------------------------
! ======================================================================
! IN  : TEMPA  : TEMPERATURE EN POINTE A -------------------------------
! --- : K1A    : FACTEUR D'INTENSITE DE CONTRAINTES ELASTIQUE EN A -----
! --- : K1B    : FACTEUR D'INTENSITE DE CONTRAINTES ELASTIQUE EN B -----
! --- : K1C    : FACTEUR D'INTENSITE DE CONTRAINTES ELASTIQUE EN C -----
! --- : MATREV : MATERIAU DE REVETEMENT --------------------------------
! --- : LREV   : LONGUEUR DE REVETEMENT --------------------------------
! --- : DEKLAG : DECALAGE DU DEFAUT COTE REVETEMENT (TOUJOURS NEGATIF) -
! --- : PRODEF : PROFONDEUR DU DEFAUT ----------------------------------
! --- : ORIDEF : ORIENTATION DU DEFAUT ---------------------------------
! --- : PROFIL : TYPE DE PROFIL (ELLIPSE OU SEMI-ELLIPSE) --------------
! VAR : KAL    : FACTEUR DE MARGE EN POINTE A --------------------------
! --- : KBL    : FACTEUR DE MARGE EN POINTE B --------------------------
! --- : KCL    : FACTEUR DE MARGE EN POINTE C --------------------------
! --- : DKMA   : CORRECTION PLASTIQUE EN A -----------------------------
! --- : DKMB   : CORRECTION PLASTIQUE EN B -----------------------------
! --- : DKMC   : CORRECTION PLASTIQUE EN C -----------------------------
! OUT : K1ACP  : FACTEUR D'INTENSITE DE CONTRAINTE AVEC CORRECTION -----
! ------------ : PLASTIQUE EN A ----------------------------------------
! --- : K1BCP  : FACTEUR D'INTENSITE DE CONTRAINTE AVEC CORRECTION -----
! ------------ : PLASTIQUE EN B ----------------------------------------
! --- : K1CCP  : FACTEUR D'INTENSITE DE CONTRAINTE AVEC CORRECTION -----
! ------------ : PLASTIQUE EN C ----------------------------------------
! ======================================================================
! ======================================================================
    integer :: iadr, long, i, j, k, lreel, ineut1, ineut2, ineut3, ineut4
    integer :: ldim, ineut5, ineut6, ineut8, npara, nvale, valp
    integer :: nbpt1, nbpt2, itot1, itot2, itot10, itot11, itot12
    integer :: itot13, itot14, itot15
    real(kind=8) :: sigma, temp1, temp2, sigma1, sigma2, rest, pent
    real(kind=8) :: tempdi, lamb1, lamb2, tempd, coef1, coef2, rya, pi
    real(kind=8) :: betaa, betab, ca, cb, val1, val2
    character(len=8) :: proln
    character(len=16) :: phenom, prolg
    character(len=19) :: valnom, romnom, tranom, fonct
    character(len=24) :: nomcmp, typnom, ty2nom, autnom, vaenom, cocnom, parnom
    character(len=24) :: natnom, pronom
! ======================================================================
    call jemarq()
! ======================================================================
    nomcmp = matrev//'.MATERIAU.NOMRC'
    pi = r8pi()
    call jeveuo(nomcmp, 'L', iadr)
    call jelira(nomcmp, 'LONUTI', long)
    do 3 i = 0, long-1
        phenom = zk16(iadr+i)
        if (phenom .eq. 'ECRO_LINE') then
            valnom = matrev//'.'//phenom
            typnom = valnom//'.VALR'
            ty2nom = valnom//'.VALK'
            call jelira(typnom, 'LONUTI', lreel)
            if (lreel .gt. 0) then
                call jeveuo(typnom, 'L', ineut1)
                call jeveuo(ty2nom, 'L', ineut2)
                do 5 j = 0, lreel-1
                    if (zk8(ineut2+j) .eq. 'SY') then
                        sigma = zr(ineut1+j)
                        goto 30
                    endif
 5              continue
            else
                call jeveuo(ty2nom, 'L', ineut2)
                do 110 j = 0, long-1
                    if (zk8(ineut2+j) .eq. 'SY') then
                        fonct = zk8(ineut2+j+long)
                        autnom = fonct//'.PROL'
                        vaenom = fonct//'.VALE'
                        call jeveuo(autnom, 'L', ineut4)
                        call jeveuo(vaenom, 'L', ineut3)
                        call jelira(vaenom, 'LONUTI', ldim)
                        ldim = ldim / 2
                        if (tempa .lt. zr(ineut3)) then
                            prolg = zk24(ineut4+4)
                            if (prolg(1:1) .eq. 'E') then
                                call utmess('F', 'PREPOST_8')
                            else if (prolg(1:1).eq.'C') then
                                sigma = zr(ineut3+ldim)
                            else if (prolg(1:1).eq.'L') then
                                temp1 = zr(ineut3)
                                temp2 = zr(ineut3+1)
                                sigma1 = zr(ineut3+ldim)
                                sigma2 = zr(ineut3+ldim+1)
                                pent = (sigma2-sigma1)/(temp2-temp1)
                                rest = sigma1 - pent*temp1
                                sigma = pent*tempa + rest
                            endif
                        else if (tempa.gt.zr(ineut3+ldim-1)) then
                            prolg = zk24(ineut4+4)
                            if (prolg(2:2) .eq. 'E') then
                                call utmess('F', 'PREPOST_9')
                            else if (prolg(2:2).eq.'C') then
                                sigma = zr(ineut3+2*ldim-1)
                            else if (prolg(1:1).eq.'L') then
                                temp1 = zr(ineut3+ldim-2)
                                temp2 = zr(ineut3+ldim-1)
                                sigma1 = zr(ineut3+2*ldim-2)
                                sigma2 = zr(ineut3+2*ldim-1)
                                pent = (sigma2-sigma1)/(temp2-temp1)
                                rest = sigma1 - pent*temp1
                                sigma = pent*tempa + rest
                            endif
                        else
                            do 50 k = 1, ldim-1
                                if (tempa .lt. zr(ineut3+k)) then
                                    sigma1 = zr(ineut3+ldim+k-1)
                                    sigma2 = zr(ineut3+ldim+k )
                                    tempdi = zr(ineut3+k) - zr(ineut3+ k-1)
                                    sigma = (&
                                            1-(&
                                            tempa-zr(ineut3+k-1))/ tempdi) * sigma1 + (1-(zr(ineu&
                                            &t3+ k)-tempa&
                                            )/tempdi&
                                            ) * sigma2
                                endif
50                          continue
                        endif
                    endif
110              continue
                goto 30
            endif
        else if (phenom.eq.'TRACTION') then
            romnom = matrev//'.'//phenom
            cocnom = romnom//'.VALK'
            call jeveuo(cocnom, 'L', ineut6)
            tranom = zk8(ineut6+1)
            parnom = tranom//'.PARA'
            call jelira(parnom, 'LONUTI', npara)
            call jeveuo(parnom, 'L', ineut5)
            pronom = tranom//'.PROL'
            call jeveuo(pronom, 'L', ineut8)
            proln = zk24(ineut8+4)
            natnom = tranom//'.VALE'
            call jelira(natnom, 'NUTIOC', nvale)
            valp = 0
            do 20 j = 1, npara
                if (tempa .lt. zr(ineut5-1+j)) then
                    valp = j
                    goto 21
                endif
20          continue
21          continue
            if (valp .eq. 1) then
                if (proln(1:1) .eq. 'E') then
                    call utmess('F', 'PREPOST_8')
                else if (proln(1:1).eq.'C') then
                    call jelira(jexnum(natnom, valp ), 'LONMAX', nbpt1)
                    call jeveuo(jexnum(natnom, valp ), 'L', itot1)
                    sigma = zr(itot1-1+nbpt1/2+1)
                else
                    temp1 = zr(ineut5-1+valp )
                    temp2 = zr(ineut5-1+valp+1)
                    call jelira(jexnum(natnom, valp ), 'LONMAX', nbpt1)
                    call jeveuo(jexnum(natnom, valp ), 'L', itot10)
                    call jelira(jexnum(natnom, valp+1), 'LONMAX', nbpt2)
                    call jeveuo(jexnum(natnom, valp+1), 'L', itot11)
                    lamb1 = zr(itot10-1+nbpt1/2+1)
                    lamb2 = zr(itot11-1+nbpt2/2+1)
                    pent = (lamb2-lamb1)/(temp2-temp1)
                    rest = lamb1 - pent*temp1
                    sigma = pent*tempa+rest
                endif
            else if (valp.eq.0) then
                if (proln(2:2) .eq. 'E') then
                    call utmess('F', 'PREPOST_9')
                else if (proln(2:2).eq.'C') then
                    call jelira(jexnum(natnom, npara), 'LONMAX', nbpt1)
                    call jeveuo(jexnum(natnom, npara), 'L', itot2)
                    sigma = zr(itot2-1+nbpt1/2+1)
                else
                    temp1 = zr(ineut5-1+npara-1)
                    temp2 = zr(ineut5-1+npara )
                    call jelira(jexnum(natnom, npara-1), 'LONMAX', nbpt1)
                    call jeveuo(jexnum(natnom, npara-1), 'L', itot12)
                    call jelira(jexnum(natnom, npara ), 'LONMAX', nbpt2)
                    call jeveuo(jexnum(natnom, npara ), 'L', itot13)
                    lamb1 = zr(itot12-1+nbpt1/2+1)
                    lamb2 = zr(itot13-1+nbpt2/2+1)
                    pent = (lamb2-lamb1)/(temp2-temp1)
                    rest = lamb1 - pent*temp1
                    sigma = pent*tempa+rest
                endif
            else
                temp1 = zr(ineut5-1+valp-1)
                temp2 = zr(ineut5-1+valp )
                tempd = temp2 - temp1
                coef1 = 1 - (tempa-temp1)/tempd
                coef2 = 1 - (temp2-tempa)/tempd
                call jelira(jexnum(natnom, valp-1), 'LONMAX', nbpt1)
                call jeveuo(jexnum(natnom, valp-1), 'L', itot14)
                call jelira(jexnum(natnom, valp ), 'LONMAX', nbpt2)
                call jeveuo(jexnum(natnom, valp ), 'L', itot15)
                lamb1 = zr(itot14-1+nbpt1/2+1)
                lamb2 = zr(itot15-1+nbpt2/2+1)
                sigma = coef1*lamb1 + coef2*lamb2
            endif
            goto 30
        endif
 3  continue
    call utmess('F', 'PREPOST_10')
30  continue
!
    rya = (k1a * k1a)/(6 * pi * sigma * sigma)
    if (oridef .eq. 'LONGI') then
        ca = 0.165d0*log(prodef*1000)
        cb = 0.465d0*(1+prodef/100*1000)
    else
        ca = 0.5d0
        cb = 0.5d0
    endif
!
    betaa = 1 + ca * tanh(36*rya/(lrev+deklag))
    betab = 1 + cb * tanh(36*rya/(lrev+deklag))
!
! --- correction plastique point A
!
    if (k1a .lt. kal) then
        k1acp = k1a + dkma
    else
        val1 = betaa*k1a
        val2 = k1a + dkma
        if (val1 .gt. val2) then
            k1acp = val1
            dkma = k1acp - k1a
        else
            k1acp = k1a + dkma
        endif
    endif
    kal = k1a
!
! --- correction plastique point B
!
    if (k1b .lt. kbl) then
        k1bcp = k1b + dkmb
    else
        val1 = betab*k1b
        val2 = k1b + dkmb
        if (val1 .gt. val2) then
            k1bcp = val1
            dkmb = k1bcp - k1b
        else
            k1bcp = k1b + dkmb
        endif
    endif
    kbl = k1b
!
! --- correction plastique point C
!
    if(profil(1:12).eq.'SEMI_ELLIPSE') then
       if (k1c .lt. kcl) then
          k1ccp = k1c + dkmc
       else
          val1 = betaa*k1c
          val2 = k1c + dkmc
          if (val1 .gt. val2) then
             k1ccp = val1
             dkmc = k1ccp - k1c
          else
             k1ccp = k1c + dkmc
          endif
       endif
       kcl = k1c
    endif
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
