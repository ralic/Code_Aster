subroutine op0024()
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
!     ------------------------------------------------------------------
!
!     COMMANDE : DEFI_LIST_REEL
!
!     ON CREE LES OBJETS CONTENANT LA LISTE DE REELS:
!     -----------------------------------------------
!     RESU  .LPAS = DT_1 ,DT_2 ,... ,DT_N
!                   DT_I : PAS DE TEMPS DE L'INTERVALLE I
!     RESU  .NBPA = NPT_1 ,NPT_2 ,... ,NPT_N
!                   NPT_I : NOMBRE DE PAS DE TEMPS DE L'INTERVALLE I
!     RESU  .BINT = B_0 ,B_1 ,B_2 ,... ,B_N
!                   B_I : BORNE DE L'INTERVALLE DONNE PAR L'UTILISATEUR
!     RESU  .VALE = I_0 ,I_1 ,I_2 ,... ,I_N
!                   I_I : VALEUR DU I-EME PAS
!
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/liimpr.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    real(kind=8) :: debut, fin, pas, xxx, xpdt, toler, derpas
    real(kind=8) :: valr(3)
    integer :: ifm, niv, nv, nbvale, ndim, jpas, jnbp, jbor, jval, kval, i
    integer :: vali
    integer :: n1, nbocc, nsup, iocc, np, nbpas, nbval, iinter, ico, j
    character(len=19) :: resu
    character(len=16) :: nomcmd, concep
!     ------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
!
!
    call getres(resu, concep, nomcmd)
    call getvr8(' ', 'VALE', nbval=0, nbret=nv)
!
!
!     CAS DU MOT CLE VALE=
!    ----------------------
    if (nv .ne. 0) then
        nbvale = -nv
        ndim = max(1,nbvale-1)
        call wkvect(resu//'.LPAS', 'G V R', ndim, jpas)
        call wkvect(resu//'.NBPA', 'G V I', ndim, jnbp)
        call wkvect(resu//'.BINT', 'G V R', nbvale, jbor)
        call wkvect(resu//'.VALE', 'G V R', nbvale, jval)
        call wkvect('&&OP0024.VALE', 'V V R', nbvale, kval)
        call getvr8(' ', 'VALE', nbval=nbvale, vect=zr(kval), nbret=nv)
        do 10 i = 1, nbvale - 1
            zr(jpas+i-1) = zr(kval+i) - zr(kval+i-1)
            zi(jnbp+i-1) = 1
            zr(jbor+i-1) = zr(kval+i-1)
            zr(jval+i-1) = zr(kval+i-1)
10      continue
        zr(jbor+nbvale-1) = zr(kval+nbvale-1)
        zr(jval+nbvale-1) = zr(kval+nbvale-1)
!
!
!     CAS DU MOT CLE INTERVALLE=
!    ----------------------------
    else
        call getvr8(' ', 'DEBUT', scal=debut, nbret=n1)
        call getfac('INTERVALLE', nbocc)
        toler = 1.d-3
!
!       -- ON COMPTE LE NOMBRE D'INTERVALLES SUPPLEMENTAIRES (NSUP)
!          QU'IL FAUDRA CREER DU FAIT DES ARRONDIS (MOT CLE PAS)
!       ---------------------------------------------------------
        nsup = 0
        do 20 iocc = 1, nbocc
            call getvr8('INTERVALLE', 'JUSQU_A', iocc=iocc, scal=fin, nbret=n1)
            call getvr8('INTERVALLE', 'PAS', iocc=iocc, scal=pas, nbret=np)
            if (np .eq. 1) then
                if (pas .eq. 0.d0) then
                    call utmess('F', 'ALGORITH9_16')
                endif
!
                xpdt = (fin-debut)/pas
                if (xpdt .le. (1.d0-toler)) then
                    valr(1) = debut
                    valr(2) = fin
                    valr(3) = pas
                    call utmess('F', 'ALGORITH9_15', nr=3, valr=valr)
                endif
                nbpas = nint(xpdt)
                if (nbpas .le. 0) then
                    call utmess('F', 'ALGORITH9_17')
                endif
!
                derpas = fin - (debut+ (nbpas-1)*pas)
                if (abs((derpas-pas)/pas) .gt. toler) nsup = nsup + 1
            endif
            debut = fin
20      continue
!
!
!
        call wkvect(resu//'.LPAS', 'G V R', nbocc+nsup, jpas)
        call wkvect(resu//'.NBPA', 'G V I', nbocc+nsup, jnbp)
        call wkvect(resu//'.BINT', 'G V R', nbocc+nsup+1, jbor)
!
        call getvr8(' ', 'DEBUT', scal=debut, nbret=n1)
        zr(jbor-1+1) = debut
        nbval = 1
        iinter = 0
        do 30 iocc = 1, nbocc
            iinter = iinter + 1
            call getvr8('INTERVALLE', 'JUSQU_A', iocc=iocc, scal=fin, nbret=n1)
!
            xxx = fin - debut
            call getvr8('INTERVALLE', 'PAS', iocc=iocc, scal=pas, nbret=np)
!
            if (np .eq. 1) then
                nbpas = nint(xxx/pas)
!
                derpas = fin - (debut+ (nbpas-1)*pas)
                if (abs((derpas-pas)/pas) .gt. toler) then
                    if ((debut+nbpas*pas) .gt. fin) nbpas = nbpas - 1
                    zi(jnbp-1+iinter) = nbpas
                    zr(jpas-1+iinter) = pas
                    zr(jbor-1+iinter+1) = debut+pas*nbpas
                    nbval = nbval + nbpas
!
!             -- CREATION D'UN INTERVALLE SUPPLEMENTAIRE:
                    iinter = iinter + 1
                    zi(jnbp-1+iinter) = 1
                    zr(jpas-1+iinter) = fin - (debut+pas*nbpas)
                    zr(jbor-1+iinter+1) = fin
                    nbval = nbval + 1
!
                    valr(1) = pas
                    vali = iocc
                    call utmess('A', 'ALGORITH13_82', si=vali, sr=valr(1))
!
!
!
                else
                    zi(jnbp-1+iinter) = nbpas
                    zr(jpas-1+iinter) = pas
                    zr(jbor-1+iinter+1) = fin
                    nbval = nbval + nbpas
                endif
!
            else
                call getvis('INTERVALLE', 'NOMBRE', iocc=iocc, scal=nbpas, nbret=n1)
                if (nbpas .le. 0) then
                    call utmess('F', 'ALGORITH9_17')
                endif
                zi(jnbp-1+iinter) = nbpas
                zr(jpas-1+iinter) = xxx/nbpas
                zr(jbor-1+iinter+1) = fin
                nbval = nbval + nbpas
            endif
            debut = fin
30      continue
!
!
!       -- CREATION DE L'OBJET .VALE :
!       -------------------------------
        call wkvect(resu//'.VALE', 'G V R', nbval, jval)
        zr(jval) = zr(jbor)
        ico = 0
        do 50 i = 1, nbocc + nsup
            xpdt = zr(jpas-1+i)
            do 40 j = 1, zi(jnbp-1+i) - 1
                ico = ico + 1
                zr(jval+ico) = zr(jval+ico-1) + xpdt
40          continue
            ico = ico + 1
            zr(jval+ico) = zr(jbor+i)
50      continue
    endif
!
!
!     --- TITRE ---
    call titre()
!
!
!     --- IMPRESSION ---
    if (niv .gt. 1) call liimpr(resu, niv, 'MESSAGE')
!
    call jedema()
end subroutine
