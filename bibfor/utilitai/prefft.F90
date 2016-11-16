subroutine prefft(resin, method, symetr, nsens, grand,&
                  vectot, nbva, ier)
    implicit none
#include "jeveux.h"
#include "asterfort/gettco.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/spdfft.h"
#include "asterfort/wkvect.h"
    integer :: npara, nsens
    character(len=4) :: grand
    character(len=16) :: symetr, method
    character(len=19) :: resin, vectot
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     REALISATION N.GREFFET
!     OPERATEUR "ENVOI A FFT : CREATION DE FONCTIONS"
!     IN:
!       RESIN    : SD_RESULTAT INITIALE HARMONIQUE
!                  (VENANT DE DYNA_VIBRA//HARM OU //TRAN/GEN)
!       METHOD   : METHODE POUR FFT
!       SYMETRIE : SPECTRE SYMETRIQUE OU NON
!       NSENS    : SENS FFT (1=DIRECT,-1=INVERSE)
!       GRAND    : GRANDEUR PHYSIQUE (DEPL,VITE,ACCE)
!
!     OUT:
!       NPARA : POINTEUR DU TABLEAU DE DONNEE VENANT DE LA FFT
!       NBVA  : NOMBRE DE PAS DE TEMPS
!
!
!
!
!
!     ------------------------------------------------------------------
    integer :: nbordr, jordr, ibid, i, ii
    integer :: iordr, lacce, lfon, iddl, nbva, neq
    integer :: lvar, ier, lordr, lval, iret, tord(1)
    integer :: lvale, nout, nbvin, nbvout, lfon2, j
    real(kind=8) :: r8b, dimag
    complex(kind=8) :: c16b
    character(len=4) :: grande
    character(len=8) :: k8b
    character(len=16) :: sym
    character(len=19) :: chdep, knume, cham19, nomfon, fonout
    character(len=24) :: chdep2, typres
!     ------------------------------------------------------------------
    call jemarq()
!      pour ne pas invalider NPARA
    grande = grand
    ier = 0
!
    call gettco(resin, typres)
    call jelira(resin//'.ORDR', 'LONUTI', nbordr)
    call rsorac(resin, 'LONUTI', 0, r8b, k8b,&
                c16b, r8b, k8b, tord, 1,&
                ibid)
    nbordr=tord(1)            
    knume='KNUME'
    call wkvect(knume, 'V V I', nbordr, jordr)
    call rsorac(resin, 'TOUT_ORDRE', 0, r8b, k8b,&
                c16b, r8b, k8b, zi(jordr), nbordr,&
                ibid)
    call jeveuo(knume, 'L', lordr)
!
!    Creation objet fonction
!
    nomfon = '&&PREFFT.FON_AV'
    if (nsens .eq. 1) then
        call wkvect(nomfon, 'V V R', 2*nbordr, lvar)
    else if (nsens.eq.-1) then
        call wkvect(nomfon, 'V V R', 3*nbordr, lvar)
    endif
    lfon = lvar + nbordr
    fonout = '&&PREFFT.FCTFFT'
!
    if (typres(6:9) .ne. 'GENE') then
        call rsexch('F', resin, grande, 1, chdep,&
                    iret)
        call jeveuo(chdep//'.VALE', 'L', lval)
!        --- NOMBRE D'EQUATIONS : NEQ
        chdep2 = chdep(1:19)//'.VALE'
        call jelira(chdep2, 'LONMAX', neq)
    else
        call jeveuo(resin//'.'//grande, 'L', lval)
        chdep2 = resin//'.'//grande
        call jelira(chdep2, 'LONMAX', neq)
        neq = neq / nbordr
        call jeveuo(resin//'.DISC', 'L', lacce)
!        --- LACCE : ACCES A LA LISTE D'INSTANTS/FREQUENCES
    endif
!
    iddl = 1
    ii = 0
    sym = symetr
!
    if (nsens .eq. 1) then
!     --- DE TEMPOREL EN FREQUENTIEL : TRAN_GENE EN HARM_GENE
!         OU BIEN DYNA_TRANS EN DYNA_HARMO
!
!        --- PREMIER FFT SUR UN SEUL DDL DANS LE BUT DE DIMENSIONNER
!            LE VECTEUR RESULTAT VECTOT, INDEXE PAR NPARA
        if (typres .ne. 'TRAN_GENE') then
!        --- CAS OU ON DISPOSE D'UNE DYNA_TRANS:
            do 5 iordr = 0, nbordr-1
!           --- BOUCLE SUR LES NUMEROS D'ORDRE (INSTANTS ARCHIVES)
                call rsexch('F', resin, grande, zi(jordr+iordr), cham19,&
                            iret)
                call rsadpa(resin, 'L', 1, 'INST', zi(jordr+iordr),&
                            0, sjv=lacce, styp=k8b)
                call jeveuo(cham19//'.VALE', 'L', lvale)
!              --- REMPLIR LE VECTEUR ABSCISSES DE LA FONCTION PREFFT
!                  AVEC LA LISTE D'INSTANTS RECUPEREE
                zr(lvar+iordr) = zr(lacce)
!              --- REMPLIR LE VECTEUR ORDONNES DE LA FONCTION PREFFT
!                  AVEC LE CHAMP RECUPERE
                zr(lfon+ii) = zr(lvale+iddl-1)
                ii = ii + 1
                call jelibe(cham19//'.VALE')
 5          continue
        else
!        --- CAS D'UNE TRAN_GENE:
            do 6 iordr = 0, nbordr-1
!              --- REMPLIR L'ABSCISSE ET ORDONNE DE LA FONCTION PREFFT
                zr(lvar+iordr) = zr(lacce+iordr)
                zr(lfon+ii) = zr(lval+iddl-1+(neq*iordr))
                ii = ii + 1
 6          continue
        endif
!
        nbvin = nbordr*2
!
!        --- CALCUL DE LA FFT DE LA FONCTION PREFFT DEFINIE PRECEDEMNT
        call spdfft(nsens, nomfon, nbvin, fonout, nbvout,&
                    method, sym, 'V')
        call jeveuo(fonout, 'L', nout)
!
!        --- VERIFICATIONS AVANT CREATION DE LA VECTEUR DES FFT FINAL
        call jeexin(vectot, iret)
        if (iret .ne. 0) call jedetr(vectot)
!
!        --- CREATION DU VECTEUR DES FFTS
        call wkvect(vectot, 'V V C', (neq+1)*nbvout, npara)
!
!        --- REMPLISSAGE AVEC LES PREMIERS RESULTATS POUR IDDL=1
        lfon2 = nout + nbvout
        do 15 i = 1, nbvout
            zc(npara+(iddl-1)*nbvout+i-1) = zc(lfon2+i-1)
15      continue
!
!        --- BOUCLE DES FFTS SUR LES AUTRES DDL'S
!            REFERER AUX PRECEDENTS COMMENTAIRES POUR + DE DETAILS
        if (typres .ne. 'TRAN_GENE') then
!        --- CAS D'UNE DYNA_TRANS A L'ENTREE
            call jelibe(cham19//'.VALE')
!           --- BOUCLE SUR LES DDL'S 2 A NEQ
            do 10 iddl = 2, neq
                ii = 0
!              --- REMPLISSAGE ORDONNEES DE LA FONCTION PREFFT
                do 20 iordr = 0, nbordr-1
                    call rsexch('F', resin, grande, zi(jordr+iordr), cham19,&
                                iret)
                    call jeveuo(cham19//'.VALE', 'L', lvale)
                    zr(lfon+ii) = zr(lvale+iddl-1)
                    ii = ii + 1
                    call jelibe(cham19//'.VALE')
20              continue
                sym = symetr
                nbvin = nbordr*2
!              --- CALCUL DES FFT
                call spdfft(nsens, nomfon, nbvin, fonout, nbvout,&
                            method, sym, 'V')
                call jeveuo(fonout, 'L', nout)
!              --- SAUVEGARDE DES RESULTATS DANS VECTOT
                lfon2 = nout + nbvout
                do 30 j = 1, nbvout
                    zc(npara+(iddl-1)*nbvout+j-1) = zc(lfon2+j-1)
30              continue
10          continue
!
        else
!        --- CAS D'UNE TRAN_GENE A L'ENTREE
!           --- BOUCLE SUR LES DDL'S 2 A NEQ
            do 11 iddl = 2, neq
                ii = 0
!              --- REMPLISSAGE ORDONNEES DE LA FONCTION PREFFT
                do 21 iordr = 0, nbordr-1
                    zr(lfon+ii) = zr(lval+iddl-1+(neq*iordr))
                    ii = ii + 1
21              continue
                sym = symetr
                nbvin = nbordr*2
!              --- CALCUL DES FFT
                call spdfft(nsens, nomfon, nbvin, fonout, nbvout,&
                            method, sym, 'V')
                call jeveuo(fonout, 'L', nout)
!              --- SAUVEGARDE DES RESULTATS DANS VECTOT
                lfon2 = nout + nbvout
                do 31 j = 1, nbvout
                    zc(npara+(iddl-1)*nbvout+j-1) = zc(lfon2+j-1)
31              continue
11          continue
!
        endif
!
!        --- STOCKAGE DES INSTANTS A LA FIN DANS VECTOT
        do 40 i = 1, nbvout
            zc(npara+(neq*nbvout)+i-1) = zc(nout+i-1)
40      continue
!
    else if (nsens.eq.-1) then
!     --- DE FREQUENTIEL EN TEMPOREL : HARM_GENE EN TRAN_GENE
!         OU BIEN DYNA_HARMO EN DYNA_TRANS
!
!        --- PREMIER FFT_INV SUR UN SEUL DDL DS LE BUT DE DIMENSIONNER
!            LE VECTEUR RESULTAT VECTOT, INDEXE PAR NPARA
        if (typres .ne. 'HARM_GENE') then
!        --- CAS D'UNE SD ENTRANTE HARMONIQUE SUR BASE PHYSIQUE
!            => SD_RESULTAT
            do 50 iordr = 1, nbordr
!             --- REMPLIR L'ABSCISSE ET ORDONNE DE LA FONCTION PREFFT
!             --- NOTE : VALEURS DES CHAMPS SONT COMPLEXES
                call rsexch('F', resin, grande, zi(jordr+iordr-1), cham19,&
                            iret)
                call rsadpa(resin, 'L', 1, 'FREQ', zi(jordr+iordr-1),&
                            0, sjv=lacce, styp=k8b)
                call jeveuo(cham19//'.VALE', 'L', lvale)
                zr(lvar+iordr-1) = zr(lacce)
                zr(lfon+ii) = dble(zc(lvale+iddl-1))
                ii = ii + 1
                zr(lfon+ii) = dimag(zc(lvale+iddl-1))
                ii = ii + 1
                call jelibe(cham19//'.VALE')
50          continue
        else
!        --- CAS D'UNE SD HARM_GENE
            do 51 iordr = 0, nbordr-1
!             --- REMPLIR L'ABSCISSE ET ORDONNE DE LA FONCTION PREFFT
!             --- NOTE : VALEURS DES CHAMPS SONT COMPLEXES
                zr(lvar+iordr) = zr(lacce+iordr)
                zr(lfon+ii) = dble(zc(lval+iddl-1+(neq*iordr)))
                ii = ii + 1
                zr(lfon+ii) = dimag(zc(lval+iddl-1+(neq*iordr)))
                ii = ii + 1
51          continue
        endif
!
        if (abs(zr(lfon+ii-1)) .lt. ((1.d-6)*abs(zr(lfon+ii-2)))) then
            zr(lfon+ii-1) = 0.d0
        endif
!
        nbvin = nbordr*3
!        --- CALCUL DU PREMIER FFT INVERSE SUR LA FONCTION CALCULEE
        call spdfft(nsens, nomfon, nbvin, fonout, nbvout,&
                    method, sym, 'V')
        call jeveuo(fonout, 'L', nout)
!
!        --- VERIFICATIONS AVANT CREATION DE LA VECTEUR DES FFT FINAL
        call jeexin(vectot, iret)
        if (iret .ne. 0) call jedetr(vectot)
!
!        --- CREATION DU VECTEUR DES FFTS
        call wkvect(vectot, 'V V R', (neq+1)*nbvout, npara)
!
!        --- REMPLISSAGE AVEC LES PREMIERS RESULTATS POUR IDDL=1
        lfon2 = nout + nbvout
        do 55 i = 1, nbvout
            zr(npara+(iddl-1)*nbvout+i-1) = zr(lfon2+i-1)
55      continue
!
!        --- BOUCLE DES FFTS INVERSES SUR LES AUTRES DDL'S
!            REFERER AUX PRECEDENTS COMMENTAIRES POUR + DE DETAILS
        if (typres .ne. 'HARM_GENE') then
            call jelibe(cham19//'.VALE')
!          --- BOUCLE SUR LES AUTRES DDLS DE 2 A NEQ
            do 100 iddl = 2, neq
                ii = 0
                do 70 iordr = 1, nbordr
                    call rsexch('F', resin, grande, zi(jordr+iordr-1), cham19,&
                                iret)
                    call jeveuo(cham19//'.VALE', 'L', lvale)
                    zr(lfon+ii) = dble(zc(lvale+iddl-1))
                    ii = ii + 1
                    zr(lfon+ii) = dimag(zc(lvale+iddl-1))
                    ii = ii + 1
                    call jelibe(cham19//'.VALE')
70              continue
                sym = symetr
                nbvin = nbordr*3
!             --- CALCUL DES FFT'S INVERSES
                call spdfft(nsens, nomfon, nbvin, fonout, nbvout,&
                            method, sym, 'V')
                call jeveuo(fonout, 'L', nout)
!             --- SAUVEGARDE DES RESULTATS DANS VECTOT
                lfon2 = nout + nbvout
                do 80 j = 1, nbvout
                    zr(npara+(iddl-1)*nbvout+j-1) = zr(lfon2+j-1)
80              continue
100          continue
        else
            do 101 iddl = 2, neq
                ii = 0
                do 102 iordr = 0, nbordr-1
!               --- REMPLIR L'ABSCISSE ET ORDONNE DE LA FONCTION PREFFT
                    zr(lfon+ii) = dble(zc(lval+iddl-1+(neq*iordr)))
                    ii = ii + 1
                    zr(lfon+ii) = dimag(zc(lval+iddl-1+(neq*iordr)))
                    ii = ii + 1
102              continue
                sym = symetr
                nbvin = nbordr*3
!             --- CALCUL DES FFT'S INVERSES
                call spdfft(nsens, nomfon, nbvin, fonout, nbvout,&
                            method, sym, 'V')
                call jeveuo(fonout, 'L', nout)
!             --- SAUVEGARDE DES RESULTATS DANS VECTOT
                lfon2 = nout + nbvout
                do 103 j = 1, nbvout
                    zr(npara+(iddl-1)*nbvout+j-1) = zr(lfon2+j-1)
103              continue
101          continue
        endif
!
! On stocke les instants a la fin
!
        do 400 i = 1, nbvout
            zr(npara+(neq*nbvout)+i-1) = zr(nout+i-1)
400      continue
    endif
    if (typres(6:9) .ne. 'GENE') call jelibe(cham19//'.VALE')
!
    nbva = nbvout
    call jedetr(knume)
    call jedetr(nomfon)
    call jedema()
end subroutine
