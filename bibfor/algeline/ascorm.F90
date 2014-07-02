subroutine ascorm(monoap, typcmo, nbsup, nsupp, neq,&
                  nbmode, repmo1, repmo2, amort, modal,&
                  id, temps, recmor, recmop, tabs,&
                  nomsy, vecmod, reasup, spectr, corfre,&
                  muapde, tcosup, nintra, nbdis, f1gup,&
                  f2gup)
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterc/r8pi.h"
#include "asterfort/ascarm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbsup, nsupp(*), neq, nbmode, id, nintra, tcosup(nbsup, *)
    integer :: nbdis(nbsup)
    real(kind=8) :: vecmod(neq, *), spectr(*)
    real(kind=8) :: repmo1(nbsup, neq, *), amort(*)
    real(kind=8) :: repmo2(nbsup, neq, *)
    real(kind=8) :: reasup(nbsup, nbmode, *)
    real(kind=8) :: modal(nbmode, *), tabs(nbsup, *)
    real(kind=8) :: recmor(nbsup, neq, *), recmop(nbsup, neq, *)
    real(kind=8) :: temps, f1gup, f2gup
    character(len=*) :: typcmo
    character(len=16) :: nomsy
    aster_logical :: monoap, corfre, muapde
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
!     ------------------------------------------------------------------
!     COMMANDE : COMB_SISM_MODAL
!        RECOMBINAISON DES REPONSES MODALES
!        POUR LE MULTI_APPUI, CAS DES EXCITATIONS DECORRELEES
!     ------------------------------------------------------------------
! IN  : TYPCMO : TYPE DE COMBINAISON
! IN  : MONOAP : =.TRUE. , CAS DU MONO-APPUI
! IN  : NBSUP  : NOMBRE DE SUPPORT
! IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : NBMODE : NOMBRE DE MODES
! IN  : REPMOD : VECTEUR DES REPONSES MODALES
! IN  : AMORT  : VECTEUR DES AMORTISSEMENTS MODAUX
! IN  : MODAL  : VECTEUR DES PARAMETRES MODAUX
! IN  : ID     : DIRECTION
! IN  : TEMPS  : DUREE DE LA PARTIE FORTE SU SEISME (TYPCMO='DSC')
! OUT : RECMOR : VECTEUR DES COMBINAISONS DES REPONSES RIGIDES DES MODES
! OUT : RECMOP : VECTEUR DES COMBINAISONS DES REPONSES PERIO DES MODES
! IN  : NINTRA : NOMBRE d'INTRA-GROUPE
! IN  : NBDIS  : APPARTENANCE DES SUPPORTS AUX INTRAGROUPES
! IN  : F1GUP  : FREQUENCE F1 POUR LA METHODE DE GUPTA
! IN  : F2GUP  : FREQUENCE F2 POUR LA METHODE DE GUPTA
!     ------------------------------------------------------------------
    integer :: nsup, ii, im, im1, im2, in, is, ioc
    real(kind=8) :: b1, b12, b2, b22, w0, w1, w12, w2, w22
    real(kind=8) :: b1w1, b2w2, bp1, bp2, wp1, wp2, bp1w1, bp2w2
    real(kind=8) :: xnu, xde, xxx, xx1, xx2, test
    real(kind=8) :: zero, demi, un, deux, quatre, huit
    real(kind=8) :: fprop, alpha, alpha1, alpha2, pi
    real(kind=8) :: repmor, repmop
!
    call jemarq()
!
    pi = r8pi()
    zero = 0.d0
    demi = 0.5d0
    un = 1.d0
    deux = 2.d0
    quatre = 4.d0
    huit = 8.d0
!
    if (monoap .or. (.not.muapde)) then
!CC         NSUP=NBSUP
        nsup=1
    else
        nsup=nsupp(id)
    endif
!
    do 2 is = 1, nsup
        do 3 in = 1, neq
            recmor(is,in,id) = zero
            recmop(is,in,id) = zero
  3     continue
  2 end do
!
!     --- COMBINAISON EN VALEURS ABSOLUES ---
    if (typcmo(1:3) .eq. 'ABS') then
!
        do 5 im = 1, nbmode
            call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                        nbmode, vecmod, modal, id, reasup,&
                        spectr, repmo1, corfre, amort, muapde,&
                        tcosup, im, nbdis)
            do 4 is = 1, nsup
                do 6 in = 1, neq
                    xxx = repmo1(is,in,id)
                    ioc = nbdis(is)
                    recmop(ioc,in,id) = recmop(ioc,in,id) + abs(xxx)
  6             continue
  4         continue
  5     continue
        do 7 is = 1, nsup
            do 8 in = 1, neq
                ioc = nbdis(is)
                recmop(ioc,in,id) = recmop(ioc,in,id) * recmop(ioc,in, id)
  8         continue
  7     continue
!
!     --- AVEC REGLE DES "DIX POUR CENT" ---
    else if (typcmo(1:3).eq.'DPC') then
        im = 0
 40     continue
        im = im + 1
        if (im .le. nbmode) then
            im1 = im
            call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                        nbmode, vecmod, modal, id, reasup,&
                        spectr, repmo1, corfre, amort, muapde,&
                        tcosup, im1, nbdis)
            do 41 is = 1, nsup
                do 42 in = 1, neq
                    tabs(is,in) = abs(repmo1(is,in,id))
 42             continue
 41         continue
 52         continue
            if (im1 .le. nbmode) then
                w1 = sqrt(modal(im1,1))
                ii = 0
                do 44 im2 = im1+1, nbmode
                    w2 = sqrt(modal(im2,1))
                    w0 = demi * ( w1 + w2 )
                    test = ( w1 - w2 ) / w0
                    if (abs(test) .le. 0.10d0) then
                        ii = 1
                        im = im + 1
                        call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                                    nbmode, vecmod, modal, id, reasup,&
                                    spectr, repmo2, corfre, amort, muapde,&
                                    tcosup, im2, nbdis)
                        do 45 is = 1, nsup
                            do 46 in = 1, neq
                                xxx = abs(repmo2(is,in,id))
                                tabs(is,in)= tabs(is,in) + xxx
 46                         continue
 45                     continue
                    else
                        if (ii .eq. 0) goto 48
                        im1 = im2 - 1
                        goto 52
                    endif
 44             continue
            endif
 48         continue
            do 49 is = 1, nsup
                do 50 in = 1, neq
                    xxx = tabs(is,in)
                    ioc = nbdis(is)
                    recmop(ioc,in,id) = recmop(ioc,in,id) + xxx*xxx
 50             continue
 49         continue
            goto 40
        endif
!
!     --- GUPTA ---
    else if (typcmo(1:3).eq.'GUP') then
        do 88 im = 1, nbmode
!
!       CALCUL DU FACTEUR DE REPONSE RIGIDE DU MODE IM
            fprop = sqrt(modal(im,1))/(deux*pi)
            if (fprop .le. f1gup) then
                alpha = zero
            else if (fprop.ge.f2gup) then
                alpha = un
            else
                alpha = log(fprop/f1gup)/log(f2gup/f1gup)
            endif
            call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                        nbmode, vecmod, modal, id, reasup,&
                        spectr, repmo1, corfre, amort, muapde,&
                        tcosup, im, nbdis)
            do 89 is = 1, nsup
                do 90 in = 1, neq
!
!           CALCUL DES PARTIES RIGIDE ET PERIODIQUE DE LA REPONDE DU
!           MODE IM
                    repmor = alpha*repmo1(is,in,id)
                    repmop = sqrt(un-alpha*alpha)*repmo1(is,in,id)
                    ioc = nbdis(is)
!
!           SOMME ALGEBRIQUE DES REPONSES RIGIDES MODALES
                    recmor(ioc,in,id) = recmor(ioc,in,id) + repmor
!
!           SOMME DES CARRES DES REPONSES MODALES DYNAMIQUES
!           METHODE SIMPLE
                    recmop(ioc,in,id) = recmop(ioc,in,id) + repmop**2
 90             continue
 89         continue
 88     end do
!
!     SOMME DES CARRES DES REPONSES MODALES DYNAMIQUES
!     METHODE CQC
        do 220 im1 = 1, nbmode-1
            fprop = sqrt(modal(im1,1))/(deux*pi)
            if (fprop .le. f1gup) then
                alpha1 = zero
            else if (fprop.ge.f2gup) then
                alpha1 = un
            else
                alpha1 = log(fprop/f1gup)/log(f2gup/f1gup)
            endif
            b1 = amort(im1)
            w1 = sqrt(modal(im1,1))
            b1w1 = b1 * w1
            b12 = b1 * b1
            w12 = w1 * w1
            do 222 im2 = im1+1, nbmode
                fprop = sqrt(modal(im2,1))/(deux*pi)
                if (fprop .le. f1gup) then
                    alpha2 = zero
                else if (fprop.ge.f2gup) then
                    alpha2 = un
                else
                    alpha2 = log(fprop/f1gup)/log(f2gup/f1gup)
                endif
                b2 = amort(im2)
                w2 = sqrt(modal(im2,1))
                b2w2 = b2 * w2
                b22 = b2 * b2
                w22 = w2 * w2
                xnu = (sqrt(b1w1*b2w2)) * (b1w1+b2w2) * w1 * w2 * huit
                xde = (w12-w22)*(w12-w22) + b1w1*b2w2*(w12+w22)* quatre + (b12+b22)*w12*w22*quatr&
                      &e
                xxx = xnu / xde
                call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                            nbmode, vecmod, modal, id, reasup,&
                            spectr, repmo1, corfre, amort, muapde,&
                            tcosup, im1, nbdis)
                call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                            nbmode, vecmod, modal, id, reasup,&
                            spectr, repmo2, corfre, amort, muapde,&
                            tcosup, im2, nbdis)
                do 221 is = 1, nsup
                    do 224 in = 1, neq
                        xx1 = sqrt(un-alpha1*alpha1)*repmo1(is,in,id)
                        xx2 = sqrt(un-alpha2*alpha2)*repmo2(is,in,id)
                        ioc = nbdis(is)
                        recmop(ioc,in,id) = recmop(ioc,in,id) + (deux* xx1*xx2*xxx)
224                 continue
221             continue
222         continue
220     end do
    else
!
!     --- COMBINAISON QUADRATIQUE SIMPLE ---
        do 11 im = 1, nbmode
            call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                        nbmode, vecmod, modal, id, reasup,&
                        spectr, repmo1, corfre, amort, muapde,&
                        tcosup, im, nbdis)
            do 10 is = 1, nsup
                do 12 in = 1, neq
                    xxx = repmo1(is,in,id)
                    ioc = nbdis(is)
                    recmop(ioc,in,id) = recmop(ioc,in,id) + ( xxx * xxx )
 12             continue
 10         continue
 11     end do
!
!     --- CQC AVEC FORMULE DE DER-KIUREGHIAN ---
        if (typcmo(1:3) .eq. 'CQC') then
            do 20 im1 = 1, nbmode-1
                b1 = amort(im1)
                w1 = sqrt(modal(im1,1))
                b1w1 = b1 * w1
                b12 = b1 * b1
                w12 = w1 * w1
                do 22 im2 = im1+1, nbmode
                    b2 = amort(im2)
                    w2 = sqrt(modal(im2,1))
                    b2w2 = b2 * w2
                    b22 = b2 * b2
                    w22 = w2 * w2
                    xnu = (sqrt(b1w1*b2w2)) * (b1w1+b2w2) * w1 * w2 * huit
                    xde = (w12-w22)*(w12-w22) + b1w1*b2w2*(w12+w22)* quatre + (b12+b22)*w12*w22*q&
                          &uatre
                    xxx = xnu / xde
                    call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                                nbmode, vecmod, modal, id, reasup,&
                                spectr, repmo1, corfre, amort, muapde,&
                                tcosup, im1, nbdis)
                    call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                                nbmode, vecmod, modal, id, reasup,&
                                spectr, repmo2, corfre, amort, muapde,&
                                tcosup, im2, nbdis)
                    do 21 is = 1, nsup
                        do 24 in = 1, neq
                            xx1 = repmo1(is,in,id)
                            xx2 = repmo2(is,in,id)
                            ioc = nbdis(is)
                            recmop(ioc,in,id) = recmop(ioc,in,id) + (deux*xx1*xx2*xxx)
 24                     continue
 21                 continue
 22             continue
 20         continue
!
!     --- DSC AVEC FORMULE DE ROSENBLUETH ---
        else if (typcmo(1:3).eq.'DSC') then
            do 30 im1 = 1, nbmode-1
                b1 = amort(im1)
                w1 = sqrt(modal(im1,1))
                bp1 = b1 + ( deux / ( temps * w1 ) )
                wp1 = w1 * (sqrt( un - (bp1*bp1) ) )
                bp1w1 = bp1 * w1
                do 31 im2 = im1+1, nbmode
                    b2 = amort(im2)
                    w2 = sqrt(modal(im2,1))
                    bp2 = b2 + ( deux / ( temps * w2 ) )
                    wp2 = w2 * (sqrt( un - (bp2*bp2) ) )
                    bp2w2 = bp2 * w2
                    xde = ( wp1-wp2 ) / ( bp1w1 + bp2w2 )
                    xxx = un / ( un + (xde*xde) )
                    call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                                nbmode, vecmod, modal, id, reasup,&
                                spectr, repmo1, corfre, amort, muapde,&
                                tcosup, im1, nbdis)
                    call ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                                nbmode, vecmod, modal, id, reasup,&
                                spectr, repmo2, corfre, amort, muapde,&
                                tcosup, im2, nbdis)
                    do 32 is = 1, nsup
                        do 34 in = 1, neq
                            xx1 = repmo1(is,in,id)
                            xx2 = repmo2(is,in,id)
                            ioc = nbdis(is)
                            recmop(ioc,in,id) = recmop(ioc,in,id) + (deux*xx1*xx2*xxx)
 34                     continue
 32                 continue
 31             continue
 30         continue
        endif
    endif
!
!
    call jedema()
end subroutine
