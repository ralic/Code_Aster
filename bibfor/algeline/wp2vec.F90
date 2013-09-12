subroutine wp2vec(appr, opt, nbfreq, nbvect, neq,&
                  shift, yh, yb, vr, nlivr,&
                  vpr, vpi, vecp, mxresf, resufi,&
                  resufr, lagr, omecor)
    implicit none
#include "jeveux.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
#include "asterfort/wpordo.h"
#include "asterfort/wprest.h"
#include "asterfort/wptest.h"
    character(len=1) :: appr
    character(len=*) :: opt
    integer :: nbfreq, nbvect, neq, lagr(*), mxresf, nlivr, resufi(mxresf, *)
    complex(kind=8) :: vecp(neq, *), shift
    real(kind=8) :: resufr(mxresf, *), yh(neq, *), yb(neq, *), vpr(*), vpi(*)
    real(kind=8) :: vr(nlivr, *), omecor
!     -----------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     RESTITUTION DES VALEURS PROPRES ET DES MODES DU PB QUADRATIQUE
!     AVEC TRI SUIVANT LES PARTIES IMMAGINAIRES CROISANTES
!     ET NORMALISATION A LA PLUS GRANDE CMP NON LAGRANGE
!
! --> EN PUS ON MET UN TEST DE CONJUGAISON
! --> STOCKAGE D'UN SEUL COUPLE PARMI 2 CONJUGUES
!     -----------------------------------------------------------------
! IN  APPR   : K : INDICATEUR D' APPROCHE 'R' OU 'I'
! IN  OPT    : K : OPTION : 'CENTRE' OU 'PLUS_PETITE'
! IN  NBFREQ : I : NOMBRE DE MODES DEMANDES
! IN  NBVECT : I : NOMBRE DE VECTEURS DE LANCZOS
! IN  NEQ    : I : TAILLE DES MATRICES DU PB QUADRATIQUE
! IN  SHIFT  : R : VALEUR DU DECALAGE
! IN  YH     : R : PARTIE HAUTE DES VECTEURS DE LANCZOS
! IN  YB     : R : PARTIE BASSE DES VECTEURS DE LANCZOS
! IN  LAGR   : I : INDICATEUR DES NON-LAGRANGE
! VAR VPR    : R : IN  : PARTIE REELLE DES VALEURS PROPRE DU PB REDUIT
!            :   : OUT : PARTIE REELLE DES VALEURS PROPRE DU PB QUAD
! VAR VPI    : R : IN  : PARTIE IMMAGI DES VALEURS PROPRE DU PB REDUIT
!            :   : OUT : PARTIE IMMAGI DES VALEURS PROPRE DU PB QUAD
! IN  VR     : R : MODES DU PB REDUIT. C'EST L'EQUIVALENCE D'UNE
!                  MATRICE COMPLEXE DE LONGUEUR NBVECT.
! OUT VECP   : C : MODES DU PB QUADRATIQUE
! OUT RESUFR : C : TABLEAU DE POST-TRAITEMENT
! IN  OMECOR : R : "ZERO MODAL", SEUIL EN DECA DUQUEL DEUX MODES SONT
!                  CONSIDERES COMME IDENTIQUES
!     -----------------------------------------------------------------
!
!
!     ------------------------------------------------------------------
    real(kind=8) :: si, mod2, a, b, nmabp, nmabm, am, om, eps, seuilr, seuilp
    real(kind=8) :: c1, auxrj, seuilc, auxij, auxrk, auxik
    integer :: i, j, k, av1, av2, av, iadind, nbfrga, vali(5), nbcmpp, nbcmpc
    integer :: nbreel, nbfr, ibid
    complex(kind=8) :: des, vpq, mhu, vpp, vpm
    logical :: trouve, lconj
    character(len=1) :: kmsg
    character(len=16) :: valk, typres
!
!     -----------------------------------------------------------------
    call jemarq()
    si = dimag(shift)
!
! --- 1. PARTITION ,TEST DE CONJUGAISON, ELIMINATION DES CONJUGUES
!        REMARQUE : SI QR N' A PAS EU DE PB ALORS LES MODES REDUITS :
!                   * COMPLEXES ET 2 A 2 CONJUGUES,LES CONJUGUES
!                     APPARAISSENT LES UNS A LA SUITE DES AUTRES
!                   * REELS
!
!        IMPLEMENTATION : DANS UN TABLEAU D' ENTIER (ZI(IADIND))
!        INVARIANT      : T(I) = -2 VP(I) NON CLASSEE
!                         T(I) =  1 <=> VP(I) COMPLEXE AVEC CONJUGUEE
!                                             SELECTIONNEE
!                         T(I) = -1 <=> VP(I) COMPLEXE AVEC CONJUGUEE
!                                             ELIMINEE
!                         T(I) =  0 <=> VP(I) COMPLEXE SANS CONJUGUEE
!                                             OU REELLE
!                                             ELIMINEE
!     -----------------------------------------------------------------
! --- 1.1. PARTITION (OPERATEUR REEL)
    nbcmpp = 0
    nbcmpc = 0
    nbreel = 0
!
!     SI IM(VP)<SEUILR: VP EST CONSIDEREE COMME REELLE
    seuilr=1.d-7
!     SI MODULE(VPK-VPJ) < SEUILP: VPK = CONJUGEE DE VPJ
    seuilp=omecor
!     SEUIL POUR LE COUPLAGE HAUT-BAS DES VECTEURS PROPRES
    seuilc=1.d-4
!
    call wkvect('&&WP2VEC.INDIC.PART.VP', 'V V I', nbvect, iadind)
    do 1 j = 1, nbvect
        zi(iadind + j-1) = -2
 1  end do
    do 2 j = 1, nbvect
        auxrj=vpr(j)
        auxij=vpi(j)
        if (zi(iadind + j-1) .eq. -2) then
            if (abs(auxij) .lt. seuilr) then
                zi(iadind+j-1) = -3
                nbreel=nbreel+1
            else
                if (abs(auxrj) .lt. seuilr) auxrj=0.d0
                k = j + 1
                trouve = .false.
 3              continue
                if ((.not. trouve ) .and. ( k .le. nbvect)) then
                    auxrk=vpr(k)
                    auxik=vpi(k)
                    if (abs(auxrk) .lt. seuilr) auxrk=0.d0
                    if (abs(auxik) .lt. seuilr) auxik=0.d0
                    c1=sqrt((auxrj-auxrk)**2+(auxij+auxik)**2)
                    if (c1 .lt. seuilp) then
                        lconj=.true.
                    else
                        lconj=.false.
                    endif
                    if ((zi(iadind+k-1).eq.-2) .and. lconj .and. (auxij* auxik.le.0.d0)) then
                        trouve = .true.
                        nbcmpc = nbcmpc + 1
                        if (auxij .gt. 0.d0) then
                            zi(iadind + j-1) = 1
                            zi(iadind + k-1) = -1
                        else
                            zi(iadind + j-1) = -1
                            zi(iadind + k-1) = 1
                        endif
                    else
                        k = k + 1
                    endif
                    goto 3
                endif
                if (.not. trouve) then
                    nbcmpp = nbcmpp + 1
                    zi(iadind + j-1) = 0
                endif
            endif
        endif
 2  end do
!
!
    if (zi(iadind + nbvect-1) .eq. -2) then
        zi(iadind + nbvect-1) = 0
        nbcmpp = nbcmpp +1
    endif
!
    if (nbcmpp .gt. 0) then
        vali (1) = nbreel
        vali (2) = nbcmpc
        vali (3) = nbcmpp
        call u2mesi('A', 'ALGELINE4_87', 3, vali)
    endif
!
    if (nbreel .gt. 0) then
        vali (1) = nbreel
        vali (2) = nbcmpc
        vali (3) = nbcmpp
        call u2mesi('I', 'ALGELINE4_88', 3, vali)
    endif
!
! --- 1.2. DETERMINATION DE NB FREQUENCES GARDEES
    nbfrga =nbcmpc
!
! --- 1.3. ELIMINATION DES CONJUGUES (OPERATEUR REEL) -- COMPACTAGE --
    k = 1
    do 4 j = 1, nbvect
        if (zi(iadind + j-1) .gt. 0) then
            if (k .ne. j) then
                vpr(k) = vpr(j)
                vpi(k) = vpi(j)
                zi(iadind + k-1) = zi(iadind + j-1)
                do 5, i = 1, nlivr, 1
                vr(i,k) = vr(i,j)
 5              continue
            endif
            k = k + 1
        endif
 4  end do
    nbfrga=k-1
! NBRE DE VP RECOMPACTEES
    nbfr=k-1
!
!     ---------- FIN DE PARTITION TEST ET ELIMINATION -----------------
!     ----------    AU NIVEAU DE L' OPERATEUR REEL    -----------------
!
! --- 2. CALCUL DES SOLUTIONS PROPRES DU PB QUADRATIQUE ---
    if (opt .eq. 'CENTRE') then
        call wkvect('&&WP2VEC.VEC.AUX.C1', 'V V C', neq, av1)
        call wkvect('&&WP2VEC.VEC.AUX.C2', 'V V C', neq, av2)
        call wkvect('&&WP2VEC.VEC.AUX.C ', 'V V C', neq, av)
    endif
    do 10 j = 1, nbfr
        if (zi(iadind + j-1) .gt. 0) then
            a = vpr(j)
            b = vpi(j)
            mhu = dcmplx(a,b)
            mod2 = a*a + b*b
            mod2 = 1.d0/mod2
            call wprest(yh, vr(1, j), neq, nbvect, vecp(1, j))
            if (opt .eq. 'PLUS_PETITE') then
                a = a*mod2
                b = -b*mod2
            else if (opt .eq. 'CENTRE') then
                call wprest(yb, vr(1, j), neq, nbvect, zc(av))
                if (appr .eq. 'R') then
                    des = dcmplx(1.d0,0.d0)-dcmplx(4.d0*si*si,0.d0)* mhu*mhu
                    des = sqrt(des)
                    vpq = .5d0*( dcmplx(1.d0,0.d0)-dcmplx(0.d0,2.d0*si) *mhu + des )/mhu
                    vpp = vpq + shift
                    call wptest(lagr, vecp(1, j), zc(av), vpp, neq,&
                                nmabp)
                    vpq = .5d0*( dcmplx(1.d0,0.d0)-dcmplx(0.d0,2.d0*si) *mhu - des )/mhu
                    vpm = vpq + shift
                    call wptest(lagr, vecp(1, j), zc(av), vpm, neq,&
                                nmabm)
                else
                    des = -dcmplx(si*si,0.d0)*mhu*mhu + dcmplx(si, 0.d0)*mhu
                    des = sqrt(des)
                    vpq = -dcmplx(0.d0,si) + des/mhu
                    vpp = vpq + shift
                    call wptest(lagr, vecp(1, j), zc(av), vpp, neq,&
                                nmabp)
                    vpq = -dcmplx(0.d0,si) - des/mhu
                    vpm = vpq + shift
                    call wptest(lagr, vecp(1, j), zc(av), vpm, neq,&
                                nmabm)
                endif
                if (nmabm .lt. nmabp) then
                    a = dble (vpm)
                    b = dimag(vpm)
                    eps=nmabm
                else
                    a = dble (vpp)
                    b = dimag(vpp)
                    eps=nmabp
                endif
                if (eps .gt. seuilc) then
                    zi(iadind + j-1)=0
                    nbfrga=nbfrga-1
                endif
            endif
            vpr(j) = a
            vpi(j) = b
        endif
10  end do
!
! --- 1.3. ELIMINATION DES VALEURS FAUSSES -- RECOMPACTAGE --
    k = 1
    do 44 j = 1, nbfr
        if (zi(iadind + j-1) .gt. 0) then
            if (k .ne. j) then
                vpr(k) = vpr(j)
                vpi(k) = vpi(j)
                zi(iadind + k-1) = zi(iadind + j-1)
                do 55, i = 1, nlivr, 1
                vr(i,k) = vr(i,j)
55              continue
            endif
            k = k + 1
        endif
44  end do
    nbfrga=k-1
!
! --- 3. SELECTION DES VALEURS PROPRES (PB QUADRATIQUE)
    do 20, j = 1, nbfrga, 1
    if ((zi(iadind + j-1).eq.1 ) .and. ( vpi(j).lt.0.d0)) then
        vpi(j) = -vpi(j)
        do 21 i = 1, neq
            vecp(i,j) = dconjg(vecp(i,j))
21      continue
    endif
    20 end do
!
! --- 4. PREPARATION DE RESUFR
    if (nbfreq .gt. nbfrga) then
        vali(1)=nbfreq
        vali(2)=nbfrga
        nbfreq=nbfrga
        if (nbfreq .eq. 0) then
            kmsg='F'
        else
            kmsg='A'
        endif
        call getvtx(' ', 'TYPE_RESU', scal=typres, nbret=ibid)
        valk='FREQ'
        if (typres .ne. 'DYNAMIQUE') valk='CHAR_CRIT'
        call u2mesg(kmsg//'+', 'ALGELINE5_79', 1, valk, 2,&
                    vali, 0, 0.d0)
        if (kmsg .eq. 'A') call u2mesk(kmsg//'+', 'ALGELINE5_80', 1, valk)
        call u2mesk(kmsg, 'ALGELINE5_81', 1, valk)
    endif
!
! --- 5. TRI (DANS LE SPECTRE ET DE PRESENTATION) DES VALEURS PROPRES-
    call wpordo(1, shift, vpr, vpi, vecp,&
                nbfrga, neq)
    call wpordo(0, shift, vpr, vpi, vecp,&
                nbfreq, neq)
!
    do 30 j = 1, nbfreq
        am = vpr(j)*vpr(j)
        om = vpi(j)*vpi(j)
        resufi(j,1) = j
        resufr(j,2) = om
        resufr(j,3) = -vpr(j)/sqrt(om + am)
30  end do
!
! --- 6. DESTRUCTION DES OJB TEMPORAIRES
    if (opt .eq. 'CENTRE') then
        call jedetr('&&WP2VEC.VEC.AUX.C1')
        call jedetr('&&WP2VEC.VEC.AUX.C2')
        call jedetr('&&WP2VEC.VEC.AUX.C ')
    endif
    call jedetr('&&WP2VEC.INDIC.PART.VP')
!
    call jedema()
end subroutine
