subroutine xcfaq2(jlsn, jlst, jgrlsn, igeom, noma,&
                  nmaabs, pinter, ninter, ainter, nface,&
                  nptf, cface, nbtot, nfiss, ifiss)
    implicit none
!
#include "jeveux.h"
!
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/trigom.h"
#include "asterfort/abscvf.h"
#include "asterfort/abscvl.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/padist.h"
#include "asterfort/provec.h"
#include "asterfort/tecael.h"
#include "asterfort/vecini.h"
#include "asterfort/xajpin.h"
#include "asterfort/xcfacf.h"
#include "asterfort/xintar.h"
#include "asterfort/xinvac.h"
#include "asterfort/xmilfi.h"
#include "asterfort/xxmmvd.h"
#include "blas/ddot.h"
    integer :: jgrlsn, igeom, ninter, nface, cface(5, 3), jlsn, jlst
    integer :: nfiss, ifiss, nptf, nbtot, nmaabs
    real(kind=8) :: pinter(*), ainter(*)
    character(len=8) :: noma
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!                     TROUVER LES PTS D'INTERSECTION ENTRE LES ARETES,
!                     ET LE PLAN DE FISSURE, DÉCOUPAGE EN FACETTES,
!                     POINT MILIEU DE FISSURE (UNIQUEMENT 2D)
!     ENTREE
!       LSN      : VALEURS DE LA LEVEL SET NORMALE
!       LST      : VALEURS DE LA LEVEL SET TANGENTE
!       JGRLSN   : ADRESSE DU GRADIENT DE LA LEVEL SET NORMALE
!       IGEOM    : ADRESSE DES COORDONNEES DES NOEUDS DE L'ELT PARENT
!       NOMA     : NOM DU MAILLAGE
!       NMAABS   : INDICE DE LA MAILLE
!
!     SORTIE
!       PTINTER  : COORDONNEES DES POINTS D'INTERSECTION
!       NINTER  : NOMBRE DE POINTS D'INTERSECTION
!       AINTER  : INFOS ARETE ASSOCIEE AU POINTS D'INTERSECTION
!       NFACE   : NOMBRE DE FACETTES
!       NPTF    : NOMBRE DE POINTS PAR FACETTE
!       CFACE   : CONNECTIVITE DES NOEUDS DES FACETTES
!
!     ----------------------------------------------------------------
!
    real(kind=8) :: a(3), b(3), c(3), lsna, lsnb, longar, tampor(4)
    real(kind=8) :: alpha, nd(3), coor2d(9), bar(3), oa(3), am(3), ps, ps1
    real(kind=8) :: ab(2), lsta, lstb, lstc, abprim(2), prec
    real(kind=8) :: h(3), oh(3), noh, cos, noa, r3(3), theta(6), eps
    real(kind=8) :: ff(27), ksic(3), sc, tabar(9), lambda
    real(kind=8) :: m(3), lsnm, lstm, ksi, milfi(3), smilfi
    integer :: j, ar(12, 3), nbar, na, nb, ins,n(3)
    integer :: ia, i, ipt, ibid, pp, pd, nno, k
    integer :: iadzi, iazk24, ndim, ptmax
    integer :: zxain
    integer :: inm, inc, nm, nbnomx
    logical :: cut, ajout
    character(len=8) :: typma, elp, elc
!
    parameter       (ptmax=4, elc='SE3',nbnomx=27)
! --------------------------------------------------------------------
!
    call jemarq()
!
    eps=-1.0d-10
!
!     PREC PERMET D'EVITER LES ERREURS DE PRECISION CONDUISANT
!     A IA=IN=0 POUR LES MAILLES DU FRONT
    prec = 1000*r8prem()
    zxain = xxmmvd('ZXAIN')
    call elref1(elp)
    call elref4(' ', 'RIGI', ndim, nno, ibid,&
                ibid, ibid, ibid, ibid, ibid)
    ASSERT(ndim.eq.2)
!
!     1) RECHERCHE DES POINTS D'INTERSECTION
!     --------------------------------------
!
!     VECTEUR REEL À ZXAIN COMPOSANTES, POUR CHAQUE PT D'INTER :
!     - NUMÉRO ARETE CORRESPONDANTE         (0 SI C'EST UN NOEUD SOMMET)
!     - NUMÉRO NOEUD SI NOEUD SOMMET        (0 SINON)
!     - LONGUEUR DE L'ARETE
!     - POSITION DU PT SUR L'ARETE          (0 SI C'EST UN NOEUD SOMMET)
!     - ARETE VITALE                        (0 SI NON)
!
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
!
!     L'ELEMENT EST IL TRAVERSE PAR LA FISSURE?
    cut=.false.
    i=1
 1  continue
!     (1) RECHERCHE D'UN NOEUD PIVOT (LSN NON NULLE)
    if (zr(jlsn-1+i) .ne. 0 .and. i .lt. nno) then
        do 30 k = i+1, nno
!     (2) PRODUIT DE CE PIVOT PAR LES AUTRES LSN
            if (zr(jlsn-1+i)*zr(jlsn-1+k) .lt. 0.d0) cut=.true.
30      continue
    else
        i=i+1
        goto 1
    endif
    ipt=0
!     COMPTEUR DE POINT INTERSECTION = NOEUD SOMMET
    ins=0
!     COMPTEUR DE POINT INTERSECTION = POINT ARETE
    inc=0
!     COMPTEUR DE POINT INTERSECTION = NOEUD MILIEU
    inm=0
    call conare(typma, ar, nbar)
!
!     BOUCLE SUR LES ARETES POUR DETERMINER LES POINTS D'INTERSECTION
    do 100 ia = 1, nbar
!
!       NUM NO DE L'ELEMENT
        na=ar(ia,1)
        nb=ar(ia,2)
        nm=ar(ia,3)
        lsna=zr(jlsn-1+na)
        lsnb=zr(jlsn-1+nb)
        lsnm=zr(jlsn-1+nm)
        lsta=zr(jlst-1+na)
        lstb=zr(jlst-1+nb)
        lstm=zr(jlst-1+nm)
        do 110 i = 1, ndim
            a(i)=zr(igeom-1+ndim*(na-1)+i)
            b(i)=zr(igeom-1+ndim*(nb-1)+i)
            m(i)=zr(igeom-1+ndim*(nm-1)+i)
110      continue
        if (ndim .lt. 3) then
            a(3)=0.d0
            b(3)=0.d0
            c(3)=0.d0
            m(3)=0.d0
        endif
!
        ksi=1.d0
        longar=padist(ndim,a,b)
!
        if ((lsna*lsnb) .le. 0.d0) then
            if ((lsna.eq.0.d0) .and. (lsta.le.prec)) then
!           ON AJOUTE A LA LISTE LE POINT A
                if (lsta .ge. 0.d0) then
                    call xajpin(ndim, pinter, ptmax, ipt, ins,&
                                a, longar, ainter, 0, 0,&
                                0.d0, ajout)
                else
                    call xajpin(ndim, pinter, ptmax, ipt, ins,&
                                a, longar, ainter, 0, na,&
                                0.d0, ajout)
                endif
            endif
            if (lsnb .eq. 0.d0 .and. lstb .le. prec) then
!           ON AJOUTE A LA LISTE LE POINT B
                if (lstb .ge. 0.d0) then
                    call xajpin(ndim, pinter, ptmax, ipt, ins,&
                                b, longar, ainter, 0, 0,&
                                0.d0, ajout)
                else
                    call xajpin(ndim, pinter, ptmax, ipt, ins,&
                                b, longar, ainter, 0, nb,&
                                0.d0, ajout)
                endif
            endif
!
            if (lsnm .eq. 0.d0 .and. lstm .le. prec) then
!           ON AJOUTE A LA LISTE LE POINT M
                alpha=padist(ndim,a,m)
                if (lstm .ge. 0.d0) then
                    call xajpin(ndim, pinter, ptmax, ipt, inm,&
                                m, longar, ainter, 0, 0,&
                                0.d0, ajout)
                else
                    if (cut) then
                        call xajpin(ndim, pinter, ptmax, ipt, inc,&
                                    m, longar, ainter, ia, 0,&
                                    alpha, ajout)
                    else if (.not.cut) then
                        call xajpin(ndim, pinter, ptmax, ipt, inm,&
                                    m, longar, ainter, 0, nm,&
                                    alpha, ajout)
                    endif
                endif
            endif
!
            if (lsna .ne. 0.d0 .and. lsnb .ne. 0.d0 .and. lsnm .ne. 0) then
!           INTERPOLATION DES COORDONNÉES DE C
                call xintar(lsna, lsnb, lsnm, a, b,&
                            m, ndim, c)
!           POSITION DU PT D'INTERSECTION SUR L'ARETE
                alpha=padist(ndim,a,c)
                do 307 i = 1, ndim
                    tabar(i)=b(i)
                    tabar(ndim+i)=a(i)
                    tabar(2*ndim+i)=m(i)
307              continue
!          CALCUL DES FF DU SE3 (REEREF N'ACCEPTE PAS NDIM=2 & NNO=3)
                call abscvl(ndim, tabar, c, sc)
                call xinvac(elp, ndim, tabar, sc, ksic)
                ASSERT(ksic(1).ge.-1 .and. ksic(1).le.1)
                call elrfvf(elc, ksic(1), nbnomx, ff, ibid)
                lstc=ff(1)*lstb+ff(2)*lsta+ff(3)*lstm
                if (lstc .le. prec) then
                    if (lstc .ge. 0.d0) then
                        call xajpin(ndim, pinter, ptmax, ipt, inc,&
                                    c, longar, ainter, 0, 0,&
                                    0.d0, ajout)
                    else
                        call xajpin(ndim, pinter, ptmax, ipt, inc,&
                                    c, longar, ainter, ia, 0,&
                                    alpha, ajout)
                    endif
                endif
            endif
!
        endif
!
100  end do
!
!     RECHERCHE SPECIFIQUE POUR LES ELEMENTS EN FOND DE FISSURE
    call xcfacf(pinter, ptmax, ipt, ainter, zr(jlsn),&
                zr(jlst), igeom, nno, ndim, typma,&
                noma, nmaabs)
!
    ninter=ins+inc
    nbtot=ninter+inm
!
    if (cut .and. ninter .eq. 2 .and. inm .ne. 1) then
        do 20 j= 1,3
          n(j)=0
20      continue
! LE NEWTON NE PEUT PAS CONVERGER QUAND NDIM=3
! IL NOUS MANQUE L INFORMATION SUR LA FACE DE L ELT PARENT 
! TRANSPORTEE PAS N(3)
        ASSERT(ndim.lt.3)
!       RECHERCHE POINT MILIEU FISSURE
        call xmilfi(elp, n, ndim, nno, pinter, ndim,&
                       igeom,jlsn, 1, 2, milfi)
        do 10 j = 1, ndim
            coor2d(j)=pinter(j)
            coor2d(ndim+j)=pinter(ndim+j)
            coor2d(2*ndim+j)=milfi(j)
10      continue
        ksi=0.d0
        call abscvf(ndim, coor2d, ksi, smilfi)
!       ON AJOUTE A LA LISTE LE POINT MILFI
        call xajpin(ndim, pinter, ptmax, ipt, nbtot,&
                    milfi, smilfi*2, ainter, 5, 0,&
                    smilfi, ajout)
    endif
!
!     2) DECOUPAGE EN FACETTES TRIANGULAIRES DE LA SURFACE DEFINIE
!     ------------------------------------------------------------
!
!                  (BOOK IV 09/09/04)
!
!     CAS 3D
    if (ndim .eq. 3) then
        if (ninter .lt. 3) goto 500
!
        do 200 i = 1, 5
            do 201 j = 1, 3
                cface(i,j)=0
201          continue
200      continue
!
!       NORMALE A LA FISSURE (MOYENNE DE LA NORMALE AUX NOEUDS)
        call vecini(3, 0.d0, nd)
        do 210 i = 1, nno
            do 211 j = 1, 3
                nd(j)=nd(j)+zr(jgrlsn-1+3*(nfiss*(i-1)+ifiss-1)+j)/&
                nno
211          continue
210      continue
!
!       PROJECTION ET NUMEROTATION DES POINTS COMME DANS XORIFF
        call vecini(3, 0.d0, bar)
        do 220 i = 1, ninter
            do 221 j = 1, 3
                bar(j)=bar(j)+pinter((i-1)*3+j)/ninter
221          continue
220      continue
        do 230 j = 1, 3
            a(j)=pinter((1-1)*3+j)
            oa(j)=a(j)-bar(j)
230      continue
        noa=sqrt(oa(1)*oa(1) + oa(2)*oa(2) + oa(3)*oa(3))
!
!       BOUCLE SUR LES POINTS D'INTERSECTION POUR CALCULER L'ANGLE THETA
        do 240 i = 1, ninter
            do 241 j = 1, 3
                m(j)=pinter((i-1)*3+j)
                am(j)=m(j)-a(j)
241          continue
            ps=ddot(3,am,1,nd,1)
!
            ps1=ddot(3,nd,1,nd,1)
            lambda=-ps/ps1
            do 242 j = 1, 3
                h(j)=m(j)+lambda*nd(j)
                oh(j)=h(j)-bar(j)
242          continue
            ps=ddot(3,oa,1,oh,1)
!
            noh=sqrt(oh(1)*oh(1) + oh(2)*oh(2) + oh(3)*oh(3))
            cos=ps/(noa*noh)
!
            theta(i)=trigom('ACOS',cos)
!        SIGNE DE THETA (06/01/2004)
            call provec(oa, oh, r3)
            ps=ddot(3,r3,1,nd,1)
            if (ps .lt. eps) theta(i) = -1 * theta(i) + 2 * r8pi()
!
240      continue
!
!       TRI SUIVANT THETA CROISSANT
        do 250 pd = 1, ninter-1
            pp=pd
            do 251 i = pp, ninter
                if (theta(i) .lt. theta(pp)) pp=i
251          continue
            tampor(1)=theta(pp)
            theta(pp)=theta(pd)
            theta(pd)=tampor(1)
            do 252 k = 1, 3
                tampor(k)=pinter(3*(pp-1)+k)
                pinter(3*(pp-1)+k)=pinter(3*(pd-1)+k)
                pinter(3*(pd-1)+k)=tampor(k)
252          continue
            do 253 k = 1, zxain
                tampor(k)=ainter(zxain*(pp-1)+k)
                ainter(zxain*(pp-1)+k)=ainter(zxain*(pd-1)+k)
                ainter(zxain*(pd-1)+k)=tampor(k)
253          continue
250      continue
!
500      continue
!
!       NOMBRE DE POINTS D'INTERSECTION IMPOSSIBLE.
!       NORMALEMENT, ON A DEJE FAIT LA VERIF DANS XAJPIN
!       CEINTURE ET BRETELLE
        ASSERT(ninter.le.7)
!
        if (ninter .eq. 7) then
            nface=5
            nptf=3
            cface(1,1)=1
            cface(1,2)=2
            cface(1,3)=3
            cface(2,1)=1
            cface(2,2)=3
            cface(2,3)=5
            cface(3,1)=3
            cface(3,2)=4
            cface(3,3)=5
            cface(4,1)=1
            cface(4,2)=5
            cface(4,3)=7
            cface(5,1)=5
            cface(5,2)=6
            cface(5,3)=7
        else if (ninter.eq.6) then
            nface=4
            nptf=3
            cface(1,1)=1
            cface(1,2)=2
            cface(1,3)=3
            cface(2,1)=1
            cface(2,2)=3
            cface(2,3)=5
            cface(3,1)=1
            cface(3,2)=5
            cface(3,3)=6
            cface(4,1)=3
            cface(4,2)=4
            cface(4,3)=5
        else if (ninter.eq.5) then
            nface=3
            nptf=3
            cface(1,1)=1
            cface(1,2)=2
            cface(1,3)=3
            cface(2,1)=1
            cface(2,2)=3
            cface(2,3)=4
            cface(3,1)=1
            cface(3,2)=4
            cface(3,3)=5
        else if (ninter.eq.4) then
            nface=2
            nptf=3
            cface(1,1)=1
            cface(1,2)=2
            cface(1,3)=3
            cface(2,1)=1
            cface(2,2)=3
            cface(2,3)=4
        else if (ninter.eq.3) then
            nface=1
            nptf=3
            cface(1,1)=1
            cface(1,2)=2
            cface(1,3)=3
        else
            nptf=0
            nface=0
        endif
!
!     CAS 2D
    else if (ndim .eq. 2) then
!
        do 800 i = 1, 5
            do 801 j = 1, 3
                cface(i,j)=0
801          continue
800      continue
        if (ninter .eq. 2) then
!         NORMALE A LA FISSURE (MOYENNE DE LA NORMALE AUX NOEUDS)
            call vecini(2, 0.d0, nd)
            do 810 i = 1, nno
                do 811 j = 1, 2
                    nd(j)=nd(j)+zr(jgrlsn-1+2*(i-1)+j)/nno
811              continue
810          continue
!
            do 841 j = 1, 2
                a(j)=pinter(j)
                b(j)=pinter(2+j)
                ab(j)=b(j)-a(j)
841          continue
!
            abprim(1)=-ab(2)
            abprim(2)=ab(1)
!
            if (ddot(2,abprim,1,nd,1) .lt. 0.d0) then
                do 852 k = 1, 2
                    tampor(k)=pinter(k)
                    pinter(k)=pinter(2+k)
                    pinter(2+k)=tampor(k)
852              continue
                do 853 k = 1, 4
                    tampor(k)=ainter(k)
                    ainter(k)=ainter(zxain+k)
                    ainter(zxain+k)=tampor(k)
853              continue
            endif
            nface=1
            nptf=3
            cface(1,1)=1
            cface(1,2)=2
            cface(1,3)=3
        else
            nptf=0
            nface=0
        endif
!
    else
!       PROBLEME DE DIMENSION : NI 2D, NI 3D
        ASSERT(ndim.eq.2 .or. ndim.eq.3)
    endif
!
    call jedema()
end subroutine
