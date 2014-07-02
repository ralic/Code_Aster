subroutine xcface(lsn, lst, jgrlsn, igeom, enr,&
                  nfiss, ifiss, fisco, nfisc, noma,&
                  nmaabs, pinter, ninter, ainter, nface,&
                  nptf, cface)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterc/r8maem.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/padist.h"
#include "asterfort/provec.h"
#include "asterfort/tecael.h"
#include "asterfort/trigom.h"
#include "asterfort/vecini.h"
#include "asterfort/xajpin.h"
#include "asterfort/xcfacf.h"
#include "asterfort/xcfacj.h"
#include "asterfort/xxmmvd.h"
#include "blas/ddot.h"
    real(kind=8) :: lsn(*), lst(*), pinter(*), ainter(*)
    integer :: jgrlsn, igeom, ninter, nface, cface(5, 3), nptf
    integer :: nfiss, ifiss, fisco(*), nfisc, nmaabs
    character(len=8) :: noma
    character(len=16) :: enr
!     ------------------------------------------------------------------
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
! person_in_charge: samuel.geniaut at edf.fr
!                     TROUVER LES PTS D'INTERSECTION ENTRE LES ARETES
!                      ET LE PLAN DE FISSURE ET DÉCOUPAGE EN FACETTES
!
!     ENTREE
!       LSN      : VALEURS DE LA LEVEL SET NORMALE
!       LST      : VALEURS DE LA LEVEL SET TANGENTE
!       JGRLSN   : ADRESSE DU GRADIENT DE LA LEVEL SET NORMALE
!       IGEOM    : ADRESSE DES COORDONNEES DES NOEUDS DE L'ELT PARENT
!       ENR      : VALEUR DE L'ATTRIBUT DE L'ELEMENT
!       NFISS    : NOMBRE DE FISSURES VUES DANS L'ÉLÉMENT
!       IFISS    : NUMÉRO DE LA FISSURE EN COURS
!       FISCO    : NUM ET COEF DES FISS SUR LESQUELLES IFISS SE BRANCHE
!       NFISC    : NOMBRE DE FISSURES SUR LESQUELLES IFISS SE BRANCHE
!       NOMA     : NOM DU MAILLAGE
!       NMAABS   : INDICE DE LA MAILLE
!
!     SORTIE
!
!       PINTER  : COORDONNEES DES POINTS D'INTERSECTION POUR IFISS
!       NINTER  : NOMBRE DE POINTS D'INTERSECTION POUR IFISS
!       AINTER  : INFOS ARETE ASSOCIEE AU POINTS D'INTERSECTI POUR IFISS
!       NFACE   : NOMBRE DE FACETTES POUR IFISS
!       NPTF    : NOMBRE DE "NOEUDS" DES FACETTES (SOMMETS ET MILIEUX)
!       CFACE   : CONNECTIVITE DES NOEUDS DES FACETTES POUR IFISS
!
!     ------------------------------------------------------------------
!
    real(kind=8) :: a(3), b(3), c(3), lsna, lsnb, longar, tampor(5)
    real(kind=8) :: alpha, bar(3), oa(3), m(3), am(3), nd(3), ps, ps1, lambda
    real(kind=8) :: h(3), oh(3), noh, cos, noa, r3(3), theta(6), eps
    real(kind=8) :: ab(2), lsta, lstb, lstc, abprim(2), prec, pre2
    real(kind=8) :: lsja(nfisc+1), lsjb(nfisc+1), lsjc, beta
    real(kind=8) :: minlsn
    integer :: j, ar(12, 3), nbar, na, nb, nc, ins
    integer :: ia, i, ipt, ibid, pp, pd, nno, k, nnos
    integer :: iadzi, iazk24, ndim, ptmax
    character(len=8) :: typma
    integer :: zxain
    aster_logical :: lcont, lajpa, lajpb, lajpc, ajout
! ----------------------------------------------------------------------
!
    call jemarq()
!
    eps=-1.0d-10
!
!   PREC PERMET D"EVITER LES ERREURS DE PRÉCISION CONDUISANT
!   A IA=IN=0 POUR LES MAILLES DU FRONT
    prec = 1000*r8prem()
    minlsn = 1*r8maem()
!
    zxain = xxmmvd('ZXAIN')
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos)
!
    if (ndim .eq. 3) then
        ptmax=6
    else if (ndim .eq. 2) then
        ptmax=2
    endif
    lcont = (enr(3:3).eq.'C').or.(enr(4:4).eq.'C')
    pre2 = 0
    if (lcont) pre2 = prec
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
    ipt=0
!       COMPTEUR DE POINT INTERSECTION = NOEUD SOMMENT
    ins=0
    call conare(typma, ar, nbar)
!
!       BOUCLE SUR LES ARETES POUR DETERMINER LES POINTS D'INTERSECTION
    do ia = 1, nbar
!
!       NUM NO DE L'ELEMENT
        na=ar(ia,1)
        nb=ar(ia,2)
        nc=ia
        lsna=lsn((na-1)*nfiss+ifiss)
        lsnb=lsn((nb-1)*nfiss+ifiss)
        if (lsna .lt. minlsn) minlsn=lsna
        if (lsnb .lt. minlsn) minlsn=lsnb
        if ((lsna*lsnb) .le. 0.d0) then
            lsta=lst((na-1)*nfiss+ifiss)
            lstb=lst((nb-1)*nfiss+ifiss)
            do 110 i = 1, ndim
                a(i)=zr(igeom-1+ndim*(na-1)+i)
                b(i)=zr(igeom-1+ndim*(nb-1)+i)
110         continue
            if (ndim .lt. 3) then
                a(3)=0.d0
                b(3)=0.d0
                c(3)=0.d0
            endif
            longar=padist(ndim,a,b)
!
            lajpa = .false.
            lajpb = .false.
            lajpc = .false.
!
            if (lsna .eq. 0.d0 .and. lsta .le. pre2) then
!           ON AJOUTE A LA LISTE LE POINT A
                lajpa = .true.
                if (lcont .and. lsta .ge. 0.d0) na=0
            endif
            if (lsnb .eq. 0.d0 .and. lstb .le. pre2) then
!           ON AJOUTE A LA LISTE LE POINT B
                lajpb = .true.
                if (lcont .and. lstb .ge. 0.d0) nb=0
            endif
            if (lsna .ne. 0.d0 .and. lsnb .ne. 0.d0) then
                beta = lsna/(lsnb-lsna)
                do 120 i = 1, ndim
                    c(i)=a(i)-beta*(b(i)-a(i))
120             continue
!           POSITION DU PT D'INTERSECTION SUR L'ARETE
                alpha=padist(ndim,a,c)
                lstc=lsta-beta*(lstb-lsta)
                if (lstc .le. prec) then
                    lajpc = .true.
                    if (lcont .and. lstc .ge. 0.d0) nc = 0
                endif
            endif
!         MODIFICATION EN TENANT COMPTE DE LA LEVEL SET JONCTION
            if (nfisc .gt. 0) then
!           POUR LES FISSURES SUR LESQUELLES IFISS SE BRANCHE
                ASSERT(na.gt.0.and.nb.gt.0.and.nc.gt.0)
                do 130 j = 1, nfisc
                    lsja(j)=lsn((na-1)*nfiss+fisco(2*j-1))*fisco(2*j)
                    lsjb(j)=lsn((nb-1)*nfiss+fisco(2*j-1))*fisco(2*j)
130             continue
                do 140 j = 1, nfisc
                    if (lajpa) then
                        if (lsja(j) .gt. pre2) lajpa = .false.
                        if (lcont .and. lsja(j) .ge. 0) na = 0
                    endif
                    if (lajpb) then
                        if (lsjb(j) .gt. pre2) lajpb = .false.
                        if (lcont .and. lsjb(j) .ge. 0) nb = 0
                    endif
                    if (lajpc) then
                        lsjc=lsja(j)-beta*(lsjb(j)-lsja(j))
                        if (lsjc .gt. prec) lajpc = .false.
                        if (lcont .and. lsjc .ge. 0) nc = 0
                    endif
140             continue
            endif
            do 150 j = nfisc+1, nfiss
!           POUR LES FISSURES QUI SE BRANCHENT SUR IFISS
                k = fisco(2*j-1)
                if (k .gt. 0) then
                    lsja(1) = lsn((na-1)*nfiss+k)
                    lsjb(1) = lsn((nb-1)*nfiss+k)
                    if (lajpa .and. abs(lsja(1)) .lt. 1d-12) then
                        na = 0
                    endif
                    if (lajpb .and. abs(lsjb(1)) .lt. 1d-12) then
                        nb = 0
                    endif
                    if (lajpc) then
                        lsjc=lsja(1)-beta*(lsjb(1)-lsja(1))
!             ON RETIENT LE NUM D'ARETE AVEC LE SIGNE -
!             POUR REPÉRER L'ARETE DANS XAINT2
                        if (abs(lsjc) .lt. 1d-12) nc = -abs(nc)
                    endif
                endif
150         continue
!
            if (lajpa) call xajpin(ndim, pinter, ptmax, ipt, ins,&
                                   a, longar, ainter, 0, na,&
                                   0.d0, ajout)
            if (lajpb) call xajpin(ndim, pinter, ptmax, ipt, ins,&
                                   b, longar, ainter, 0, nb,&
                                   0.d0, ajout)
            if (lajpc) call xajpin(ndim, pinter, ptmax, ipt, ibid,&
                                   c, longar, ainter, nc, 0,&
                                   alpha, ajout)
        endif
!
    end do
!
!     RECHERCHE SPECIFIQUE POUR LES ELEMENTS INTERSECTÉES
    if (nfisc .gt. 0) then
        call xcfacj(pinter, ptmax, ipt, ainter, lsn,&
                    igeom, nno, ndim, nfiss, ifiss,&
                    fisco, nfisc, typma)
    endif
!     RECHERCHE SPECIFIQUE POUR LES ELEMENTS EN FOND DE FISSURE
    if (enr(2:2) .eq. 'T' .or. enr(3:3) .eq. 'T') then
!
!       ON A DROIT A 1 POINT EN PLUS
        call xcfacf(pinter, ptmax+1, ipt, ainter, lsn,&
                    lst, igeom, nno, ndim, typma,&
                    noma, nmaabs)
    endif
    ninter=ipt
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
201         continue
200     continue
!
!       NORMALE A LA FISSURE (MOYENNE DE LA NORMALE AUX NOEUDS)
        call vecini(3, 0.d0, nd)
        do 210 i = 1, nno
            do 211 j = 1, 3
                nd(j)=nd(j)+zr(jgrlsn-1+3*(nfiss*(i-1)+ifiss-1)+j)/&
                nno
211         continue
210     continue
!
!       PROJECTION ET NUMEROTATION DES POINTS COMME DANS XORIFF
        call vecini(3, 0.d0, bar)
        do 220 i = 1, ninter
            do 221 j = 1, 3
                bar(j)=bar(j)+pinter((i-1)*3+j)/ninter
221         continue
220     continue
        do 230 j = 1, 3
            a(j)=pinter((1-1)*3+j)
            oa(j)=a(j)-bar(j)
230     continue
        noa=sqrt(oa(1)*oa(1) + oa(2)*oa(2) + oa(3)*oa(3))
!
!       BOUCLE SUR LES POINTS D'INTERSECTION POUR CALCULER L'ANGLE THETA
        do 240 i = 1, ninter
            do 241 j = 1, 3
                m(j)=pinter((i-1)*3+j)
                am(j)=m(j)-a(j)
241         continue
            ps=ddot(3,am,1,nd,1)
!
            ps1=ddot(3,nd,1,nd,1)
            lambda=-ps/ps1
            do 242 j = 1, 3
                h(j)=m(j)+lambda*nd(j)
                oh(j)=h(j)-bar(j)
242         continue
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
240     continue
!
!       TRI SUIVANT THETA CROISSANT
        do 250 pd = 1, ninter-1
            pp=pd
            do 251 i = pp, ninter
                if (theta(i) .lt. theta(pp)) pp=i
251         continue
            tampor(1)=theta(pp)
            theta(pp)=theta(pd)
            theta(pd)=tampor(1)
            do 252 k = 1, 3
                tampor(k)=pinter(3*(pp-1)+k)
                pinter(3*(pp-1)+k)=pinter(3*(pd-1)+k)
                pinter(3*(pd-1)+k)=tampor(k)
252         continue
            do 253 k = 1, zxain
                tampor(k)=ainter(zxain*(pp-1)+k)
                ainter(zxain*(pp-1)+k)=ainter(zxain*(pd-1)+k)
                ainter(zxain*(pd-1)+k)=tampor(k)
253         continue
250     continue
!
500     continue
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
801         continue
800     continue
        if (ninter .eq. 2) then
!         NORMALE A LA FISSURE (MOYENNE DE LA NORMALE AUX NOEUDS)
            call vecini(2, 0.d0, nd)
            do 810 i = 1, nno
                do 811 j = 1, 2
                    nd(j)=nd(j)+zr(jgrlsn-1+2*(nfiss*(i-1)+ifiss-1)+j)&
                    /nno
811             continue
810         continue
!
            do 841 j = 1, 2
                a(j)=pinter(j)
                b(j)=pinter(2+j)
                ab(j)=b(j)-a(j)
841         continue
!
            abprim(1)=-ab(2)
            abprim(2)=ab(1)
!
            if (ddot(2,abprim,1,nd,1) .lt. 0.d0) then
                do 852 k = 1, 2
                    tampor(k)=pinter(k)
                    pinter(k)=pinter(2+k)
                    pinter(2+k)=tampor(k)
852             continue
                do 853 k = 1, 4
                    tampor(k)=ainter(k)
                    ainter(k)=ainter(zxain+k)
                    ainter(zxain+k)=tampor(k)
853             continue
            endif
            nface=1
            nptf=2
            cface(1,1)=1
            cface(1,2)=2
        else
            nptf=0
            nface=0
        endif
!
    else
!       PROBLEME DE DIMENSION : NI 2D, NI 3D
        ASSERT(ndim.eq.2 .or. ndim.eq.3)
    endif
    if (nfiss .gt. 1 .and. minlsn .eq. 0) nface = 0
    if (nface .eq. 0) ninter = 0
    call jedema()
end subroutine
