subroutine xcfaq2(jlsn, jlst, jgrlsn, igeom, noma,&
                  nmaabs, pinter, ainter, nface,&
                  nptf, cface, nbtot, nfiss, ifiss)
    implicit none
!
#include "asterf_types.h"
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
#include "asterfort/elrefe_info.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/loncar.h"
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
    integer :: jgrlsn, igeom, nface, cface(18, 6), jlsn, jlst
    integer :: nfiss, ifiss, nptf, nbtot, nmaabs
    real(kind=8) :: pinter(*), ainter(*)
    character(len=8) :: noma
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
    real(kind=8) :: alpha, nd(3), coor2d(9)
    real(kind=8) :: ab(2), lsta, lstb, lstc, abprim(2), prec, lonref, cridist
    real(kind=8) :: eps
    real(kind=8) :: ff(27), ksic(3), sc, tabar(9), minlsn, maxlsn
    real(kind=8) :: m(3), lsnm, lstm, ksi, milfi(3), smilfi, lsnabs
    integer :: j, ar(12, 3), nbar, na, nb, ins, n(3)
    integer :: ia, i, ipt, ibid, nno, k
    integer :: iadzi, iazk24, ndim, ptmax
    integer :: zxain
    integer :: inm, inc, nm, nbnomx, ninter
    parameter(cridist=1.d-7)
    aster_logical :: cut, ajout, arete
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
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno)
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
    call tecael(iadzi, iazk24, noms=0)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
    call loncar(ndim, typma, zr(igeom), lonref)
!
!     L'ELEMENT EST-IL TRAVERSE STRICTEMENT PAR LSN=0?
    nbtot = 0
    cut=.false.
    i=1
 1  continue
!     (1) RECHERCHE D'UN NOEUD PIVOT (LSN NON NULLE)
    if (zr(jlsn-1+(i-1)*nfiss+ifiss) .ne. 0.d0 .and. i .lt. nno) then
        do 30 k = i+1, nno
!     (2) PRODUIT DE CE PIVOT PAR LES AUTRES LSN
            if (zr(jlsn-1+(i-1)*nfiss+ifiss)*zr(jlsn-1+(k-1)*nfiss+ifiss) .lt. 0.d0) cut=.true.
30      continue
    else if (i.lt.nno) then
        i=i+1
        goto 1
    endif
!     RECHERCHE DE MINLSN
    minlsn = 0.d0
    maxlsn = 0.d0
    do i = 1, nno
        minlsn=min(zr(jlsn-1+(i-1)*nfiss+ifiss),minlsn)
        maxlsn=max(zr(jlsn-1+(i-1)*nfiss+ifiss),maxlsn)
    end do
!
!     ON NE PREND QUE CERTAINS ELEMENTS POUR NE PAS AVOIR DE "DOUBLONS"
    arete = .false.
    lsnabs = 0.d0
    if(.not.cut) then
       call conare(typma, ar, nbar)
       do i = 1, nbar
           lsnabs = abs(zr(jlsn-1+(ar(i,1)-1)*nfiss+ifiss))+abs(zr(jlsn-1+(ar(i,2)-1)*nfiss+ifiss))
           if (lsnabs.le.cridist*lonref) arete = .true.
       end do
       if (.not.arete) goto 999
       if (arete.and.minlsn.ge.0.d0) goto 999
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
110     continue
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
307             continue
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
100 continue
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
        do 20 j = 1, 3
            n(j)=0
 20     continue
! LE NEWTON NE PEUT PAS CONVERGER QUAND NDIM=3
! IL NOUS MANQUE L INFORMATION SUR LA FACE DE L ELT PARENT
! TRANSPORTEE PAS N(3)
        ASSERT(ndim.lt.3)
!       RECHERCHE POINT MILIEU FISSURE
        call xmilfi(elp, n, ndim, nno, pinter,&
                    ndim, igeom, jlsn, 1, 2,&
                    milfi)
        do 10 j = 1, ndim
            coor2d(j)=pinter(j)
            coor2d(ndim+j)=pinter(ndim+j)
            coor2d(2*ndim+j)=milfi(j)
 10     continue
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
!     CAS 2D
    if (ndim .eq. 2) then
!
        do 800 i = 1, 18
            do 801 j = 1, 5
                cface(i,j)=0
801         continue
800     continue
        if (ninter .eq. 2) then
!         NORMALE A LA FISSURE (MOYENNE DE LA NORMALE AUX NOEUDS)
            call vecini(2, 0.d0, nd)
            do 810 i = 1, nno
                do 811 j = 1, 2
                    nd(j)=nd(j)+zr(jgrlsn-1+2*(i-1)+j)/nno
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
        ASSERT(ndim.eq.2)
    endif
!
999 continue
    call jedema()
end subroutine
