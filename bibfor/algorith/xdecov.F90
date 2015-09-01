subroutine xdecov(ndim, elp, nnop, nnose, it,&
                  pintt, cnset, heavt, ncomp, lsn,&
                  fisco, igeom, nfiss, ifiss, pinter,&
                  ninter, npts, ainter, nse, cnse,&
                  heav, nfisc, nsemax)
! aslint: disable=W1306,W1504
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/lteatt.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xpente.h"
#include "asterfort/xxmmvd.h"
#include "blas/ddot.h"
    real(kind=8) :: lsn(*), pintt(*), pinter(*), ainter(*)
    integer :: ndim, nnop, nnose, it, cnset(*), heavt(*), ncomp, igeom
    integer :: ninter, npts, nfiss, ifiss, nse, cnse(6, 10), fisco(*)
    integer :: nsemax, nfisc
    real(kind=8) :: heav(*)
    character(len=8) :: elp
!     ------------------------------------------------------------------
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
! person_in_charge: samuel.geniaut at edf.fr
!                      DÃCOUPER LE TETRA EN NSE SOUS-TETRAS
!
!     ENTREE
!       NNOSE    : NOMBRE DE NOEUDS DU SOUS TETRA
!       IT       : INDICE DU TETRA EN COURS
!       CNSET    : CONNECTIVITÃ DES NOEUDS DU TETRA
!       HEAVT    : FONCTION HEAVYSIDE DES TETRAS
!       LSN      : VALEURS DE LA LEVEL SET NORMALE
!       IGEOM    : ADRESSE DES COORDONNÃES DES NOEUDS DE L'ELT PARENT
!       PINTER   : COORDONNÃES DES POINTS D'INTERSECTION
!       NINTER   : NB DE POINTS D'INTERSECTION
!       NPTS     : NB DE PTS D'INTERSECTION COINCIDANT AVEC UN NOEUD
!       AINTER   : INFOS ARETE CORRESPONDATE AU PT INTERSECTION
!       NSEMAX   : NOMBRE DE SOUS-ELT MAX (TETRAS)
!
!     SORTIE
!       NSE      : NOMBRE DE SOUS-ELTS (TETRAS)
!       CNSE     : CONNECTIVITE DES SOUS-ÃLÃMENTS (TETRAS)
!       HEAV     : FONCTION HEAVYSIDE CONSTANTE SUR CHAQUE SOUS-ELT
!     ------------------------------------------------------------------
!
    real(kind=8) :: xyz(4, 3), ab(3), ac(3), ad(3), vn(3), ps, geom(3)
    real(kind=8) :: somlsn(nfisc+1), ff(nnop), rbid2(ndim)
    integer :: in, inh, i, j, ar(12, 3), nbar, ise, npent(18)
    integer :: a1, a2, a3, a4, a, b, c, d, ndime
    character(len=8) :: typma, elrese(3)
    integer :: zxain, mxstac
    aster_logical :: axi
    parameter      (mxstac=1000)
!
    data            elrese /'SEG2','TRIA3','TETRA4'/
! ----------------------------------------------------------------------
!
!
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
    ASSERT(nnop.le.mxstac)
    ASSERT(nfisc.le.mxstac)
    ASSERT(ndim.le.mxstac)
!
!
    call elrefe_info(fami='RIGI', ndim=ndime)
    zxain = xxmmvd('ZXAIN')
!
    axi = lteatt('AXIS','OUI')
!     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
!     NDIM EST LA DIMENSION DU MAILLAGE
!     NDIME EST DIMENSION DE L'ELEMENT FINI
!     PAR EXEMPLE, POUR LES ELEMENT DE BORDS D'UN MAILLAGE 3D :
!     NDIME = 2 ALORS QUE NDIM = 3
!
    do 10 in = 1, 6
        do 20 j = 1, 6
            cnse(in,j)=0
 20     continue
 10 end do
!
    typma=elrese(ndime)
    call conare(typma, ar, nbar)
!
!-----------------------------------------------------------------------
!     REMPLISSAGE DE LA CONNECTIVITÃ DES SOUS-ELEMENTS TÃTRAS
!                  ALGO BOOK III (26/04/04)
!-----------------------------------------------------------------------
!
    if (ndime .eq. 2) then
!
!
        if (ninter .lt. 2) then
!         INTER DOUTEUSE
            ASSERT(npts.eq.ninter)
!         1 SEUL ELEMENT
            nse=1
            do 90 in = 1, nnose
                cnse(1,in)=cnset(nnose*(it-1)+in)
 90         continue
        else if (ninter .eq. 2) then
            a1=nint(ainter(zxain*(1-1)+1))
            a2=nint(ainter(zxain*(2-1)+1))
            if (npts .eq. 2) then
!           1 SEUL ELEMENT
                nse=1
                do 91 in = 1, nnose
                    cnse(1,in)=cnset(nnose*(it-1)+in)
 91             continue
            else if (npts .eq. 1) then
!           2 ELEMENTS
                nse=2
                ASSERT(a1.eq.0.and.a2.ne.0)
                cnse(1,1)=nint(ainter(zxain*(npts-1)+2))
                cnse(1,2)=102
                cnse(1,3)=cnset(nnose*(it-1)+ar(a2,1))
                cnse(2,1)=nint(ainter(zxain*(npts-1)+2))
                cnse(2,2)=102
                cnse(2,3)=cnset(nnose*(it-1)+ar(a2,2))
            else
!           3 ELEMENTS
                nse=3
                ASSERT(a1.ne.0)
!           101 ET 102 LES 2 POINTS D'INTERSECTION
!           ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
                do 93 i = 1, 2
                    do 94 j = 1, 2
                        if (ar(a1,i) .eq. ar(a2,j)) then
                            a=ar(a1,i)
                            b=ar(a1,3-i)
                            c=ar(a2,3-j)
                        endif
 94                 continue
 93             continue
                cnse(1,1)=101
                cnse(1,2)=102
                cnse(1,3)=cnset(nnose*(it-1)+a)
                cnse(2,1)=101
                cnse(2,2)=102
                cnse(2,3)=cnset(nnose*(it-1)+c)
                cnse(3,1)=101
                cnse(3,2)=cnset(nnose*(it-1)+b)
                cnse(3,3)=cnset(nnose*(it-1)+c)
            endif
        else if (ninter .eq. 3) then
!         L'INTERFACE COINCIDE AVEC LE TRIA
            ASSERT(npts.eq.ninter)
!         1 SEUL ELEMENT
            nse=1
            do 92 in = 1, nnose
                cnse(1,in)=cnset(nnose*(it-1)+in)
 92         continue
        else
!         TROP DE POINTS D'INTERSECTION
            ASSERT(ninter.le.3)
        endif
!
    else if (ndime .eq. 1) then
!
        if (ninter .lt. 1) then
!         INTER DOUTEUSE
            ASSERT(npts.eq.ninter)
!         1 SEUL ELEMENT
            nse=1
            do 95 in = 1, 2
                cnse(1,in)=cnset(nnose*(it-1)+in)
 95         continue
        else if (ninter .eq. 1) then
            a1=nint(ainter(zxain*(1-1)+1))
            if (npts .eq. 1) then
!           1 SEUL ELEMENT
                nse=1
                do 96 in = 1, 2
                    cnse(1,in)=cnset(nnose*(it-1)+in)
 96             continue
            else if (npts .eq. 0) then
!           2 ELEMENTS
                nse=2
                ASSERT(a1.ne.0)
                a=ar(a1,1)
                b=ar(a1,2)
!
!           101 ET 102 LES 2 POINTS D'INTERSECTION
!           ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
                cnse(1,1)=101
                cnse(1,2)=cnset(nnose*(it-1)+a)
                cnse(2,1)=101
                cnse(2,2)=cnset(nnose*(it-1)+b)
            endif
        else if (ninter .eq. 2) then
!         L'INTERFACE COINCIDE AVEC LE SEG
            ASSERT(npts.eq.ninter)
!         1 SEUL ELEMENT
            nse=1
            do 97 in = 1, 2
                cnse(1,in)=cnset(nnose*(it-1)+in)
 97         continue
        else
!         TROP DE POINTS D'INTERSECTION
            ASSERT(ninter.le.2)
        endif
!
!
!
    else if (ndime.eq.3) then
!
        do 98 i = 1, 18
            npent(i)=0
 98     continue
        if (ninter .lt. 3) then
!
!       1Â°) AVEC MOINS DE TROIS POINTS D'INTERSECTION
!       ---------------------------------------------
!
!         INTER DOUTEUSE
            ASSERT(npts.eq.ninter)
!         ON A UN SEUL ELEMENT
            nse=1
            do 100 in = 1, nnose
                cnse(1,in)=cnset(nnose*(it-1)+in)
100         continue
!
        else if (ninter.eq.3) then
!
!         2Â°) AVEC TROIS POINTS D'INTERSECTION
!         ------------------------------------
            a1=nint(ainter(zxain*(1-1)+1))
            a2=nint(ainter(zxain*(2-1)+1))
            a3=nint(ainter(zxain*(3-1)+1))
!
            if (npts .eq. 3) then
!           ON A UN SEUL ELEMENT
                nse=1
                do 110 in = 1, nnose
                    cnse(1,in)=cnset(nnose*(it-1)+in)
110             continue
!
            else if (npts.eq.2) then
!           ON A DEUX SOUS-ELEMENTS
                nse=2
                ASSERT(a1.eq.0.and.a2.eq.0)
                ASSERT(nint(ainter(2)) .gt. 0 .and. nint(ainter( zxain+2)) .gt. 0)
!
!           CONNECTIVITE DES NSE PAR RAPPORT AU NUM DE NOEUDS DU PARENT
!           AVEC 101, 102 ET 103 LES 3 PTS D'INTERSECTION
!           ON REMPLACE 101 ET 102 PAR LES NUMEROS DES NOEUDS COUPÉS
                cnse(1,1)=nint(ainter(2))
                cnse(1,2)=nint(ainter(zxain+2))
                cnse(1,3)=103
                cnse(1,4)=cnset(nnose*(it-1)+ar(a3,1))
                cnse(2,1)=nint(ainter(2))
                cnse(2,2)=nint(ainter(zxain+2))
                cnse(2,3)=103
                cnse(2,4)=cnset(nnose*(it-1)+ar(a3,2))
!
            else if (npts.eq.1) then
!           ON A TROIS SOUS-ELEMENTS
                nse=3
                ASSERT(a1.eq.0.and.a2.ne.0)
                ASSERT(nint(ainter(2)).gt.0)
!           ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
                do 30 i = 1, 2
                    do 40 j = 1, 2
                        if (ar(a2,i) .eq. ar(a3,j)) then
                            a=ar(a2,i)
                            b=ar(a2,3-i)
                            c=ar(a3,3-j)
                        endif
 40                 continue
 30             continue
!           ON REMPLACE 101 PAR LE NUMERO DU NOEUD COUPÉ
                cnse(1,1)=nint(ainter(2))
                cnse(1,2)=102
                cnse(1,3)=103
                cnse(1,4)=cnset(nnose*(it-1)+a)
                cnse(2,1)=nint(ainter(2))
                cnse(2,2)=102
                cnse(2,3)=103
                cnse(2,4)=cnset(nnose*(it-1)+c)
                cnse(3,1)=nint(ainter(2))
                cnse(3,2)=102
                cnse(3,3)=cnset(nnose*(it-1)+b)
                cnse(3,4)=cnset(nnose*(it-1)+c)
!
            else if (npts.eq.0) then
!           ON A QUATRE SOUS-ELEMENTS
                nse=4
                a=0
                b=0
                c=0
                d=0
                do 38 i = 1, 2
                    do 48 j = 1, 2
                        if (ar(a1,i) .eq. ar(a2,j)) then
                            a=ar(a1,i)
                            b=ar(a1,3-i)
                            c=ar(a2,3-j)
                        endif
 48                 continue
 38             continue
                do 39 i = 1, 2
                    if (ar(a3,i) .ne. a) then
                        d=ar(a3,i)
                    endif
 39             continue
                ASSERT((a*b*c*d).gt.0)
!
                cnse(1,1)=101
                cnse(1,2)=102
                cnse(1,3)=103
                cnse(1,4)=cnset(nnose*(it-1)+a)
!
                npent(1)=103
                npent(2)=101
                npent(3)=102
                npent(4)=cnset(nnose*(it-1)+d)
                npent(5)=cnset(nnose*(it-1)+b)
                npent(6)=cnset(nnose*(it-1)+c)
                call xpente(2, cnse, npent)
            endif
!
        else if (ninter.eq.4) then
!
!         2Â°) AVEC QUATRE POINTS D'INTERSECTION
!          -------------------------------------
            a1=nint(ainter(zxain*(1-1)+1))
            a2=nint(ainter(zxain*(2-1)+1))
            a3=nint(ainter(zxain*(3-1)+1))
            a4=nint(ainter(zxain*(4-1)+1))
!
!         ON A SIX SOUS-ELEMENTS (DANS TOUS LES CAS ?)
            nse=6
            ASSERT((a1*a2*a3*a4).gt.0)
            a=0
            b=0
            c=0
            d=0
            do 78 i = 1, 2
                do 88 j = 1, 2
                    if (ar(a1,i) .eq. ar(a2,j)) then
                        a=ar(a1,i)
                        b=ar(a1,3-i)
                        c=ar(a2,3-j)
                    endif
                    if (ar(a3,i) .eq. ar(a4,j)) then
                        d=ar(a3,i)
                    endif
 88             continue
 78         continue
            ASSERT((a*b*c*d).gt.0)
            npent(1)=104
            npent(2)=102
            npent(3)=cnset(nnose*(it-1)+c)
            npent(4)=103
            npent(5)=101
            npent(6)=cnset(nnose*(it-1)+b)
            call xpente(1, cnse, npent)
            npent(1)=cnset(nnose*(it-1)+a)
            npent(2)=101
            npent(3)=102
            npent(4)=cnset(nnose*(it-1)+d)
            npent(5)=103
            npent(6)=104
            call xpente(4, cnse, npent)
        endif
    endif
!
!-----------------------------------------------------------------------
!     VÃRIFICATION DU SENS DES SOUS-ÃLÃMENTS TETRA
!                  ALGO BOOK III (28/04/04)
!-----------------------------------------------------------------------
!
    if (ndime .eq. 3) then
!
        do 200 ise = 1, nse
            do 210 in = 1, 4
                inh=cnse(ise,in)
                if (inh .lt. 100) then
                    do 220 j = 1, 3
                        xyz(in,j)=zr(igeom-1+ndim*(inh-1)+j)
220                 continue
                else if (inh.gt.100.and.inh.lt.1000) then
                    do 221 j = 1, 3
                        xyz(in,j)=pinter(ndim*(inh-100-1)+j)
221                 continue
                else
                    do 222 j = 1, 3
                        xyz(in,j)=pintt(ndim*(inh-1001)+j)
222                 continue
                endif
210         continue
!
            do 230 j = 1, 3
                ab(j)=xyz(2,j)-xyz(1,j)
                ac(j)=xyz(3,j)-xyz(1,j)
                ad(j)=xyz(4,j)-xyz(1,j)
230         continue
!
            call provec(ab, ac, vn)
            ps=ddot(3,vn,1,ad,1)
!
            if (ps .lt. 0) then
!          MAUVAIS SENS DU TETRA, ON INVERSE LES NOEUDS 3 ET 4
                inh=cnse(ise,3)
                cnse(ise,3)=cnse(ise,4)
                cnse(ise,4)=inh
            endif
!
200     continue
!
!
    endif
!
!-----------------------------------------------------------------------
!             MATRICE DES COORDONNÃES ET FONCTION HEAVYSIDE
!             ALGO BOOK III (28/04/04)
!-----------------------------------------------------------------------
    ASSERT(nse.le.nsemax)
    do 300 ise = 1, nse
        do 310 i = 1, ifiss-1
! ----- ON RECOPIE LES VALEURS PRÉCÉDENTES
            heav(ifiss*(ise-1)+i)=heavt(ncomp*(i-1)+it)
310     continue
! ----- ON TRAITE LA FISSURE COURANTE
        call vecini(nfisc+1, 0.d0, somlsn)
        do 320 in = 1, nnose
            inh=cnse(ise,in)
            if (inh .lt. 100) then
                do 325 i = 1, nfisc
                    somlsn(i) = somlsn(i)+lsn((inh-1)*nfiss+fisco(2*i- 1))
325             continue
                somlsn(nfisc+1) = somlsn(nfisc+1)+lsn((inh-1)*nfiss+ ifiss)
            else
!           RECUP DE LA GÉOMETRIE
                if (inh .gt. 1000) then
                    do 330 j = 1, ndim
                        geom(j) = pintt(ndim*(inh-1001)+j)
330                 continue
                else if (inh.lt.1000) then
                    do 340 j = 1, ndim
                        geom(j) = pinter(ndim*(inh-101)+j)
340                 continue
                endif
!           CALCUL DES FF
!
!
                call reeref(elp, nnop, zr(igeom), geom, ndim,&
                            rbid2, ff)
!
                do 350 j = 1, nnop
                    do 355 i = 1, nfisc
                        somlsn(i)=somlsn(i)+ff(j)*lsn((j-1)*nfiss+&
                        fisco(2*i-1))
355                 continue
                    somlsn(nfisc+1) = somlsn(nfisc+1)+ff(j) *lsn((j-1) *nfiss+ifiss)
350             continue
            endif
320     continue
!
!       MISE À ZERO POUR LA FONCTION JONCTION AU NIVEAU DU BRANCHEMENT
!
        do 360 i = 1, nfisc
            if (fisco(2*i)*somlsn(i) .gt. 0.d0) goto 300
360     continue
!
        if (somlsn(nfisc+1) .lt. 0.d0) then
            heav(ifiss*ise) = -1.d0
        else if (somlsn(nfisc+1).gt.0.d0) then
            heav(ifiss*ise) = +1.d0
        else
!       REMARQUE IMPORTANTE :
!       SI ON EST SUR UN ELEMENT DE BORD COINCIDANT AVEC L'INTERCE
!       (NDIME = NDIM - 1 ET NPTS = NINTER = NDIM) ALORS ON NE PEUT PAS
!       DÃTERMINER DE QUEL COTÃ DE L'INTERFACE ON SE TROUVE, CAR
!       ON EST TOUJOURS SUR L'INTERFACE. LA VALEUR DE HEAV(ISE)
!       EST DONC FAUSSE DANS CE CAS : ON MET 99.
!       UNE CORRECTION EST FAITE DANS XORIPE LORS DE L'ORIENTATION DES
!       NORMALES, OU ON EN PROFITE POUR CORRIGER AUSSI HEAV(ISE)
            ASSERT(ndime.eq.ndim-1.and.npts.eq.ndim.and.nse.eq.1)
            heav(ifiss*ise) = 99.d0
        endif
!
300 end do
!
end subroutine
