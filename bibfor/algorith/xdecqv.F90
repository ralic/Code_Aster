subroutine xdecqv(nnose, it, cnset, lsn, igeom,&
                  ninter, npts, ainter, nse, cnse,&
                  heav, nsemax)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/elref4.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndcent.h"
#include "asterfort/tecael.h"
#include "asterfort/xxmmvd.h"
    integer :: nnose, it, cnset(*), igeom, ninter, npts, nse, cnse(6, 6)
    integer :: nsemax
    real(kind=8) :: lsn(*), ainter(*), heav(*)
!
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
!                      DÉCOUPER LE TETRA EN NSE SOUS-TETRAS
!
!     ENTREE
!       NNOSE    : NOMBRE DE NOEUDS DU SOUS TETRA
!       IT       : INDICE DU TETRA EN COURS
!       CNSET    : CONNECTIVITÉ DES NOEUDS DU TETRA
!       LSN      : VALEURS DE LA LEVEL SET NORMALE
!       IGEOM    : ADRESSE DES COORDONNÉES DES NOEUDS DE L'ELT PARENT
!       NINTER   : NB DE POINTS D'INTERSECTION
!       NPTS     : NB DE PTS D'INTERSECTION COINCIDANT AVEC UN NOEUD
!                  SOMMET
!       AINTER   : INFOS ARETE CORRESPONDATE AU PT INTERSECTION
!
!     SORTIE
!       NSE      : NOMBRE DE SOUS-ÉLÉMENTS (TÉTRAS)
!       CNSE     : CONNECTIVITÉ DES SOUS-ÉLÉMENTS (TÉTRAS)
!       HEAV     : FONCTION HEAVYSIDE CONSTANTE SUR CHAQUE SOUS-ÉLÉMENT
!     ----------------------------------------------------------------
!
    real(kind=8) :: x(3), xlsn, lsnk
    real(kind=8) :: r8prem
    integer :: in, inh, i, j, ar(12, 3), nbar, ise, ndim, ibid
    integer :: a1, a2, a, b, c, iadzi, iazk24, ndime, jdim
    integer :: k, e, e2, nnop
    integer :: zxain
    character(len=8) :: typma, noma, elrese(3)
    logical :: cut
!
    data            elrese /'SEG3','TRIA6','TETRA10'/
! --------------------------------------------------------------------
    call jemarq()
!
    call elref4(' ', 'RIGI', ndime, nnop, ibid,&
                ibid, ibid, ibid, ibid, ibid)
    zxain = xxmmvd('ZXAIN')
    call tecael(iadzi, iazk24)
    noma=zk24(iazk24)
    call jeveuo(noma//'.DIME', 'L', jdim)
    ndim=zi(jdim-1+6)
!
!     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
!     NDIM EST LA DIMENSION DU MAILLAGE
!     NDIME EST DIMENSION DE L'ELEMENT FINI
!     PAR EXEMPLE, POUR LES ELEMENT DE BORDS D'UN MAILLAGE 3D :
!     NDIME = 2 ALORS QUE NDIM = 3
!
    do 10 in = 1, 6
        do 20 j = 1, 6
            cnse(in,j)=0
20      continue
10  end do
!
    typma=elrese(ndime)
!
!     CALCUL DES COORDONNEES ET LSN DU NOEUD 9
    if (nnop .eq. 8) then
        call ndcent(igeom, lsn, x, xlsn)
    endif
!
    call conare(typma, ar, nbar)
!
    cut=.false.
    i=1
!     (1) RECHERCHE D'UN NOEUD PIVOT (LSN NON NULLE)
 1  continue
    if (i .lt. nnose) then
        if (cnset(nnose*(it-1)+i) .eq. 9 .or. lsn(cnset(nnose*(it-1)+i)) .eq. 0.d0) then
            i=i+1
            goto 1
        endif
    endif
!     (2) PRODUIT DE CE PIVOT PAR LES AUTRES LSN
    k=i+1
30  continue
    if (k .le. nnose) then
!       RECUPERATION DE LSN(K) :
!         - CAS PARTICULIER DU NOEUD CENTRAL D'UN Q9
        if (cnset(nnose*(it-1)+k) .eq. 9) then
            lsnk=xlsn
!         - CAS GENERAL
        else
            lsnk=lsn(cnset(nnose*(it-1)+k))
        endif
!
        if (lsn(cnset(nnose*(it-1)+i))*lsnk .lt. 0.d0) then
            cut=.true.
        else
            k=k+1
            goto 30
        endif
    endif
!
!     STOCKAGE DE LA CONNECTIVITE D'UN SOUS-ELEMENT NON COUPE
    if (.not.cut) then
        nse=1
        do 31 in = 1, nnose
            cnse(1,in)=cnset(nnose*(it-1)+in)
31      continue
    endif
!
! --------------------------------------------------------------------
!     REMPLISSAGE DE LA CONNECTIVITÉ DES SOUS-ELEMENTS TÉTRAS
!                  ALGO BOOK III (26/04/04)
! --------------------------------------------------------------------
    if (ndime .eq. 2 .and. cut) then
!
        if (ninter .lt. 2) then
!       PAS DE DECOUPAGE
!         1 SEUL ELEMENT
            nse=1
            do 40 in = 1, nnose
                cnse(1,in)=cnset(nnose*(it-1)+in)
40          continue
!
        else if (ninter .eq.2) then
            a1=nint(ainter(zxain*(1-1)+1))
            a2=nint(ainter(zxain*(2-1)+1))
            if (npts .eq. 0) then
!         DECOUPAGE EN 3 ELEMENTS
!         3 SEUL ELEMENT
                nse=3
                ASSERT(a1.ne.0)
!           101 ET 102 LES 2 POINTS D'INTERSECTION
!           ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
!
                do 50 i = 1, 2
                    do 51 j = 1, 2
                        if (ar(a1,i) .eq. ar(a2,j)) then
                            a=ar(a1,i)
                            b=ar(a1,3-i)
                            c=ar(a2,3-j)
                            e=ar(6-(a1+a2),3)
                        endif
51                  continue
50              continue
                cnse(1,1)=101
                cnse(1,2)=102
                cnse(1,3)=cnset(nnose*(it-1)+a)
                cnse(1,4)=205
                cnse(1,5)=204
                cnse(1,6)=202
                cnse(2,1)=101
                cnse(2,2)=102
                cnse(2,3)=cnset(nnose*(it-1)+c)
                cnse(2,4)=205
                cnse(2,5)=203
                cnse(2,6)=206
                cnse(3,1)=101
                cnse(3,2)=cnset(nnose*(it-1)+b)
                cnse(3,3)=cnset(nnose*(it-1)+c)
                cnse(3,4)=201
                cnse(3,5)=cnset(nnose*(it-1)+e)
                cnse(3,6)=206
!
            else if (npts .eq.1) then
!         DECOUPAGE EN 2 ELEMENTS
!         2 SEUL ELEMENT
                nse=2
                ASSERT(a1.eq.0.and.a2.ne.0)
!           101 ET 102 LES 2 POINTS D'INTERSECTION
!           CNSE(1,1)=101
                b = ar(a2,1)
                c = ar(a2,2)
                if (a2 .eq. 1) then
                    e=6
                    e2=5
                else if (a2.eq.2) then
                    e=4
                    e2=6
                else if (a2.eq.3) then
                    e=5
                    e2=4
                endif
                cnse(1,1)=nint(ainter(zxain*(npts-1)+2))
                cnse(1,2)=102
                cnse(1,3)=cnset(nnose*(it-1)+b)
                cnse(1,4)=203
                cnse(1,5)=202
                cnse(1,6)=cnset(nnose*(it-1)+e)
                cnse(2,1)=nint(ainter(zxain*(npts-1)+2))
                cnse(2,2)=102
                cnse(2,3)=cnset(nnose*(it-1)+c)
                cnse(2,4)=203
                cnse(2,5)=201
                cnse(2,6)=cnset(nnose*(it-1)+e2)
!
            else if (npts .ge.2) then
!         PAS DE DECOUPAGE
!         1 SEUL ELEMENT
                nse=1
                do 60 in = 1, nnose
                    cnse(1,in)=cnset(nnose*(it-1)+in)
60              continue
            endif
!
        else if (ninter .eq.3) then
!
            if (npts .eq. 0) then
!           PAS DE DECOUPAGE
!           1 SEUL ELEMENT
                nse=1
                do 70 in = 1, nnose
                    cnse(1,in)=cnset(nnose*(it-1)+in)
70              continue
!
            else if (npts .eq.1) then
!           DECOUPAGE EN 3 ELEMENTS
!           3 ELEMENTS
                nse=3
                a1=nint(ainter(zxain*(2-1)+1))
                a2=nint(ainter(zxain*(3-1)+1))
!           ON PLACE A,B,C SUR LE TRIA
                do 80 i = 1, 2
                    do 81 j = 1, 2
                        if (ar(a1,i) .eq. ar(a2,j)) then
                            a=ar(a1,i)
                            b=ar(a1,3-i)
                            c=ar(a2,3-j)
                            e=ar(6-(a1+a2),3)
                        endif
81                  continue
80              continue
                cnse(1,1)=102
                cnse(1,2)=103
                cnse(1,3)=cnset(nnose*(it-1)+a)
                cnse(1,4)=207
                cnse(1,5)=205
                cnse(1,6)=203
                cnse(2,1)=102
                cnse(2,2)=103
                cnse(2,3)=cnset(nnose*(it-1)+c)
                cnse(2,4)=207
                cnse(2,5)=204
                cnse(2,6)=206
                cnse(3,1)=102
                cnse(3,2)=cnset(nnose*(it-1)+b)
                cnse(3,3)=cnset(nnose*(it-1)+c)
                cnse(3,4)=202
                cnse(3,5)=cnset(nnose*(it-1)+e)
                cnse(3,6)=206
!
            else if (npts .ge.2) then
!         PAS DE DECOUPAGE
!         1 SEUL ELEMENT
                nse=1
                do 90 in = 1, nnose
                    cnse(1,in)=cnset(nnose*(it-1)+in)
90              continue
!
            endif
!           ENDIF SUR NPTS DE NINTER=3
!
        else
!
!         1 SEUL ELEMENT
            nse=1
            do 100 in = 1, nnose
                cnse(1,in)=cnset(nnose*(it-1)+in)
100          continue
!
        endif
!
    else if (ndime .eq. 1 .and. cut) then
!
        if (ninter .lt. 1) then
!         PAS DE DECOUPAGE
!         1 SEUL ELEMENT
            nse=1
            do 110 in = 1, nnose
                cnse(1,in)=cnset(nnose*(it-1)+in)
110          continue
!
        else if (ninter .eq.1) then
            a1=nint(ainter(zxain*(1-1)+1))
            if (npts .eq. 0) then
!          DECOUPAGE EN 2 ELEMENTS
                nse=2
                ASSERT(a1.ne.0)
                a=ar(a1,1)
                b=ar(a1,2)
!            101 LE POINT D'INTERSECTION
!            ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
                cnse(1,1)=101
                cnse(1,2)=cnset(nnose*(it-1)+a)
                cnse(1,3)=202
                cnse(2,1)=101
                cnse(2,2)=cnset(nnose*(it-1)+b)
                cnse(2,3)=201
            endif
!
        else if (ninter .ge.2) then
!        PAS DE DECOUPAGE
!        1 SEUL ELEMENT
            nse=1
            do 120 in = 1, nnose
                cnse(1,in)=cnset(nnose*(it-1)+in)
120          continue
!
        endif
    endif
!
! --------------------------------------------------------------------
!             MATRICE DES COORDONNÉES ET FONCTION HEAVYSIDE
!             ALGO BOOK III (28/04/04)
! --------------------------------------------------------------------
!
    ASSERT(nse.le.nsemax)
    do 300 ise = 1, nse
        heav(ise)=1.d0
        do 310 in = 1, ndime+1
            inh=cnse(ise,in)
            if (inh .lt. 100) then
                if (lsn(inh) .lt. 0.d0) heav(ise)=-1.d0
            endif
310      continue
300  end do
!
!     REMARQUE IMPORTANTE :
!     SI ON EST SUR UN ELEMENT DE BORD COINCIDANT AVEC L'INTERCE
!     (NDIME = NDIM - 1 ET NPTS = NDIM) ALORS ON NE PEUT PAS
!     DÉTERMINER DE QUEL COTE DE L'INTERFACE ON SE TROUVE, CAR ON
!     EST TOUJOURS SUR L'INTERFACE. LA VALEUR DE HEAV(ISE)
!     EST DONC FAUSSE DANS CE CAS : ON MET 99.
!     UNE CORRECTION EST FAITE DANS XORIPE LORS DE L'ORIENTATION
!     DES NORMALES, OU ON EN PROFITE POUR CORRIGER AUSSI HEAV(ISE)
!     CONTRAIREMENT AU CAS LINEAIRE, ON N'A PAS NPTS = NINTER. EN 2D,
!     ON CONSIDERE DES SEG3 (NPTS = 2 ET NINTER = 3) ET EN 3D, ON
!     CONSIDERE DES TRIA6 (NPTS = 3 ET NINTER = 6).
    if (ndime .eq. ndim-1 .and. npts .eq. ndim) then
        heav(1)=99.d0
    endif
!
    call jedema()
end subroutine
