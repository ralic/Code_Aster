subroutine xdecqv(nnose, it, cnset, lsn, igeom,&
                  ninter, npts, ainter, nse, cnse,&
                  heav, nsemax, pinter, pintt)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndcent.h"
#include "asterfort/provec.h"
#include "asterfort/tecael.h"
#include "asterfort/xpente.h"
#include "asterfort/xxmmvd.h"
#include "blas/ddot.h"
    integer :: nnose, it, cnset(*), igeom, ninter, npts, nse, cnse(6, 10)
    integer :: nsemax
    real(kind=8) :: lsn(*), ainter(*), heav(*), pinter(*), pintt(*)
!
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
!           BUT:       DÉCOUPER LE TETRA EN NSE SOUS-TETRAS
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
!     SORTIE
!       NSE      : NOMBRE DE SOUS-ÉLÉMENTS (TÉTRAS)
!       CNSE     : CONNECTIVITÉ DES SOUS-ÉLÉMENTS (TÉTRAS)
!       HEAV     : FONCTION HEAVYSIDE CONSTANTE SUR CHAQUE SOUS-ÉLÉMENT
!     ----------------------------------------------------------------
!
    real(kind=8) :: xyz(4, 3), ab(3), ac(3), ad(3), vn(3), ps
    real(kind=8) :: lsnbc, lsna
    integer :: in, inh, i, j, ar(12, 3), nbar, ise, ndim
    integer :: a1, a2, a3, a4, a5, a6, a, b, c, iadzi, iazk24, ndime, n(18)
    integer :: d, e, f, g, h, l, ia, ip1
    integer :: nnop
    integer :: zxain
    character(len=8) :: typma, noma, elrese(3)
    aster_logical :: cut
    integer, pointer :: dime(:) => null()
!
    data            elrese /'SEG3','TRIA6','TETRA10'/
! --------------------------------------------------------------------
    call jemarq()
!
    call elrefe_info(fami='RIGI', ndim=ndime, nno=nnop)
    zxain = xxmmvd('ZXAIN')
    call tecael(iadzi, iazk24, noms=0)
    noma=zk24(iazk24)
    call jeveuo(noma//'.DIME', 'L', vi=dime)
    ndim=dime(6)
    nse=0
    do 10 in = 1, 6
        do 20 j = 1, 10
            cnse(in,j)=0
 20     continue
 10 continue
!
    typma=elrese(ndime)
!
    call conare(typma, ar, nbar)
!
!     L'ELEMENT EST IL TRAVERSE PAR LA FISSURE?
    cut=.false.
    do 30 ia = 1, nbar
        if (lsn(cnset(nnose*(it-1)+ar(ia,1)))*lsn(cnset(nnose*(it-1)+ar(ia,2))) .lt. 0.d0) &
        cut=.true.
 30 continue
!
!     STOCKAGE DE LA CONNECTIVITE D'UN SOUS-ELEMENT NON COUPE
    if (.not.cut) then
        nse=1
        do 31 in = 1, nnose
            cnse(1,in)=cnset(nnose*(it-1)+in)
 31     continue
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
 40         continue
!
        else if (ninter .eq. 2) then
            a1=nint(ainter(zxain*(1-1)+1))
            a2=nint(ainter(zxain*(2-1)+1))
            if (npts .eq. 0) then
                nse=3
                ASSERT(a1.ne.0)
                do 50 i = 1, 2
                    do 51 j = 1, 2
                        if (ar(a1,i) .eq. ar(a2,j)) then
                            a=ar(a1,i)
                            b=ar(a1,3-i)
                            c=ar(a2,3-j)
                            e=ar(6-(a1+a2),3)
                        endif
 51                 continue
 50             continue
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
                nse=2
                ASSERT(a1.eq.0.and.a2.ne.0)
!           101 ET 102 LES 2 POINTS D'INTERSECTION
!           CNSE(1,1)=101
                ip1=nint(ainter(zxain*(npts-1)+2))
                b = ar(a2,1)
                c = ar(a2,2)
                e=0
                f=0
                do 52 i = 1, 3
                    do 53 j = 1, 2
                        if (cnset(nnose*(it-1)+ar(i,j)) .eq. ip1 .and. ar(i,3-j) .eq. b) &
                        e= ar(i,3)
                        if (cnset(nnose*(it-1)+ar(i,j)) .eq. ip1 .and. ar(i,3-j) .eq. c) &
                        f= ar(i,3)
 53                 continue
 52             continue
                ASSERT((e*f).gt.0)
                cnse(1,1)=ip1
                cnse(1,2)=102
                cnse(1,3)=cnset(nnose*(it-1)+b)
                cnse(1,4)=203
                cnse(1,5)=202
                cnse(1,6)=cnset(nnose*(it-1)+e)
                cnse(2,1)=ip1
                cnse(2,2)=102
                cnse(2,3)=cnset(nnose*(it-1)+c)
                cnse(2,4)=203
                cnse(2,5)=201
                cnse(2,6)=cnset(nnose*(it-1)+f)
!
            else if (npts .ge.2) then
!         PAS DE DECOUPAGE
!         1 SEUL ELEMENT
                nse=1
                do 60 in = 1, nnose
                    cnse(1,in)=cnset(nnose*(it-1)+in)
 60             continue
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
 70             continue
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
 81                 continue
 80             continue
                cnse(1,1)=102
                cnse(1,2)=103
                cnse(1,3)=cnset(nnose*(it-1)+a)
                cnse(1,4)=205
                cnse(1,5)=204
                cnse(1,6)=202
                cnse(2,1)=102
                cnse(2,2)=103
                cnse(2,3)=cnset(nnose*(it-1)+c)
                cnse(2,4)=205
                cnse(2,5)=203
                cnse(2,6)=206
                cnse(3,1)=102
                cnse(3,2)=cnset(nnose*(it-1)+b)
                cnse(3,3)=cnset(nnose*(it-1)+c)
                cnse(3,4)=201
                cnse(3,5)=cnset(nnose*(it-1)+e)
                cnse(3,6)=206
!
            else if (npts .ge.2) then
!         PAS DE DECOUPAGE
!         1 SEUL ELEMENT
                nse=1
                do 90 in = 1, nnose
                    cnse(1,in)=cnset(nnose*(it-1)+in)
 90             continue
!
            endif
!           ENDIF SUR NPTS DE NINTER=3
        else
!         1 SEUL ELEMENT
            nse=1
            do 100 in = 1, nnose
                cnse(1,in)=cnset(nnose*(it-1)+in)
100         continue
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
110         continue
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
120         continue
!
        endif
!
    else if (ndime .eq. 3 .and. cut) then
!
        if (ninter .lt. 3) then
!
!       1Â°) AVEC MOINS DE TROIS POINTS D'INTERSECTION
!       ---------------------------------------------
!
!         INTER DOUTEUSE
            ASSERT(npts.eq.ninter)
!         ON A UN SEUL ELEMENT
            nse=1
            do 200 in = 1, nnose
                cnse(1,in)=cnset(nnose*(it-1)+in)
200         continue
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
                do 210 in = 1, nnose
                    cnse(1,in)=cnset(nnose*(it-1)+in)
210             continue
!
            else if (npts .eq. 2) then
!           ON A UN SEUL ELEMENT
                nse=1
                do 220 in = 1, nnose
                    cnse(1,in)=cnset(nnose*(it-1)+in)
220             continue
!
            else if (npts.eq.1) then
!           ON A TROIS SOUS-ELEMENTS
                nse=3
                ASSERT(a1.eq.0.and.a2.ne.0.and.a3.ne.0)
                ip1=nint(ainter(2))
!           ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
                a=0
                b=0
                c=0
                e=0
                f=0
                g=0
                h=0
                do 35 i = 1, 2
                    do 45 j = 1, 2
                        if (ar(a2,i) .eq. ar(a3,j)) then
                            a=ar(a2,i)
                            b=ar(a2,3-i)
                            c=ar(a3,3-j)
                        endif
 45                 continue
 35             continue
                do 36 i = 1, 6
                    do 46 j = 1, 2
                        if (ar(i,j) .eq. b .and. ar(i,3-j) .eq. c) e=ar(i,3)
                        if (ar(i,j) .eq. b .and. cnset(nnose*(it-1)+ar(i,3-j)) .eq. ip1) &
                        f=ar(i,3)
                        if (ar(i,j) .eq. c .and. cnset(nnose*(it-1)+ar(i,3-j)) .eq. ip1) &
                        g=ar(i,3)
                        if (ar(i,j) .eq. a .and. cnset(nnose*(it-1)+ar(i,3-j)) .eq. ip1) &
                        h=ar(i,3)
 46                 continue
 36             continue
                ASSERT((a*b*c*e*f*g*h).gt.0)
!           ON REMPLACE 101 PAR LE NUMERO DU NOEUD COUPÉ
                cnse(1,1)=ip1
                cnse(1,2)=102
                cnse(1,3)=103
                cnse(1,4)=cnset(nnose*(it-1)+a)
                cnse(1,5)=205
                cnse(1,6)=206
                cnse(1,7)=207
                cnse(1,8)=cnset(nnose*(it-1)+h)
                cnse(1,9)=202
                cnse(1,10)=204
                cnse(2,1)=ip1
                cnse(2,2)=102
                cnse(2,3)=103
                cnse(2,4)=cnset(nnose*(it-1)+c)
                cnse(2,5)=205
                cnse(2,6)=206
                cnse(2,7)=207
                cnse(2,8)=cnset(nnose*(it-1)+g)
                cnse(2,9)=208
                cnse(2,10)=203
                cnse(3,1)=ip1
                cnse(3,2)=102
                cnse(3,3)=cnset(nnose*(it-1)+b)
                cnse(3,4)=cnset(nnose*(it-1)+c)
                cnse(3,5)=205
                cnse(3,6)=201
                cnse(3,7)=cnset(nnose*(it-1)+f)
                cnse(3,8)=cnset(nnose*(it-1)+g)
                cnse(3,9)=208
                cnse(3,10)=cnset(nnose*(it-1)+e)
!
            else if (npts.eq.0) then
                nse=4
                ASSERT(a1.ne.0.and.a2.ne.0.and.a3.ne.0)
                a=0
                b=0
                c=0
                d=0
                e=0
                f=0
                g=0
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
                    if (ar(a3,i) .eq. a) d=ar(a3,3-i)
 39             continue
                do 59 i = 1, 6
                    do 69 j = 1, 2
                        if (ar(i,j) .eq. b .and. ar(i,3-j) .eq. c) e=ar(i,3)
                        if (ar(i,j) .eq. c .and. ar(i,3-j) .eq. d) f=ar(i,3)
                        if (ar(i,j) .eq. b .and. ar(i,3-j) .eq. d) g=ar(i,3)
 69                 continue
 59             continue
                ASSERT((a*b*c*d*e*f*g).gt.0)
!           ON A QUATRE SOUS-ELEMENTS
                cnse(1,1)=101
                cnse(1,2)=102
                cnse(1,3)=103
                cnse(1,4)=cnset(nnose*(it-1)+a)
                cnse(1,5)=207
                cnse(1,6)=208
                cnse(1,7)=209
                cnse(1,8)=202
                cnse(1,9)=204
                cnse(1,10)=206
!
                n(1)=101
                n(2)=102
                n(3)=103
                n(4)=cnset(nnose*(it-1)+b)
                n(5)=cnset(nnose*(it-1)+c)
                n(6)=cnset(nnose*(it-1)+d)
                n(7)=207
                n(8)=208
                n(9)=209
                n(10)=201
                n(11)=203
                n(12)=205
                n(13)=cnset(nnose*(it-1)+e)
                n(14)=cnset(nnose*(it-1)+f)
                n(15)=cnset(nnose*(it-1)+g)
                n(16)=210
                n(17)=211
                n(18)=212
                call xpente(2, cnse, n)
            endif
!
        else if (ninter.eq.4) then
!
            a1=nint(ainter(zxain*(1-1)+1))
            a2=nint(ainter(zxain*(2-1)+1))
            a3=nint(ainter(zxain*(3-1)+1))
            a4=nint(ainter(zxain*(4-1)+1))
!
           if (npts .eq. 1) then
!            LE PREMIER NOEUD STOCKE EST FORCEMNT UN NOEUD SOMMET ET LES AUTRES NON
               ASSERT(a1.eq.0.and.a2.gt.0.and.a3.gt.0.and.a4.gt.0)
                nse=4
                a=0
                b=0
                c=0
                d=0
                e=0
                f=0
                g=0
                do i = 1, 2
                    do j = 1, 2
                        if (ar(a2,i) .eq. ar(a3,j)) then
                        a=ar(a2,i)
                        b=ar(a2,3-i)
                        c=ar(a3,3-j)
                        endif
                     end do
                end do
                do i = 1, 2
                    if (ar(a4,i) .eq. a) d=ar(a4,3-i)
                end do
                do i = 1, 6
                    do j = 1, 2
                        if (ar(i,j) .eq. b .and. ar(i,3-j) .eq. c) e=ar(i,3)
                        if (ar(i,j) .eq. c .and. ar(i,3-j) .eq. d) f=ar(i,3)
                        if (ar(i,j) .eq. b .and. ar(i,3-j) .eq. d) g=ar(i,3)
                    end do
                end do
                ASSERT((a*b*c*d*e*f*g).gt.0)
!           ON A QUATRE SOUS-ELEMENTS
                cnse(1,1)=102
                cnse(1,2)=103
                cnse(1,3)=104
                cnse(1,4)=cnset(nnose*(it-1)+a)
                cnse(1,5)=207
                cnse(1,6)=208
                cnse(1,7)=209
                cnse(1,8)=202
                cnse(1,9)=204
                cnse(1,10)=206
!
                n(1)=102
                n(2)=103
                n(3)=104
                n(4)=cnset(nnose*(it-1)+b)
                n(5)=cnset(nnose*(it-1)+c)
                n(6)=cnset(nnose*(it-1)+d)
                n(7)=207
                n(8)=208
                n(9)=209
                n(10)=201
                n(11)=203
                n(12)=205
                n(13)=cnset(nnose*(it-1)+e)
                n(14)=cnset(nnose*(it-1)+f)
                n(15)=cnset(nnose*(it-1)+g)
                n(16)=210
                n(17)=211
                n(18)=212
                call xpente(2, cnse, n)
!
           else if (npts.eq.2) then
!            ON A DEUX SOUS-ELEMENTS
                nse=2
!            LES DEUX PREMIERS NOEUDS STOCKES SONT FORCEMNT DES NOEUDS SOMMETS ET LES AUTRES NON
                ASSERT(a1.eq.0.and.a2.eq.0.and.a3.gt.0.and.a4.gt.0)
                a=ar(a3,1)
                b=ar(a3,2)
                c=nint(ainter(2))
                d=nint(ainter(zxain+2))
                do 771 i = 1, 6
                    do 772 j = 1, 2
                        if (cnset(nnose*(it-1)+ar(i,j)) .eq. c .and.&
                            cnset(nnose*(it-1)+ar(i,3-j)) .eq. d) e=ar(i,3)
772                 continue
771             continue
                do 773 i = 1, 6
                    do 774 j = 1, 2
                        if (cnset(nnose*(it-1)+ar(i,j)) .eq. c .and. ar(i,3-j) .eq. a) &
                        f=ar(i,3)
                        if (cnset(nnose*(it-1)+ar(i,j)) .eq. d .and. ar(i,3-j) .eq. a) &
                        g=ar(i,3)
                        if (cnset(nnose*(it-1)+ar(i,j)) .eq. c .and. ar(i,3-j) .eq. b) &
                        h=ar(i,3)
                        if (cnset(nnose*(it-1)+ar(i,j)) .eq. d .and. ar(i,3-j) .eq. b) &
                        l=ar(i,3)
774                 continue
773             continue
                ASSERT((e*f*g*h*l).gt.0)
                cnse(1,1)=nint(ainter(2))
                cnse(1,2)=nint(ainter(zxain+2))
                cnse(1,3)=103
                cnse(1,4)=cnset(nnose*(it-1)+a)
                cnse(1,5)=cnset(nnose*(it-1)+e)
                cnse(1,6)=204
                cnse(1,7)=203
                cnse(1,8)=cnset(nnose*(it-1)+f)
                cnse(1,9)=cnset(nnose*(it-1)+g)
                cnse(1,10)=201
                cnse(2,1)=nint(ainter(2))
                cnse(2,2)=nint(ainter(zxain+2))
                cnse(2,3)=103
                cnse(2,4)=cnset(nnose*(it-1)+b)
                cnse(2,5)=cnset(nnose*(it-1)+e)
                cnse(2,6)=204
                cnse(2,7)=203
                cnse(2,8)=cnset(nnose*(it-1)+h)
                cnse(2,9)=cnset(nnose*(it-1)+l)
                cnse(2,10)=202
!
            elseif (npts .eq.0) then
            nse=6
            ASSERT((a1*a2*a3*a4).ne.0)
            a=0
            b=0
            c=0
            d=0
            e=0
            f=0
            do i = 1, 2
                do j = 1, 2
                    if (ar(a1,i) .eq. ar(a2,j)) then
                        a=ar(a1,i)
                        b=ar(a1,3-i)
                        c=ar(a2,3-j)
                    endif
                    if (ar(a3,i).eq.ar(a4,j)) d=ar(a3,i)
                end do
            end do
            do i = 1, 6
                do j = 1, 2
                    if (ar(i,j) .eq. b .and. ar(i,3-j) .eq. c) e=ar(i,3)
                    if (ar(i,j) .eq. a .and. ar(i,3-j) .eq. d) f=ar(i,3)
                end do
            end do
            ASSERT((a*b*c*d*e*f).gt.0)
            n(1)=104
            n(2)=102
            n(3)=cnset(nnose*(it-1)+c)
            n(4)=103
            n(5)=101
            n(6)=cnset(nnose*(it-1)+b)
            n(7)=210
            n(8)=203
            n(9)=207
            n(10)=211
            n(11)=209
            n(12)=cnset(nnose*(it-1)+e)
            n(13)=212
            n(14)=201
            n(15)=205
            n(16)=213
            n(17)=214
            n(18)=216
            call xpente(1, cnse, n)
            n(1)=cnset(nnose*(it-1)+a)
            n(2)=101
            n(3)=102
            n(4)=cnset(nnose*(it-1)+d)
            n(5)=103
            n(6)=104
            n(7)=202
            n(8)=209
            n(9)=204
            n(10)=cnset(nnose*(it-1)+f)
            n(11)=212
            n(12)=210
            n(13)=206
            n(14)=211
            n(15)=208
            n(16)=217
            n(17)=213
            n(18)=215
            call xpente(4, cnse, n)
            endif
        else if (ninter.eq.5) then
!
               a1=nint(ainter(zxain*(1-1)+1))
               a2=nint(ainter(zxain*(2-1)+1))
               a3=nint(ainter(zxain*(3-1)+1))
               a4=nint(ainter(zxain*(4-1)+1))
               a5=nint(ainter(zxain*(5-1)+1))
!
           nse =6
           ASSERT(npts .eq. 1)
!            LE PREMIER NOEUD STOCKE EST FORCEMNT UN NOEUD SOMMET ET LES AUTRES
!            NON
               ASSERT(a1.eq.0.and.a2.gt.0.and.a3.gt.0.and.a4.gt.0.and.a5.gt.0)
                a=0
                b=0
                c=0
                d=0
                e=0
                f=0
                g=0
                do i = 1, 2
                   do j = 1, 2
                       if (ar(a2,i) .eq. ar(a3,j)) then
                           a=ar(a2,i)
                           b=ar(a2,3-i)
                           c=ar(a3,3-j)
                       endif
                       if (ar(a4,i).eq.ar(a5,j)) d=ar(a4,i)
                   end do
                end do
                do i = 1, 6
                   do j = 1, 2
                       if (ar(i,j) .eq. b .and. ar(i,3-j) .eq. c) e=ar(i,3)
                       if (ar(i,j) .eq. a .and. ar(i,3-j) .eq. d) f=ar(i,3)
                   end do
                end do
                ASSERT((a*b*c*d*e*f).gt.0)
!           ON A 6  SOUS-ELEMENTS
            n(1)=105
            n(2)=103
            n(3)=cnset(nnose*(it-1)+c)
            n(4)=104
            n(5)=102
            n(6)=cnset(nnose*(it-1)+b)
            n(7)=210
            n(8)=203
            n(9)=207
            n(10)=211
            n(11)=209
            n(12)=cnset(nnose*(it-1)+e)
            n(13)=212
            n(14)=201
            n(15)=205
            n(16)=213
            n(17)=214
            n(18)=216
            call xpente(1, cnse, n)
            n(1)=cnset(nnose*(it-1)+a)
            n(2)=102
            n(3)=103
            n(4)=cnset(nnose*(it-1)+d)
            n(5)=104
            n(6)=105
            n(7)=202
            n(8)=209
            n(9)=204
            n(10)=cnset(nnose*(it-1)+f)
            n(11)=212
            n(12)=210
            n(13)=206
            n(14)=211
            n(15)=208
            n(16)=217
            n(17)=213
            n(18)=215
            call xpente(4, cnse, n)
!
        else if (ninter.eq.6) then
!
               nse = 4
               a1=nint(ainter(zxain*(1-1)+1))
               a2=nint(ainter(zxain*(2-1)+1))
               a3=nint(ainter(zxain*(3-1)+1))
               a4=nint(ainter(zxain*(4-1)+1))
               a5=nint(ainter(zxain*(5-1)+1))
               a6=nint(ainter(zxain*(6-1)+1))
!
!           ON A QUATRE SOUS-ELEMENTS
               ASSERT(npts .eq. 2)
!            LES DEUX PREMIERS NOEUDS STOCKES SONT FORCEMNT DES NOEUDS SOMMETS ET LES AUTRES NON
               ASSERT(a1.eq.0.and.a2.eq.0.and.a3.gt.0.and.a4.gt.0.and.a5.gt.0.and.a6.gt.0)
                nse=4
                a=0
                b=0
                c=0
                d=0
                e=0
                f=0
                g=0
                do i = 1, 2
                   do j = 1, 2
                      if (ar(a4,i) .eq. ar(a5,j)) then
                         a=ar(a4,i)
                         c=ar(a4,3-i)
                         d=ar(a5,3-j)
                         goto 1100
                      endif 
                   end do
                end do
1100            continue
                do i = 1, 2
                   if (ar(a3,i).eq.a) then
                      b = ar(a3,3-i)
                   endif
                end do
                do i = 1, 6
                    do j = 1, 2
                          if (ar(i,j) .eq. b .and. ar(i,3-j) .eq. c) e=ar(i,3)
                          if (ar(i,j) .eq. c .and. ar(i,3-j) .eq. d) f=ar(i,3)
                          if (ar(i,j) .eq. b .and. ar(i,3-j) .eq. d) g=ar(i,3)
                    end do
                end do
                ASSERT((a*b*c*d*e*f*g).gt.0)
                cnse(1,1)=103
                cnse(1,2)=104
                cnse(1,3)=105
                cnse(1,4)=cnset(nnose*(it-1)+a)
                cnse(1,5)=207
                cnse(1,6)=208
                cnse(1,7)=209
                cnse(1,8)=202
                cnse(1,9)=204
                cnse(1,10)=206
                n(1)=103
                n(2)=104
                n(3)=105
                n(4)=cnset(nnose*(it-1)+b)
                n(5)=cnset(nnose*(it-1)+c)
                n(6)=cnset(nnose*(it-1)+d)
                n(7)=207
                n(8)=208
                n(9)=209
                n(10)=201
                n(11)=203
                n(12)=205
                n(13)=cnset(nnose*(it-1)+e)
                n(14)=cnset(nnose*(it-1)+f)
                n(15)=cnset(nnose*(it-1)+g)
                n(16)=210
                n(17)=211
                n(18)=212
                call xpente(2, cnse, n)
!
        endif
    endif
!
!-----------------------------------------------------------------------
!     VÃRIFICATION DU SENS DES SOUS-ÃLÃMENTS TETRA
!                  ALGO BOOK III (28/04/04)
!-----------------------------------------------------------------------
!
    if (ndime .eq. 3) then
        do 500 ise = 1, nse
            do 505 in = 1, 4
                inh=cnse(ise,in)
                if (inh .lt. 100) then
                    do 510 j = 1, 3
                        xyz(in,j)=zr(igeom-1+ndim*(inh-1)+j)
510                 continue
                else if (inh.gt.100.and.inh.lt.1000) then
                    do 511 j = 1, 3
                        xyz(in,j)=pinter(ndim*(inh-100-1)+j)
511                 continue
                else
                    do 512 j = 1, 3
                        xyz(in,j)=pintt(ndim*(inh-1001)+j)
512                 continue
                endif
505         continue
            do 506 j = 1, 3
                ab(j)=xyz(2,j)-xyz(1,j)
                ac(j)=xyz(3,j)-xyz(1,j)
                ad(j)=xyz(4,j)-xyz(1,j)
506         continue
            call provec(ab, ac, vn)
            ps=ddot(3,vn,1,ad,1)
            if (ps .lt. 0) then
!          MAUVAIS SENS DU TETRA, ON INVERSE LES NOEUDS 3 ET 4
                inh=cnse(ise,3)
                cnse(ise,3)=cnse(ise,4)
                cnse(ise,4)=inh
!          ON INVERSE AUSSI LES NOEUDS MILIEUX 9 ET 6 PUIS 7 ET 8
                inh=cnse(ise,9)
                cnse(ise,9)=cnse(ise,6)
                cnse(ise,6)=inh
                inh=cnse(ise,7)
                cnse(ise,7)=cnse(ise,8)
                cnse(ise,8)=inh
            endif
500     continue
    endif
!
!-----------------------------------------------------------------------
!             MATRICE DES COORDONNÃES ET FONCTION HEAVYSIDE
!             ALGO BOOK III (28/04/04)
! --------------------------------------------------------------------
!
    ASSERT(nse.le.nsemax)
    if (ninter .eq. 3 .and. npts .eq. 1 .and. ndime .eq. 2) then
        lsnbc=lsn(cnset(nnose*(it-1)+b))+lsn(cnset(nnose*(it-1)+c))
        heav(1)=-sign(1.d0,lsnbc)
        heav(2)=sign(1.d0,lsnbc)
        heav(3)=sign(1.d0,lsnbc)
    else if (ninter.eq.4.and.npts.eq.1.and.ndime.eq.3) then
      lsna=lsn(cnset(nnose*(it-1)+a))
      heav(1)=sign(1.d0,lsna)
      heav(2)=-sign(1.d0,lsna)
      heav(3)=-sign(1.d0,lsna) 
      heav(4)=-sign(1.d0,lsna)
    else if (ninter.eq.5.and.npts.eq.1.and.ndime.eq.3) then
      lsna=lsn(cnset(nnose*(it-1)+e))
      heav(1)=sign(1.d0,lsna)
      heav(2)=sign(1.d0,lsna)
      heav(3)=sign(1.d0,lsna)
      heav(4)=-sign(1.d0,lsna) 
      heav(5)=-sign(1.d0,lsna) 
      heav(6)=-sign(1.d0,lsna) 
    else if (ninter.eq.6.and.npts.eq.2.and.ndime.eq.3) then
      lsna=lsn(cnset(nnose*(it-1)+a))
      heav(1)=sign(1.d0,lsna)
      heav(2)=-sign(1.d0,lsna)
      heav(3)=-sign(1.d0,lsna)
      heav(4)=-sign(1.d0,lsna)
    else
        do 300 ise = 1, nse
            heav(ise)=1.d0
            do 310 in = 1, ndime+1
                inh=cnse(ise,in)
                if (inh .lt. 100) then
                    if (lsn(inh) .lt. 0.d0) heav(ise)=-1.d0
                endif
310         continue
300     continue
    endif
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
