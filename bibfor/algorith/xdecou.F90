subroutine xdecou(ndim, elp, nnop, nnose, it,&
                  pintt, cnset, lsn, fisco, igeom,&
                  nfiss, ifiss, pinter, ninter, npts,&
                  ainter, lonref, nfisc)
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/elref4.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lteatt.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xajpin.h"
#include "asterfort/xxmmvd.h"
    real(kind=8) :: lsn(*), pintt(*), lonref, pinter(*), ainter(*)
    integer :: ndim, nnop, nnose, it, cnset(*), ninter, igeom, npts
    integer :: nfiss, ifiss, fisco(*), nfisc
    character(len=8) :: elp
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!                      TROUVER LES PTS D'INTERSECTION ENTRE LES ARETES
!                      ET LE PLAN DE FISSURE
!
!     ENTREE
!       NDIM     : DIMENSION DE L'ESPACE
!       ELP      : ELEMENT DE REFERENCE PARENT
!       NNOP     : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
!       NNOSE    : NOMBRE DE NOEUDS DU SOUS TETRA
!       IT       : INDICE DU TETRA EN COURS
!       PINTT    :
!       CNSET    : CONNECTIVITEE DES NOEUDS DU TETRA
!       LSN      : VALEURS DE LA LEVEL SET NORMALE
!       FISCO    :
!       IGEOM    : ADRESSE DES COORDONNEES DES NOEUDS DE L'ELT PARENT
!       NFIS     : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT
!       NFISS
!       IFISS
!       LONREF   :
!       NFISC    :
!
!     SORTIE
!       PINTER   : COORDONNEES DES POINTS D'INTERSECTION
!       NINTER   : NB DE POINTS D'INTERSECTION
!       NPTS     : NB DE PTS D'INTERSECTION COINCIDANT AVEC UN NOEUD
!       AINTER   : INFOS ARETE ASSOCIÃE AU POINTS D'INTERSECTION
!     ------------------------------------------------------------------
!
    real(kind=8) :: a(3), b(3), c(3), lsna, lsnb, tampor(4)
    real(kind=8) :: lsnc, somlsn(nfisc+1), ff(nnop)
    real(kind=8) :: rbid2(ndim)
    integer :: ar(12, 3), nbar, nta, ntb, na, nb, ins
    integer :: ia, i, j, ipt, ibid, pp, pd, k, ptmax
    integer :: ndime, iter, a1, a2
    integer :: mxstac
    character(len=8) :: typma
    integer :: zxain
    logical :: axi, papillon, ajout
    parameter      (mxstac=1000)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
    ASSERT(nnop.le.mxstac)
    ASSERT(nfisc.le.mxstac)
    ASSERT(ndim.le.mxstac)
!
    zxain = xxmmvd('ZXAIN')
    call elref4(' ', 'RIGI', ndime, ibid, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
    axi = lteatt(' ','AXIS','OUI')
!
    if (ndim .eq. 3) then
!
        if (ndime .eq. 3) then
            typma='TETRA4'
            ptmax=4
        else if (ndime.eq.2) then
            typma='TRIA3'
            ptmax=3
        else if (ndime.eq.1) then
            typma='SEG2'
            ptmax=2
        endif
!
    else if (ndim.eq.2) then
!
        if (ndime .eq. 2) then
            typma='TRIA3'
            ptmax=2
        else if (ndime.eq.1) then
            typma='SEG2'
            ptmax=2
        endif
!
    else if (ndim.eq.1) then
!
        ASSERT(.false.)
!
    endif
!
!     VECTEUR REEL A ZXAIN COMPOSANTES, POUR CHAQUE PT D'INTER :
!     - NUMERO ARETE CORRESPONDANTE (0 SI C'EST UN NOEUD SOMMET)
!     - VRAI NUMERO NOEUD CORRESPONDANT (SERT QUE POUR NOEUD SOMMET)
!     - LONGUEUR DE L'ARETE
!     - POSITION DU PT SUR L'ARETE
!     - ARETE VITALE (NE SERT A RIEN ICI)
!
!     COMPTEUR DE POINT INTERSECTION ET POINT D'INTERSECTION SOMMENT
    ipt=0
    ins=0
!
!     SOMME DES LSN SUR LES NOEUDS DU SE
    call vecini(nfisc+1, 0.d0, somlsn)
    do 300 k = 1, nnose
        na=cnset(nnose*(it-1)+k)
        if (na .lt. 1000) then
            do 305 i = 1, nfisc
                somlsn(i) = somlsn(i)+lsn((na-1)*nfiss+fisco(2*i-1))
305          continue
        else
!         RECUP COOR GLOBALES
            call vecini(3, 0.d0, a)
            do 310 i = 1, ndim
                a(i)=pintt(ndim*(na-1001)+i)
310          continue
!           CALCUL DES FF
            call reeref(elp, nnop, zr(igeom), a, ndim, rbid2, ff)
!           INTERPOLATION LSN
            do 320 j = 1, nnop
                do 325 i = 1, nfisc
                    somlsn(i)=somlsn(i)+ff(j)*lsn((j-1)*nfiss+fisco(2*&
                    i-1))
325              continue
320          continue
        endif
300  end do
!  SI ON EST PAS DU COTÉ INTERSECTÉ, ON SORT
    do 330 i = 1, nfisc
        if (fisco(2*i)*somlsn(i) .gt. 0) goto 999
330  end do
!
    call conare(typma, ar, nbar)
!
!     BOUCLE SUR LES ARETES POUR DETERMINER LES POINTS D'INTERSECTION
!
    do 100 ia = 1, nbar
!       NUM NO DU SOUS-ELEMENT
        nta=ar(ia,1)
        ntb=ar(ia,2)
!       NUM NO OU POINT D'INTER DE L'ELEMENT PARENT
        na=cnset(nnose*(it-1)+nta)
        nb=cnset(nnose*(it-1)+ntb)
!
        call vecini(3, 0.d0, a)
        call vecini(3, 0.d0, b)
        do 110 i = 1, ndim
            if (na .lt. 1000) then
                a(i)=zr(igeom-1+ndim*(na-1)+i)
            else
                a(i)=pintt(ndim*(na-1001)+i)
            endif
            if (nb .lt. 1000) then
                b(i)=zr(igeom-1+ndim*(nb-1)+i)
            else
                b(i)=pintt(ndim*(nb-1001)+i)
            endif
110      continue
!        LONGAR=PADIST(NDIM,A,B)
!
        if (na .lt. 1000) then
            lsna=lsn((na-1)*nfiss+ifiss)
        else
!         CALCUL DES FF
            call reeref(elp, nnop, zr(igeom), a, ndim, rbid2, ff)
!         INTERPOLATION LSN
            lsna=0
            do 10 i = 1, nnop
                lsna = lsna + ff(i)*lsn((i-1)*nfiss+ifiss)
10          continue
            if (abs(lsna) .lt. lonref*1.d-4) lsna = 0
        endif
        if (nb .lt. 1000) then
            lsnb=lsn((nb-1)*nfiss+ifiss)
        else
!         CALCUL DES FF
            call reeref(elp, nnop, zr(igeom), b, ndim, rbid2, ff)
!         INTERPOLATION LSN
            lsnb=0
            do 20 i = 1, nnop
                lsnb = lsnb + ff(i)*lsn((i-1)*nfiss+ifiss)
20          continue
            if (abs(lsnb) .lt. lonref*1.d-4) lsnb = 0
        endif
!
        if ((lsna*lsnb) .le. 0) then
            if (lsna .eq. 0) then
!           ON AJOUTE A LA LISTE LE POINT A
                call xajpin(ndim, pinter, ptmax, ipt, ins,&
                            a, lonref, ainter, 0, na,&
                            0.d0, ajout)
            endif
            if (lsnb .eq. 0) then
!           ON AJOUTE A LA LISTE LE POINT B
                call xajpin(ndim, pinter, ptmax, ipt, ins,&
                            b, lonref, ainter, 0, nb,&
                            0.d0, ajout)
            endif
            if (lsna .ne. 0 .and. lsnb .ne. 0) then
!           INTERPOLATION DES COORDONNEES DE C
888              continue
                do 130 i = 1, ndim
                    c(i)=a(i)-lsna/(lsnb-lsna)*(b(i)-a(i))
130              continue
                if (nfiss .ge. 2) then
!         CALCUL DES FF
                    call reeref(elp, nnop, zr(igeom), c, ndim, rbid2, ff)
!         INTERPOLATION LSN
                    lsnc=0
                    iter = 0
                    do 30 i = 1, nnop
                        lsnc = lsnc + ff(i)*lsn((i-1)*nfiss+ifiss)
30                  continue
                    if (abs(lsnc) .gt. lonref*1d-8) then
                        iter = iter+1
                        ASSERT(iter.lt.50)
                        lsnb = lsnc
                        do 140 i = 1, ndim
                            b(i) = c(i)
140                      continue
                        goto 888
                    endif
                endif
!           POSITION DU PT D'INTERSECTION SUR L'ARETE
!            ALPHA=PADIST(NDIM,A,C)
!           ON AJOUTE A LA LISTE LE POINT C
                call xajpin(ndim, pinter, ptmax, ipt, ibid,&
                            c, lonref, ainter, ia, 0,&
                            0.d0, ajout)
            endif
        endif
100  end do
!
999  continue
    ninter=ipt
    npts  =ins
    ASSERT(ninter.ge.npts.and.ninter.le.ptmax)
!
!     TRI DES POINTS D'INTERSECTION PAR ORDRE CROISSANT DES ARETES
    do 200 pd = 1, ninter-1
        pp=pd
        do 201 i = pp, ninter
            if (ainter(zxain*(i-1)+1) .lt. ainter(zxain*(pp-1)+1)) pp=i
201      continue
        do 202 k = 1, 4
            tampor(k)=ainter(zxain*(pp-1)+k)
            ainter(zxain*(pp-1)+k)=ainter(zxain*(pd-1)+k)
            ainter(zxain*(pd-1)+k)=tampor(k)
202      continue
        do 203 k = 1, ndim
            tampor(k)=pinter(ndim*(pp-1)+k)
            pinter(ndim*(pp-1)+k)=pinter(ndim*(pd-1)+k)
            pinter(ndim*(pd-1)+k)=tampor(k)
203      continue
200  end do
!
!      TRI DES POINTS POUR QUE LE POLYGONE IP1,IP2,IP3,IP4 SOIT CONVEXE
!      IP1 IP2 ET IP3 ONT UN SOMMET EN COMMUN
!      IP1 ET IP4 N ONT PAS DE SOMMET COMMUN
    if (ninter .eq. 4 .and. npts.eq.0) then
        a1=nint(ainter(1))
        do 220 ia = 2,3
          a2=nint(ainter(zxain*(ia-1)+1))
          papillon=.true.
          do 224 i = 1,2
            do 225 j = 1,2            
              if(ar(a1,i).eq.ar(a2,j)) papillon=.false.
225         continue          
224       continue
          if(papillon) then
!        CONFIGURATION RENCONTREE PAR EXEMPLE DANS SSNV510C
            do 226 k = 1,(zxain-1)
              tampor(k)=ainter(zxain*(ia-1)+k)
              ainter(zxain*(ia-1)+k)=ainter(zxain*(4-1)+k)
              ainter(zxain*(4-1)+k)=tampor(k)
226         continue
            do 227 k = 1, ndim
              tampor(k)=pinter(ndim*(ia-1)+k)
              pinter(ndim*(ia-1)+k)=pinter(ndim*(4-1)+k)
              pinter(ndim*(4-1)+k)=tampor(k)
227         continue
          endif
220     continue
    endif
    call jedema()
end subroutine
