subroutine xdecqu(nnose, it, ndim, cnset, jlsn,&
                  igeom, pinter, ninter, npts, ainter,&
                  pmilie, nmilie, mfis, tx, txlsn,&
                  pintt, pmitt, ifiss, nfiss, fisco, &
                  nfisc, cut, coupe)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/loncar.h"
#include "asterfort/padist.h"
#include "asterfort/reeref.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xajpin.h"
#include "asterfort/xalgo2.h"
#include "asterfort/xalgo3.h"
#include "asterfort/xerfis.h"
#include "asterfort/xelrex.h"
#include "asterfort/xinter.h"
#include "asterfort/xxmmvd.h"
    integer :: nnose, it, ndim, cnset(*), ninter, igeom, npts, nmilie, mfis
    integer :: jlsn, ifiss, nfiss, nfisc, fisco(*), coupe(nfiss)
    real(kind=8) :: pinter(*), ainter(*), pmilie(*), tx(3, 7), txlsn(28)
    real(kind=8) :: pintt(*), pmitt(*)
    aster_logical :: cut
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
!                      TROUVER LES PTS D'INTERSECTION ENTRE LES ARETES
!                      ET LE PLAN DE FISSURE
!
!     ENTREE
!       NNOSE    : NOMBRE DE NOEUDS DU SOUS TETRA
!       IT       : INDICE DU TETRA EN COURS
!       CNSET    : CONNECTIVITÃ DES NOEUDS DU TETRA
!       LSN      : VALEURS DE LA LEVEL SET NORMALE
!       IGEOM    : ADRESSE DES COORDONNÃES DES NOEUDS DE L'ELT PARENT
!       IN IVF       FONCTIONS DE FORMES QUADRATIQUES
!       PINTT    : COORDONNEES REELLES DES POINTS D'INTERSECTION
!       PMITT    : COORDONNEES REELLES DES POINTS MILIEUX
!       IFISS    : FISSURE COURANTE
!       NFISS    : NOMBRE DE FISSURE
!       FISCO    : CONNECTIVITE FISSURE/DDL
!       NFISC    : NOMBRE DE JONCTIONS
!       CUT      : CE SOUS ELEMNT EST-IL COUPE?
!
!     SORTIE
!       PINTER   : COORDONNÃES DES POINTS D'INTERSECTION
!       NINTER   : NB DE POINTS D'INTERSECTION
!       NPTS     : NB DE PTS D'INTERSECTION COINCIDANT AVEC UN
!                  NOEUD SOMMET
!       AINTER   : INFOS ARETE ASSOCIÃE AU POINTS D'INTERSECTION
!       PMILIE   : COORDONNEES DES POINTS MILIEUX
!       NMILIE   : NB DE POINTS MILIEUX
!       MFIS     : NB DE POINTS MILIEUX SUR LA FISSURE
!       CUT      : LE SOUS ELEMENT EST-IL COUPE?
!       TABLS    : TABLEAU DES LSN DU SOUS ELEMENT
!     ----------------------------------------------------------------
!
    real(kind=8) :: a(3), b(3), c(3), m(3), lsna, lsnb, lsnm, tabls(27)
    real(kind=8) :: alpha, longar, lonref, tampor(4), tabco(81)
    real(kind=8) :: val, rbid, cref(ndim), pinref(18), lsnelp(27)
    real(kind=8) :: xref(81), ff(27), newpt(ndim), somlsn(nfisc+1)
    integer :: ar(12, 3), nbar, nta, ntb, na, nb, ins
    integer :: ia, i, ipi, ibid, pp, pd, k
    integer :: ndime, noeua, noeub, im
    integer :: j, a1, a2, ipt, nm
    integer :: ptmax, pmmaxi(3), pmmax
    integer :: ntm, inm, nptm, nnop, inter
    integer :: zxain, mxstac
    character(len=8) :: typma, elrese(3), elrefp
    aster_logical :: papillon, ajout, jonc
!
    parameter       (ptmax=6)
    parameter       (mxstac=1000)
    data            elrese /'SEG3','TRIA6','TETRA10'/
!
    data            pmmaxi  / 2,  6,   17 /
! --------------------------------------------------------------------
!
    ASSERT(nfisc.le.mxstac)
!
!
    zxain = xxmmvd('ZXAIN')
    call elref1(elrefp)
    call elrefe_info(fami='RIGI', ndim=ndime, nno=nnop)
!
!     VECTEUR REEL A 4 COMPOSANTES, POUR CHAQUE PT D'INTER :
!     - NUMERO ARETE CORRESPONDANTE (0 SI C'EST UN NOEUD SOMMET)
!     - VRAI NUMERO NOEUD CORRESPONDANT (SERT QUE POUR NOEUD SOMMET)
!     - LONGUEUR DE L'ARETE
!     - POSITION DU PT SUR L'ARETE
!
    typma=elrese(ndime)
!
    call vecini(81, 0.d0, tabco)
    call vecini(18, 0.d0, pinref)
    call vecini(81, 0.d0, xref)
!   RECUPERATION DES COORDONNES DE REFERENCE DE L ELEMENENT COMPLET
    call xelrex(elrefp, ibid, xref)
!
!     TABLEAU DES COORDONNEES DES NOEUDS DE L'ELEMENT ENFANT POUR LA FISSURE
!     COURANTE
    do 50 j = 1, nnose
        do 51 i = 1, ndim
            if (((nnop.eq.20).or.(nnop.eq.15).or.(nnop.eq.13).or. (nnop.eq.8)) .and.&
                (cnset(nnose*(it-1)+j).gt.nnop) .and. (cnset(nnose*(it-1)+j).lt.1000)) then
                val = tx(i,(cnset(nnose*(it-1)+j)-nnop))
            else if (cnset(nnose*(it-1)+j).lt.1000) then
                val = zr(igeom-1+ndim*(cnset(nnose*(it-1)+j)-1)+i)
            else if (cnset(nnose*(it-1)+j).lt.2000) then
                val = pintt(ndim*(cnset(nnose*(it-1)+j)-1001)+i)
            else
                val = pmitt(ndim*(cnset(nnose*(it-1)+j)-2001)+i)
            endif
            tabco(ndim*(j-1)+i)=val
 51     continue
 50 continue
!
    call vecini(27, 0.d0, tabls)
    call vecini(nfisc+1, 0.d0, somlsn)
    cut=.false.
!
!     TABLEAU DES LSN DES NOEUDS DE L'ELEMENT ENFANT
    do 40 j = 1, nnose
        na=cnset(nnose*(it-1)+j)
        if (((nnop.eq.20).or.(nnop.eq.15).or.(nnop.eq.13).or.( nnop.eq.8)) .and.&
            (na.gt.nnop) .and. (na.lt.1000)) then
            val = txlsn((na-nnop-1)*nfiss+ifiss)
            do i = 1, nfisc
                somlsn(i) = somlsn(i)+txlsn((na-nnop-1)*nfiss+fisco(2*i-1))
            end do
        else if (na.lt.1000) then
            val = zr(jlsn-1+(na-1)*nfiss+ifiss)
            do i = 1, nfisc
                somlsn(i) = somlsn(i)+zr(jlsn-1+(na-1)*nfiss+fisco(2*i-1))
            end do
        else
            do i = 1, ndim
                newpt(i) = tabco(ndim*(j-1)+i)
            end do
            call reeref(elrefp, nnop, zr(igeom), newpt, ndim,&
                        cref, ff)
            val = 0.d0
            do i = 1, nnop
                val = val + zr(jlsn-1+(i-1)*nfiss+ifiss)*ff(i)
                do ia = 1, nfisc
                    somlsn(ia)=somlsn(ia)+ff(i)*zr(jlsn-1+(i-1)*nfiss+fisco(2*ia-1))
                end do
            end do
        endif
        if (abs(val).le.1.d-8) val =0.d0
        tabls(j)=val
 40 continue
!  EN CAS DE JONCTION, SI ON EST PAS DU COTÃ~I INTERSECTÃ~I, ON SORT
    do i = 1, nfisc
        if (fisco(2*i)*somlsn(i) .gt. 0) goto 999
    end do
!
    call conare(typma, ar, nbar)
!     COMPTEUR DE POINT INTERSECTION = NOEUD SOMMET
    ins=0
!     COMPTEUR DE POINT INTERSECTION = NOEUD MILIEU
    inm=0
!     COMPTEUR DE POINT INTERSECTION = TOUS TYPES CONFONDUS
    ipi=0
!     LONGUEUR D'ARETE MAXIMALE DE L'ELEMENT (DU SE3 OU TR6)
    lonref=0.d0
!
!     L'ELEMENT EST IL TRAVERSE PAR LA FISSURE?
    do 30 ia = 1, nbar
        if ((tabls(ar(ia,1))*tabls(ar(ia,2))) .lt. 0.d0) cut = .true.
 30 continue
   coupe(ifiss) = 1
!
!     BOUCLE SUR LES ARETES DE L'ELEMENT ENFANT POUR AJUSTEMENT DES
!     LEVEL SET NORMALES SUR LES ARETES INTERNES
    do ia = 1, nbar
!
!       RECUPERATION NUM ENFANT DES NOEUDS DE L'ARETE
        nta=ar(ia,1)
        ntb=ar(ia,2)
        ntm=ar(ia,3)
!
!       RECUPERATION NUM PARENT DES NOEUDS DE L'ARETE
        na=cnset(nnose*(it-1)+nta)
        if (na .gt. 1000 .and. na .lt. 2000) then
           na = na - 1000
        elseif (na .gt. 2000 .and. na .lt. 3000) then
           na = na - 2000
        elseif (na .gt. 3000) then
           na = na - 3000
        endif
        nb=cnset(nnose*(it-1)+ntb)
        if (nb .gt. 1000 .and. nb .lt. 2000) then
           nb = nb - 1000
        elseif (nb .gt. 2000 .and. nb .lt. 3000) then
           nb = nb - 2000
        elseif (nb .gt. 3000) then
           nb = nb - 3000
        endif
        nm=cnset(nnose*(it-1)+ntm)
        if (nm .gt. 1000 .and. nm .lt. 2000) then
           nm = nm - 1000
        elseif (nm .gt. 2000 .and. nm .lt. 3000) then
           nm = nm - 2000
        elseif (nm .gt. 3000) then
           nm = nm - 3000
        endif
!
!       RECUPERATION LSN DES NOEUDS EXTREMITE DE L'ARETE
        lsna=tabls(nta)
        lsnb=tabls(ntb)
        lsnm=tabls(ntm)
!     BLINDAGE PARTIEL : FISSURE RENTRANTE SUR UNE ARETE
        if (lsna .eq. 0 .and. lsnb .eq. 0) then
           lsnm = 0.d0
           tabls(ntm) = 0.d0
        else if (lsna*lsnm .lt. 0 .and. lsnb*lsnm .lt. 0) then
           if(abs(lsna).ge.abs(lsnb)) then
              lsnb = 0.d0
              tabls(ntb) = 0.d0
              if (ifiss.eq.1) then
                 zr(jlsn-1+(nb-1)*nfiss+ifiss) = 0.d0
              endif
           else
              lsna = 0.d0
              tabls(nta) = 0.d0
              if (ifiss.eq.1) then
                 zr(jlsn-1+(na-1)*nfiss+ifiss) = 0.d0
              endif
           endif
           lsnm = 0.d0
           tabls(ntm) = 0.d0
        else if (lsnm .eq. 0 .and. lsnb*lsna .gt. 0) then
           if(abs(lsna).ge.abs(lsnb)) then
              lsnb = 0.d0
              tabls(ntb) = 0.d0
              if (ifiss.eq.1) then
                 zr(jlsn-1+(nb-1)*nfiss+ifiss) = 0.d0
              endif
           else
              lsna = 0.d0
              tabls(nta) = 0.d0
              if (ifiss.eq.1) then
                 zr(jlsn-1+(na-1)*nfiss+ifiss) = 0.d0
              endif
           endif
        else if (lsna .eq. 0 .and. lsnb*lsnm .lt. 0) then
           lsnm = 0.d0
           tabls(ntm) = 0.d0
        else if (lsnb .eq. 0 .and. lsna*lsnm .lt. 0) then
           lsnm = 0.d0
           tabls(ntm) = 0.d0
        endif
    end do
!
!     TABLEAU DES LSN DES NOEUDS DE L'ELEMENT PARENT POUR LA FISSURE COURANTE
    call vecini(27, 0.d0, lsnelp)
    do j = 1, nnop
       lsnelp(j) = zr(jlsn-1+(j-1)*nfiss+ifiss)
    end do
!
!     BOUCLE SUR LES ARETES POUR DETERMINER LES POINTS D'INTERSECTION
    do 100 ia = 1, nbar
!
!       RECUPERATION NUM ENFANT DES NOEUDS DE L'ARETE
        nta=ar(ia,1)
        ntb=ar(ia,2)
        ntm=ar(ia,3)
!
!       RECUPERATION NUM PARENT DES NOEUDS DE L'ARETE
        na=cnset(nnose*(it-1)+nta)
        nb=cnset(nnose*(it-1)+ntb)
        nm=cnset(nnose*(it-1)+ntm)
!
!       RECUPERATION LSN DES NOEUDS EXTREMITE DE L'ARETE
        lsna=tabls(nta)
        lsnb=tabls(ntb)
        lsnm=tabls(ntm)
!
        call vecini(ndim, 0.d0, a)
        call vecini(ndim, 0.d0, b)
        call vecini(ndim, 0.d0, m)
!
!       RECUPERATION COORDONNEES DES NOEUDS EXTREMITE DE L'ARETE
        do 101 i = 1, ndim
            a(i)=tabco(ndim*(nta-1)+i)
            b(i)=tabco(ndim*(ntb-1)+i)
            m(i)=tabco(ndim*(ntm-1)+i)
101     continue
!
!       LONGUEUR DE L'ARETE
        longar=padist(ndim,a,b)
!
! DEBUT RECHERCHE COORDONNEES DES POINTS D'INTERSECTION
! UN SEUL POINT INTER NON NOEUD SOMMET PAR ARETE!
!
!       SI LA FISSURE COUPE L'ARETE
        if ((lsna*lsnb) .le. 0) then
!
!         SI LA FISSURE COUPE L'EXTREMITE A
            if (lsna .eq. 0) then
!           ON AJOUTE A LA LISTE LE POINT A
                call xajpin(ndim, pinter, ptmax, ipi, ins,&
                            a, longar, ainter, 0, na,&
                            0.d0, ajout)
!
                if (ajout) then
                    do k = 1, ndime
                        if (na.lt.1000) then
                           pinref(ndime*(ipi-1)+k)=xref(ndime*(na-1)+k)
                        else
                           do i = 1, ndim
                              newpt(i) = a(i)
                           end do
                           call reeref(elrefp, nnop, zr(igeom), newpt, ndim,&
                                       cref, ff)
                           pinref(ndime*(ipi-1)+k)=cref(k)
                        endif
                    enddo
                endif
            endif
!
!         SI LA FISSURE COUPE L'EXTREMITE B
            if (lsnb .eq. 0) then
!           ON AJOUTE A LA LISTE LE POINT B
                call xajpin(ndim, pinter, ptmax, ipi, ins,&
                            b, longar, ainter, 0, nb,&
                            longar, ajout)
!
                if (ajout) then
                    do k = 1, ndime
                        if (nb.lt.1000) then
                           pinref(ndime*(ipi-1)+k)=xref(ndime*(nb-1)+k)
                        else
                           do i = 1, ndim
                              newpt(i) = b(i)
                           end do
                           call reeref(elrefp, nnop, zr(igeom), newpt, ndim,&
                                       cref, ff)
                           pinref(ndime*(ipi-1)+k)=cref(k)
                        endif
                    enddo
                endif
            endif
!
!         SI LA FISSURE COUPE LE MILIEU M
!         PETITE TOLERANCE SUR LSNM CAR VALEUR CALCULEE
            if (lsnm .eq. 0 .or. (lsna .eq. 0 .and. lsnb .eq. 0)) then
                alpha=padist(ndim,a,m)
!
                if (cut) then
                    call xajpin(ndim, pinter, ptmax, ipi, inm,&
                                m, longar, ainter, ia, 0,&
                                alpha, ajout)
!
                    if (ajout) then
                        do k = 1, ndime
                            if (nm.lt.1000) then
                               pinref(ndime*(ipi-1)+k)=xref(ndime*(nm-1)+k)
                            else
                               do i = 1, ndim
                                  newpt(i) = m(i)
                               end do
                               call reeref(elrefp, nnop, zr(igeom), newpt, ndim,&
                                           cref, ff)
                               pinref(ndime*(ipi-1)+k)=cref(k)
                            endif
                        enddo
                    endif
                else if (.not.cut) then
                    call xajpin(ndim, pinter, ptmax, ipi, inm,&
                                m, longar, ainter, 0, nm,&
                                alpha, ajout)
!
                    if (ajout) then
                        do k = 1, ndime
                            if (nm.lt.1000) then
                               pinref(ndime*(ipi-1)+k)=xref(ndime*(nm-1)+k)
                            else
                               do i = 1, ndim
                                  newpt(i) = m(i)
                               end do
                               call reeref(elrefp, nnop, zr(igeom), newpt, ndim,&
                                           cref, ff)
                               pinref(ndime*(ipi-1)+k)=cref(k)
                            endif
                        enddo
                    endif
                endif
            endif
!
!         SI LA FISSURE COUPE AILLEURS
            if (lsna .ne. 0 .and. lsnb .ne. 0 .and. lsnm .ne. 0) then
!           INTERPOLATION DES COORDONNEES DE C
                call xinter(ndim, ndime, elrefp, zr(igeom), lsnelp, na, nb,&
                            nm, pintt, pmitt, lsna, lsnb, lsnm, cref, c) 
!           POSITION DU PT D'INTERSECTION SUR L'ARETE
                alpha=padist(ndim,a,c)
!           ON AJOUTE A LA LISTE LE POINT C
                call xajpin(ndim, pinter, ptmax, ipi, ibid,&
                            c, longar, ainter, ia, 0,&
                            alpha, ajout)
                if (ajout .and. (na .gt. 1000 .and. nb .gt. 1000)) then
!           IL S'AGIT D'UN POINT DE JONCTION DE FISSURE, ON LE MARQUE AVEC
!           UN ALPHA EGAL A -1
                   ainter(zxain*(ipi-1)+4) = -1.d0
                endif
!
                if (ajout) then
                    do k = 1, ndime
                        pinref(ndime*(ipi-1)+k)=cref(k)
                    enddo
                endif
!           ON RAJOUTE A LA LISTE DES COORDONNEES DE REFERENCE
            endif
!
        endif
!
100 continue
!
!       NB DE POINTS D'INTERSECTION
    ninter=ipi
!       NB DE POINTS D'INTERSECTION = NOEUD SOMMET
    npts  =ins
!       NB DE POINTS D'INTERSECTION = NOEUD MILIEU
    nptm  =inm
!
!     TRI DES POINTS D'INTERSECTION PAR ORDRE CROISSANT DES ARETES
    do 200 pd = 1, ninter-1
        pp=pd
        do 201 i = pp, ninter
            if (ainter(zxain*(i-1)+1) .lt. ainter(zxain*(pp-1)+1)) pp=i
201     continue
        do 202 k = 1, 4
            tampor(k)=ainter(zxain*(pp-1)+k)
            ainter(zxain*(pp-1)+k)=ainter(zxain*(pd-1)+k)
            ainter(zxain*(pd-1)+k)=tampor(k)
202     continue
        do 203 k = 1, ndim
            tampor(k)=pinter(ndim*(pp-1)+k)
            pinter(ndim*(pp-1)+k)=pinter(ndim*(pd-1)+k)
            pinter(ndim*(pd-1)+k)=tampor(k)
!  TRAITEMENT DE PINREF
            tampor(k)=pinref(ndim*(pp-1)+k)
            pinref(ndim*(pp-1)+k)=pinref(ndim*(pd-1)+k)
            pinref(ndim*(pd-1)+k)=tampor(k)
203     continue
200 continue
!
!      TRI DES POINTS POUR QUE LE POLYGONE IP1,IP2,IP3,IP4 SOIT CONVEXE
!      IP1 IP2 ET IP3 ONT UN SOMMET EN COMMUN
!      IP1 ET IP4 N ONT PAS DE SOMMET COMMUN
    if (ninter .eq. 4 .and. npts .eq. 0) then
        a1=nint(ainter(1))
        do 220 ia = 2, 3
            a2=nint(ainter(zxain*(ia-1)+1))
            papillon=.true.
            do 224 i = 1, 2
                do 225 j = 1, 2
                    if (ar(a1,i) .eq. ar(a2,j)) papillon=.false.
225             continue
224         continue
            if (papillon) then
!        CONFIGURATION RENCONTREE PAR EXEMPLE DANS SSNV510C
                do 226 k = 1, 4
                    tampor(k)=ainter(zxain*(ia-1)+k)
                    ainter(zxain*(ia-1)+k)=ainter(zxain*(4-1)+k)
                    ainter(zxain*(4-1)+k)=tampor(k)
226             continue
                do 227 k = 1, ndim
                    tampor(k)=pinter(ndim*(ia-1)+k)
                    pinter(ndim*(ia-1)+k)=pinter(ndim*(4-1)+k)
                    pinter(ndim*(4-1)+k)=tampor(k)
!  TRAITEMENT DE PINREF
                    tampor(k)=pinref(ndim*(ia-1)+k)
                    pinref(ndim*(ia-1)+k)=pinref(ndim*(4-1)+k)
                    pinref(ndim*(4-1)+k)=tampor(k)
227             continue
            endif
220     continue
    endif
!
!       LES CONFIGS 3D / NINTER=4,NPTS=2 / NINTER=6,NPTS=2/ NE RESSEMBLENT PAS AUTRES 
!       CONFIG IMPLEMENTEES POUR CETTE CONFIG LE NOEUD MILIEU M EST CONSIDERE COMME UN 
!       POINT D INTERSECTION SANS QUE L ARETE NE SOIT COUPEE "TRANSVERSALEMENT" EN M
!       L ALGO NE DOIT PAS LE TRAITER COMME UN POINT D INTERSECTION CLASSIQUE
!             ==> C EST UN CAS DEGENERE TRES ENNUYEUX
    if ((ndime.eq.3.and.ninter .eq. 4 .and. npts.eq.2.and.cut).or.&
        (ndime.eq.3.and.ninter .eq. 6 .and. npts.eq.2.and.cut)) then
!      SOLUTION : ON MET LE POINT MILIEU EN DERNIERE POSITION => POUR LE DISTINGUER DES AUTRES PI
      noeua=nint(ainter(2))
      noeub=nint(ainter(zxain+2))
      im=0
      do 230 i=1,6
        do 231 j=1,2
          if (cnset(nnose*(it-1)+ar(i,j)).eq.noeua.and.&
            cnset(nnose*(it-1)+ar(i,3-j)).eq.noeub) im=i
231     continue
230   continue
      ASSERT(im.gt.0)
      do ipt=1,ninter-1
        if (nint(ainter(zxain*(ipt-1)+1)).eq.im) then
          do i=1,(zxain-1)
            rbid=ainter(zxain*(ninter-1)+i)
            ainter(zxain*(ninter-1)+i)=ainter(zxain*(ipt-1)+i)
            ainter(zxain*(ipt-1)+i)=rbid
          enddo
          do  k=1,ndim
            tampor(k)=pinter(ndim*(ninter-1)+k)
            pinter(ndim*(ninter-1)+k)=pinter(ndim*(ipt-1)+k)
            pinter(ndim*(ipt-1)+k)=tampor(k)
!  TRAITEMENT DE PINREF
            tampor(k)=pinref(ndim*(ninter-1)+k)
            pinref(ndim*(ninter-1)+k)=pinref(ndim*(ipt-1)+k)
            pinref(ndim*(ipt-1)+k)=tampor(k)
         enddo
        endif
      enddo
!    ON FORCE ainter(zxain*(4-1)+1)=0
!       CAR LE NOEUD MILIEU EST REELLEMENT DANS LE PLAN DE LA FISSURE
!       ainter(zxain*(4-1)+1)=0.d0
    endif
!
    if (.not.cut) goto 999
!
! VERIFICATION DES CONFIGURATIONS AUTORISEES
    if (ifiss.eq.1) then
       call xerfis(ndime, ninter, npts, nptm)
    endif
!
! CALCUL DES POINTS MILIEUX
!
    pmmax=pmmaxi(ndim)
    call loncar(ndim, typma, tabco, lonref)
!
    jonc = .false.
    inter = 0
    do i = 1, ifiss
       inter = inter + coupe(i)
    end do
    if (inter.gt.1) jonc = .true.
!
    if (ndim .le. 2) call xalgo2(ndim, elrefp, nnop, it, nnose,&
                                 cnset, typma, ndime, igeom, lsnelp,&
                                 pmilie, ninter, ainter, ar, npts,&
                                 nptm, pmmax, nmilie, mfis, lonref,&
                                 pinref, pintt, pmitt, jonc)
!
    if (ndim .eq. 3) call xalgo3(ndim, elrefp, nnop, it, nnose,&
                                 cnset, typma, ndime, igeom, lsnelp,&
                                 pmilie, ninter, ainter, ar, npts,&
                                 nptm, pmmax, nmilie, mfis, lonref,&
                                 pinref, pintt, pmitt, jonc)
!
999 continue
end subroutine
