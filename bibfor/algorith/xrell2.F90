subroutine xrell2(tabnoz, ndim, narz, tabcoz, tabcrz,&
                  lgroup, nliseq)
!
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
!
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/xneuvi.h"
    integer :: narz, ndim
    integer :: tabnoz(3, narz)
    real(kind=8) :: tabcoz(ndim, narz), tabcrz(narz)
    character(len=19) :: nliseq
    logical :: lgroup
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION)
!
!     CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT - V2:
!                    (VOIR BOOK VI 30/09/05)
!    - CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
! ----------------------------------------------------------------------
!
!
! IN  NAR    : NOMBRE D'ARETES COUPEES
! IN  TABNOZ : TABLEAU DES NOEUDS EXTREMITES ET NOEUD MILIEU
! IN  TABCOZ : TABLEAU DES COORDONNEES DES POINTS D'INTERSECTION
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  LGROUP : .TRUE. SI CONSTRUCTION GROUPES ARETES
! OUT NLISRL : LISTE REL. LIN. POUR V1 ET V2
! OUT NLISCO : LISTE REL. LIN. POUR V1 ET V2
! OUT NLISEQ : LISTE REL. LIN. POUR V2 SEULEMENT
!
!
!
!
    integer :: nar, i, j, tabno(narz, 3), nbarvi, nbarhy, scorno(2*narz)
    integer :: cpt, deja, k, noeud(2*narz), ik, tabdir(narz, 2), nbno, ii, ia
    integer :: scorar(narz), bestar, maxi, ir, t1(2*narz)
    integer :: mi, ma, npaq, t2(narz), nreleq, ip, dimeq, eq(narz), ie, ipaq
    integer :: liseqt(narz, 2), jlis1
    integer ::  npil
    integer :: ifm, niv
    real(kind=8) :: tabco(narz, ndim), tabcr(narz)
    integer :: nunoa, nunob
    character(len=8) :: k8bid
    integer :: iarg
    logical :: pilo
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
! --- INITIALISATIONS
!
    nar = narz
    pilo = .false.
    do 100 i = 1, nar
        do 101 j = 1, 3
            tabno(i,j)=tabnoz(j,i)
101      continue
        do 102 j = 1, ndim
            tabco(i,j)=tabcoz(j,i)
102      continue
        tabcr(i)=tabcrz(i)
100  end do
!     COMPTEUR D'ARETES HYPERSTATIQUES
    nbarhy = 0
    do 200 i = 1, 2*nar
        scorno(i)=0
        noeud(i)=0
200  end do
!
! --- SELECTION DES ARETES VITALES
!
!
! --- CALCUL DE SCORNO : NB D'ARETES CONNECTEES AU NOEUD
!
    cpt=0
    do 201 i = 1, nar
        do 202 j = 1, 2
            deja=0
            do 203 k = 1, cpt
                if (tabno(i,j) .eq. noeud(k)) then
                    deja=1
                    ik=k
                endif
203          continue
            if (deja .eq. 0) then
                cpt=cpt+1
                noeud(cpt)=tabno(i,j)
                tabdir(i,j)=cpt
            else
                tabdir(i,j)=ik
            endif
            scorno(tabdir(i,j))=scorno(tabdir(i,j))+1
202      continue
201  end do
!
! --- NOMBRE DE NOEUDS
!
    nbno=cpt
!
!     BOUCLE TANT QU'IL RESTE DES ARETES HYPERSTATIQUES
    do 210 ii = 1, narz
!
!       CALCUL SCORAR : MIN DE SCORE POUR CHAQUE NOEUD
!       CALCUL SCOAR2 : LONGUEUR DE L'ARETE
        do 211 ia = 1, nar
            scorar(ia) = min(scorno(tabdir(ia,1)),scorno(tabdir(ia,2)) )
!
!     SI PILOTAGE, ON AFFECTE UN SCORE DE 1 AUX NOEUDS INTERSECTES
            if (getexm('PILOTAGE','DIRE_PILO') .eq. 1) then
                call getvtx('PILOTAGE', 'DIRE_PILO', 1, iarg, 0,&
                            k8bid, npil)
                npil=-npil
                if (npil .ge. 1) then
                    pilo = .true.
                    if (tabdir(ia,1) .eq. tabdir(ia,2)) then
                        scorar(ia)=1
                    endif
                endif
            endif
211      continue
!
!       ARETE AU SCORE MAX
        maxi=-1
        do 212 ia = 1, nar
            if (scorar(ia) .gt. maxi) then
                maxi =scorar(ia)
                bestar=ia
            else
                if (scorar(ia) .eq. maxi) then
                    if (tabcr(ia) .gt. tabcr(bestar)) then
                        bestar=ia
                    endif
                endif
            endif
212      continue
!
!       SI SCORE DE LA MEILLEURE ARETE =1 ALORS IL RESTE QUE DES ARETES
!       VITALES ET DONC ON SORT DE LA BOUCLE 210
        if (maxi .eq. 1) then
            goto 299
        else
!         ON SAUVE BESTAR DANS ARHY ET NOARHY
            nbarhy=nbarhy+1
!
!         UPDATE SCORE DES NOEUDS SI ON SUPPRIMAIT LA MEILLEURE ARETE
            scorno(tabdir(bestar,1))=scorno(tabdir(bestar,1))-1
            scorno(tabdir(bestar,2))=scorno(tabdir(bestar,2))-1
!
!         ON SUPPRIME EFFECTIVEMENT LA MEILLEURE ARETE
            do 220 i = bestar, nar-1
                tabno(i,1) = tabno(i+1,1)
                tabno(i,2) = tabno(i+1,2)
                tabno(i,3) = tabno(i+1,3)
                tabdir(i,1)= tabdir(i+1,1)
                tabdir(i,2)= tabdir(i+1,2)
                do 230 j = 1, ndim
                    tabco(i,j) = tabco(i+1,j)
230              continue
                tabcr(i)=tabcr(i+1)
220          continue
            tabno(nar,1)=0
            tabno(nar,2)=0
            tabno(nar,3)=0
            tabdir(nar,1)=0
            tabdir(nar,2)=0
            do 240 j = 1, ndim
                tabco(nar,j)=0
240          continue
            tabcr(nar) = 0.d0
            nar=nar-1
        endif
!
210  end do
!
299  continue
!
    if (.not.pilo) then
        call xneuvi(narz, nar, nbno, tabdir, scorno,&
                    noeud, nliseq)
    endif
!
!     NOMBRE D'ARETES VITALES : NB D'ARETES RESTANTES
    nbarvi=nar
!
!     VERIF SI NB ARETES HYPERS + NB ARETES VITALES = NB ARETES INITIAL
    ASSERT(nbarhy+nbarvi.eq.narz)
!
!
!     ATTENTION : MAINTENANT, TABNO ET TABDIR SONT DE LONGUEUR NBARVI
!
!
    if (lgroup) goto 600
!     CREATION DU TABLEAU TEMPORAIRE DES RELATIONS D'EGALITE : LISEQT
!     UNIQUEMENT AVEC LES ARETES VITALES
    nreleq=0
    do 602 ir = 1, nbarvi
        nunoa=noeud(tabdir(ir,1))
        nunob=noeud(tabdir(ir,2))
        nreleq=nreleq+1
        liseqt(nreleq,1)=nunoa
        liseqt(nreleq,2)=nunob
602  end do
    goto 700
!
! --- DETECTION PAQUETS D ARETES
!
! --- CREATION DES RELATIONS D'EGALITE A IMPOSER
! --- TABLEAU T1 : PAQUET DE NOEUDS  DIM : NBNO
!
600  continue
    ipaq=0
    do 400 i = 1, 2*narz
        t1(i)=0
400  end do
    do 401 ir = 1, nbarvi
        if (t1(tabdir(ir,1)) .eq. 0 .and. t1(tabdir(ir,2)) .eq. 0) then
            ipaq=ipaq+1
            t1(tabdir(ir,1))=ipaq
            t1(tabdir(ir,2))=ipaq
            elseif (t1(tabdir(ir,1)).eq.0 .and. t1(tabdir(ir,2)).ne.0)&
        then
            t1(tabdir(ir,1))=t1(tabdir(ir,2))
            elseif (t1(tabdir(ir,1)).ne.0 .and. t1(tabdir(ir,2)).eq.0)&
        then
            t1(tabdir(ir,2))=t1(tabdir(ir,1))
            elseif (t1(tabdir(ir,1)).ne.0 .and. t1(tabdir(ir,2)).ne.0)&
        then
!         SI ILS APPARTIENNET A DEUX PAQUETS DIFFERENTS
!         ALORS ON REGROUPE LES PAQUETS
            if (t1(tabdir(ir,1)) .ne. t1(tabdir(ir,2))) then
                mi=min(t1(tabdir(ir,1)) , t1(tabdir(ir,2)))
                ma=max(t1(tabdir(ir,1)) , t1(tabdir(ir,2)))
                do 402 i = 1, nbno
                    if (t1(i) .eq. ma) t1(i)=mi
402              continue
            endif
        endif
401  end do
!     NOMBRE DE PAQUETS
    npaq=ipaq
!
!     TABLEAU T2 : PAQUETS D'ARETES  (DIM : NBARVI)
    do 410 ia = 1, nbarvi
        ASSERT(t1(tabdir(ia, 1)) .eq. t1(tabdir(ia, 2)))
        t2(ia)=t1(tabdir(ia,1))
410  end do
!
!     CREATION DU TABLEAU TEMPORAIRE DES RELATIONS D'EGALITE : LISEQT
    nreleq=0
    do 440 ip = 1, npaq
        dimeq=0
!       RECHERCHE DES ARETES DU PAQUET
        do 441 ia = 1, nbarvi
            if (ip .eq. t2(ia)) then
                dimeq=dimeq+1
                eq(dimeq)=tabno(ia,3)
            endif
441      continue
        ASSERT(dimeq-1.ge.0)
        do 442 ie = 1, dimeq-1
            nreleq=nreleq+1
            liseqt(nreleq,1)=eq(ie)
            liseqt(nreleq,2)=eq(ie+1)
442      continue
440  end do
!
! --- FIN DETECTION DES PAQUETS
!
700  continue
!
    write(ifm,*)'NOMBRE DE RELATIONS D''EGALITE : ',nreleq
!
!     STOCKAGE DE LISEQT
    if (nreleq .gt. 0) then
        call wkvect(nliseq, 'G V I', nreleq*2, jlis1)
        do 450 ie = 1, nreleq
            zi(jlis1-1+2*(ie-1)+1)=liseqt(ie,1)
            zi(jlis1-1+2*(ie-1)+2)=liseqt(ie,2)
450      continue
    endif
    call jedema()
end subroutine
