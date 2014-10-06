subroutine xrell2(tabl_node     , nb_dim      , nb_edgez, tabl_ptin, tabl_scor,&
                  l_create_group, sdline_crack, l_pilo)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/xneuvi.h"
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
! aslint: disable=W1306
!
    integer, intent(in) :: nb_edgez
    integer, intent(in) :: nb_dim
    integer, intent(in) :: tabl_node(3, nb_edgez)
    real(kind=8), intent(in) :: tabl_ptin(nb_dim, nb_edgez)
    real(kind=8), intent(in) :: tabl_scor(nb_edgez)
    character(len=14), intent(in) :: sdline_crack
    aster_logical, intent(in) :: l_create_group
    aster_logical, intent(in) :: l_pilo
!
! --------------------------------------------------------------------------------------------------
!
! XFEM - Contact definition
!
! Create list of linear relations (algorithm 2)
!
! --------------------------------------------------------------------------------------------------
!
! (VOIR BOOK VI 30/09/05)
!    - CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
! --------------------------------------------------------------------------------------------------
!
!
! In  nb_dim         : dimension of space
! In  sdline_crack   : name of datastructure of linear relations for crack
! In  nb_edgez       : number of cut edges
! In  tabl_node      : table of nodes for edges (middle et vertex nodes)
! In  tabl_ptin      : table of intersection points
! In  tabl_scor      : table of score
! In  l_create_group : .true. if create group
! In  l_pilo         : .true. if creation of linear relations for continuation method (PILOTAGE)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_edge, i, j, tabno(nb_edgez, 3), nbarvi, nbarhy, scorno(2*nb_edgez)
    integer :: cpt, deja, k, noeud(2*nb_edgez), ik, tabdir(nb_edgez, 2), nbno, ii, ia
    integer :: scorar(nb_edgez), bestar, maxi, ir, t1(2*nb_edgez)
    integer :: mi, ma, npaq, t2(nb_edgez), nreleq, ip, dimeq, eq(nb_edgez), ie, ipaq
    integer :: liseqt(nb_edgez, 2), jlis1
    integer :: npil
    real(kind=8) :: tabco(nb_edgez, nb_dim), tabcr(nb_edgez)
    integer :: nunoa, nunob
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nb_edge = nb_edgez
    do i = 1, nb_edge
        do j = 1, 3
            tabno(i,j)=tabl_node(j,i)
        end do
        do j = 1, nb_dim
            tabco(i,j)=tabl_ptin(j,i)
        end do
        tabcr(i)=tabl_scor(i)
    end do
!     COMPTEUR D'ARETES HYPERSTATIQUES
    nbarhy = 0
    do i = 1, 2*nb_edge
        scorno(i)=0
        noeud(i)=0
    end do
!
! --- SELECTION DES ARETES VITALES
!
!
! --- CALCUL DE SCORNO : NB D'ARETES CONNECTEES AU NOEUD
!
    cpt=0
    do i = 1, nb_edge
        do j = 1, 2
            deja=0
            do k = 1, cpt
                if (tabno(i,j) .eq. noeud(k)) then
                    deja=1
                    ik=k
                endif
            end do
            if (deja .eq. 0) then
                cpt=cpt+1
                noeud(cpt)=tabno(i,j)
                tabdir(i,j)=cpt
            else
                tabdir(i,j)=ik
            endif
            scorno(tabdir(i,j))=scorno(tabdir(i,j))+1
        end do
    end do
!
! --- NOMBRE DE NOEUDS
!
    nbno=cpt
!
!     BOUCLE TANT QU'IL RESTE DES ARETES HYPERSTATIQUES
    do ii = 1, nb_edgez
!
!       CALCUL SCORAR : MIN DE SCORE POUR CHAQUE NOEUD
!       CALCUL SCOAR2 : LONGUEUR DE L'ARETE
        do ia = 1, nb_edge
            scorar(ia) = min(scorno(tabdir(ia,1)),scorno(tabdir(ia,2)) )
!
!     SI PILOTAGE, ON AFFECTE UN SCORE DE 1 AUX NOEUDS INTERSECTES
            if (l_pilo) then
                call getvtx('PILOTAGE', 'DIRE_PILO', iocc=1, nbval=0, nbret=npil)
                npil=-npil
                if (npil .ge. 1) then
                    if (tabdir(ia,1) .eq. tabdir(ia,2)) then
                        scorar(ia)=1
                    endif
                endif
            endif
        end do
!
!       ARETE AU SCORE MAX
        maxi=-1
        do ia = 1, nb_edge
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
        end do
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
            do i = bestar, nb_edge-1
                tabno(i,1) = tabno(i+1,1)
                tabno(i,2) = tabno(i+1,2)
                tabno(i,3) = tabno(i+1,3)
                tabdir(i,1)= tabdir(i+1,1)
                tabdir(i,2)= tabdir(i+1,2)
                do j = 1, nb_dim
                    tabco(i,j) = tabco(i+1,j)
                end do
                tabcr(i)=tabcr(i+1)
            end do
            tabno(nb_edge,1)=0
            tabno(nb_edge,2)=0
            tabno(nb_edge,3)=0
            tabdir(nb_edge,1)=0
            tabdir(nb_edge,2)=0
            do j = 1, nb_dim
                tabco(nb_edge,j)=0
            end do
            tabcr(nb_edge) = 0.d0
            nb_edge=nb_edge-1
        endif
    end do
!
299 continue
!
    if (.not.l_pilo) then
        call xneuvi(nb_edgez, nb_edge, nbno, tabdir, scorno,&
                    noeud, sdline_crack)
    endif
!
!     NOMBRE D'ARETES VITALES : NB D'ARETES RESTANTES
    nbarvi=nb_edge
!
!     VERIF SI NB ARETES HYPERS + NB ARETES VITALES = NB ARETES INITIAL
    ASSERT(nbarhy+nbarvi.eq.nb_edgez)
!
!
!     ATTENTION : MAINTENANT, TABNO ET TABDIR SONT DE LONGUEUR NBARVI
!
!
    if (l_create_group) goto 600
!     CREATION DU TABLEAU TEMPORAIRE DES RELATIONS D'EGALITE : LISEQT
!     UNIQUEMENT AVEC LES ARETES VITALES
    nreleq=0
    do  ir = 1, nbarvi
        nunoa=noeud(tabdir(ir,1))
        nunob=noeud(tabdir(ir,2))
        nreleq=nreleq+1
        liseqt(nreleq,1)=nunoa
        liseqt(nreleq,2)=nunob
    end do
    goto 700
!
! --- DETECTION PAQUETS D ARETES
!
! --- CREATION DES RELATIONS D'EGALITE A IMPOSER
! --- TABLEAU T1 : PAQUET DE NOEUDS  DIM : NBNO
!
600 continue
    ipaq=0
    do i = 1, 2*nb_edgez
        t1(i)=0
    end do
    do ir = 1, nbarvi
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
                do i = 1, nbno
                    if (t1(i) .eq. ma) t1(i)=mi
                end do
            endif
        endif
    end do
!     NOMBRE DE PAQUETS
    npaq=ipaq
!
!     TABLEAU T2 : PAQUETS D'ARETES  (DIM : NBARVI)
    do ia = 1, nbarvi
        ASSERT(t1(tabdir(ia, 1)) .eq. t1(tabdir(ia, 2)))
        t2(ia)=t1(tabdir(ia,1))
    end do
!
!     CREATION DU TABLEAU TEMPORAIRE DES RELATIONS D'EGALITE : LISEQT
    nreleq=0
    do ip = 1, npaq
        dimeq=0
!       RECHERCHE DES ARETES DU PAQUET
        do ia = 1, nbarvi
            if (ip .eq. t2(ia)) then
                dimeq=dimeq+1
                eq(dimeq)=tabno(ia,3)
            endif
        end do
        ASSERT(dimeq-1.ge.0)
        do  ie = 1, dimeq-1
            nreleq=nreleq+1
            liseqt(nreleq,1)=eq(ie)
            liseqt(nreleq,2)=eq(ie+1)
        end do
    end do
!
! --- FIN DETECTION DES PAQUETS
!
700 continue
!
!     STOCKAGE DE LISEQT
    if (nreleq .gt. 0) then
        call wkvect(sdline_crack, 'G V I', nreleq*2, jlis1)
        do ie = 1, nreleq
            zi(jlis1-1+2*(ie-1)+1)=liseqt(ie,1)
            zi(jlis1-1+2*(ie-1)+2)=liseqt(ie,2)
        end do
    endif
    call jedema()
end subroutine
