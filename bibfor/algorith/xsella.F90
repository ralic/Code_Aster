subroutine xsella(fiss, nbno, narz, tabnoz, pickno,&
                  nbpino)
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
!
#include "jeveux.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/jeveuo.h"
    integer :: nbno, narz
    integer :: tabnoz(3, narz), pickno(narz), nbpino
    character(len=8) :: fiss
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION)
!
! CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT:
!                    (VOIR BOOK VI 15/07/05)
!    - SELECTION DES NOEUDS POUR LES LAGRANGES
!
! ----------------------------------------------------------------------
!
! IN  FISS   : NOM DE LA FISSURE
! IN  NBNO   : NOMBRE DE NOEUDS DU MAILLAGE
! IN  NARZ   : NOMBRE D'ARETES COUPEES
! IN  TABNOZ : TABLEAU DES NOEUDS EXTREMITES ET NOEUD MILIEU
! OUT PICKNO : NUMEROS DES NOEUDS SELECTIONNES
! OUT NBPINO : NOMBRE DE NOEUDS SELECTIONNES
!
!
!
!
    integer :: i, j, k, ik, cptno, deja, noeud(2*narz), tabdir(narz, 2)
    integer :: scorno(2*narz), scorar(narz), ia, max, bestar, no1, no2
    integer :: bestno, noconn, narcas, liarca(narz), nar, ni, nj
    integer :: tabno(narz, 3), ii, cpt
    real(kind=8) :: scorn2(2*narz), scora2(narz), li, lj, maxr
    character(len=19) :: cnsln
    integer :: jlnsv
!
! ----------------------------------------------------------------------
!
! --- TRANSFO. CHAM_NO -> CHAM_NO_S DE LA LEVEL SET NORMALE
!
    cnsln = '&&XSELLA.CNSLN'
    call cnocns(fiss(1:8)//'.LNNO', 'V', cnsln)
    call jeveuo(cnsln(1:19)//'.CNSV', 'L', jlnsv)
!
! --- INITIALISATIONS
!
    nar = narz
    do 100 i = 1, nar
        do 101 j = 1, 3
            tabno(i,j) = tabnoz(j,i)
101      continue
100  end do
    cptno = 0
    do 200 i = 1, 2*nar
        scorno(i) = 0
        noeud(i) = 0
200  end do
    cpt = 0
!
! --- CALCUL DE SCORNO : NB D'ARETES CONNECTESS AU NOEUD
! --- CALCUL DE SCORN2 : VALEUR ABSOLUE DE LA LEVEL SET NORMALE
!
    do 201 i = 1, nar
        do 202 j = 1, 2
            deja=0
            do 203 k = 1, cpt
                if (tabno(i,j) .eq. noeud(k)) then
                    deja = 1
                    ik = k
                endif
203          continue
            if (deja .eq. 0) then
                cpt = cpt+1
                noeud(cpt) = tabno(i,j)
                tabdir(i,j) = cpt
                scorn2(cpt) = abs(zr(jlnsv-1+tabno(i,j)))
            else
                tabdir(i,j) = ik
            endif
            scorno(tabdir(i,j)) = scorno(tabdir(i,j))+1
202      continue
201  end do
!
! --- BOUCLE TANT QUE TOUTES LES ARETES NE SONT PAS CASSEES
!
    do 210 ii = 1, nbno
!
        if (nar .eq. 0) goto 300
!
! --- CALCUL SCORAR : DIFF DE SCORE POUR CHAQUE NOEUD
! --- CALCUL SCOAR2 : RAPPORT DES LEVEL SETS
!
        do 211 ia = 1, nar
            ni = scorno(tabdir(ia,1))
            nj = scorno(tabdir(ia,2))
            scorar(ia) = abs(ni-nj)
            li = abs(zr(jlnsv-1+tabno(ia,1)))
            lj = abs(zr(jlnsv-1+tabno(ia,2)))
            if (ni .gt. nj) scora2(ia)=li/(li+lj)
            if (ni .lt. nj) scora2(ia)=lj/(li+lj)
            if (ni .eq. nj) scora2(ia)=min(li,lj)/(li+lj)
211      continue
!
! --- MEILLEURE ARETE : PICK MEILLEUR NOEUD
!
        max = -1
        maxr = -1.d0
        do 212 ia = 1, nar
            if ((scorar(ia).gt.max) .or. (scorar(ia) .eq.max.and.scora2(ia).ge.maxr)) then
                max = scorar(ia)
                maxr = scora2(ia)
                bestar = ia
            endif
212      continue
!
        no1 = tabdir(bestar,1)
        no2 = tabdir(bestar,2)
        bestno = no1
        if ((scorno(no2).gt.scorno(no1)) .or.&
            (scorno(no2).eq.scorno( no1).and. scorn2(no2).lt.scorn2(no1))) then
            bestno=no2
        endif
        cptno = cptno+1
        pickno(cptno) = noeud(bestno)
!
! --- UPDATE SCORE DES NOEUDS
!
        do 220 i = 1, nar
            do 221 j = 1, 2
                if (tabdir(i,j) .eq. bestno) then
                    scorno(bestno) = scorno(bestno)-1
                    noconn = tabdir(i,3-j)
                    scorno(noconn) = scorno(noconn)-1
                endif
221          continue
220      continue
!
! --- ON LISTE LES ARETES CONNECTEES A�BESTNO PUIS ON LES CASSE
!
        narcas = 0
        do 230 ia = 1, nar
            if (tabdir(ia,1) .eq. bestno .or. tabdir(ia,2) .eq. bestno) then
                narcas = narcas+1
                liarca(narcas) = ia
            endif
230      continue
!
! --- FEINTE : ON LES SUPPRIME EN PARTANT PAR LA FIN !
!
        do 231 ia = narcas, 1, -1
!          TABNO(LIARCA(IA),:)=[]
!          TABDIR(LIARCA(IA),:)=[]
            do 232 i = liarca(ia), nar-1
                tabno(i,1) = tabno(i+1,1)
                tabno(i,2) = tabno(i+1,2)
                tabno(i,3) = tabno(i+1,3)
                tabdir(i,1)= tabdir(i+1,1)
                tabdir(i,2)= tabdir(i+1,2)
232          continue
            tabno(nar,1) = 0
            tabno(nar,2) = 0
            tabno(nar,3) = 0
            tabdir(nar,1) = 0
            tabdir(nar,2) = 0
            nar = nar-1
231      continue
210  end do
300  continue
!
! --- NOMBRE DE PICKED NODES
!
    nbpino = cptno
!
! --- DESTRUCTION DES OBJETS TEMPORAIRES
!
    call detrsd('CHAM_NO_S', cnsln)
!
end subroutine
