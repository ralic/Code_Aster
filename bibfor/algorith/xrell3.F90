subroutine xrell3(tabnoz, nbarz, crack, sdline_crack,&
                  tabl_scor, tabai, l_ainter)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cnocns.h"
#include "asterfort/conare.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexatr.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
#include "asterfort/xneuvi.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/xxmmvd.h"
#include "asterfort/xelfis_lists.h"
    integer, intent(in) :: nbarz
    integer, intent(in) :: tabnoz(3, nbarz)
    character(len=8) :: crack
    character(len=14), intent(in) :: sdline_crack
    aster_logical, intent(in) :: l_ainter
    real(kind=8), intent(in) :: tabl_scor(nbarz)
    character(len=19) :: tabai
!
! ----------------------------------------------------------------------
!
!                  ROUTINE XFEM (PREPARATION)
!
!     CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT - V3:
!                    (VOIR BOOK VI 30/09/05)
!    - CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!    - REMPLIR LA 5EME COMPOSANTE DE TOPOFAC.AI
!    - INITIALISER LES STATUTS NVIT DES ARETES
!
! ----------------------------------------------------------------------
!
! IN  MESH   : NOM DU MAILLAGE
! IN  NBARZ  : NOMBRE D'ARETES COUPEES
! IN  TABNOZ : TABLEAU DES NOEUDS EXTREMITES ET NOEUD MILIEU
! IN  NDIM   : DIMENSION DU PROBLEME
! OUT NLISRL : LISTE REL. LIN. POUR V1 ET V2
!
    integer :: j, k, ik, deja, ino, iino, jno, iar, jar
    integer :: mini, maxi, bestar, ncta, nunoA, nunoB, ncte
    integer :: nbar, nbno, nreleq, nbarhy, cpt, ieq, ipaq, npaq
    integer :: tabno(nbarz,3), scorno(2*nbarz), noeud(2*nbarz), tabdir(nbarz,2)
    integer :: t1(2*nbarz), t2(nbarz), liseqt(nbarz,2), scorar(nbarz), tabait(nbarz)
    integer :: jlnsv, jlis1, ier, jcntes, jcnte2, jtabai
    aster_logical :: tabhyp(nbarz), tabreleq(nbarz), marqno(2*nbarz), tabpaq(nbarz)
    real(kind=8) :: tabcr(nbarz), lonA, lonB, lon, lon_min
    character(len=19) :: cnsln
    integer, pointer :: connectant(:) => null()
    real(kind=8), parameter :: seuil=0.001d0
!
!   tolerances --- absolue et relative --- pour determiner si deux valeurs du critere sont egales
    real(kind=8), parameter :: atol=1.e-12
    real(kind=8), parameter :: rtol=1.e-12
    aster_logical :: near
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbar = nbarz
    do iar = 1, nbar
        do j = 1, 3
            tabno(iar,j) = tabnoz(j,iar)
        enddo
        tabcr(iar) = tabl_scor(iar)
    end do
!
    do ino = 1, 2*nbar
        scorno(ino) = 0
        noeud(ino) = 0
        marqno(ino) = .false.
    end do
!
! --- Initialisation des scores de noeuds
!
    cpt = 0
    do iar = 1, nbar
        do j = 1, 2
            deja = 0
            do k = 1, cpt
                if (tabno(iar,j) .eq. noeud(k)) then
                    deja = 1
                    ik = k
                endif
            enddo
            if (deja .eq. 0) then
                cpt = cpt+1
                noeud(cpt) = tabno(iar,j)
                tabdir(iar,j) = cpt
            else
                tabdir(iar,j) = ik
            endif
            scorno(tabdir(iar,j)) = scorno(tabdir(iar,j)) + 1
        enddo
    end do
!
! --- Nombre de noeuds
!
    nbno = cpt
! 
! --- Variables de détection des arêtes hyperstatiques et des relations d'églalité
!
    nbarhy = 0
    nreleq = 0
    do iar = 1, nbar
        tabhyp(iar) = .false.
        tabreleq(iar) = .false.
        tabpaq(iar) = .false.
    end do
!
! --- Tableau temporaire des statuts d'arêtes (5ème composante de TOPOFAC.AI)
!     Valeur initiale = 1. Cette valeur peut changer à 0 dans certains cas
!
    do iar = 1, nbar
        tabait(iar) = 1
    enddo
!
! --- SD pour le calcul des distances relatives au sens de la lsn
!
    cnsln = '&&XRELL3.CNSLN'
    call cnocns(crack(1:8)//'.LNNO', 'V', cnsln)
    call jeveuo(cnsln(1:19)//'.CNSV', 'L', jlnsv)
!
!*****************************************************************************************
! --- DETECTION PAQUETS D ARETES
!*****************************************************************************************
!
! --- Tableau T1: paquets de noeuds
!
    ipaq=0
    do ino = 1, 2*nbar
        t1(ino) = 0
    end do
!
    do iar = 1, nbar
        if     (t1(tabdir(iar,1)).eq.0 .and. t1(tabdir(iar,2)).eq.0) then
            ipaq = ipaq+1
            t1(tabdir(iar,1)) = ipaq
            t1(tabdir(iar,2)) = ipaq
        elseif (t1(tabdir(iar,1)).eq.0 .and. t1(tabdir(iar,2)).ne.0) then
            t1(tabdir(iar,1))=t1(tabdir(iar,2))
        elseif (t1(tabdir(iar,1)).ne.0 .and. t1(tabdir(iar,2)).eq.0) then
            t1(tabdir(iar,2))=t1(tabdir(iar,1))
        elseif (t1(tabdir(iar,1)).ne.0 .and. t1(tabdir(iar,2)).ne.0) then
!       Si les noeuds appartiennent à 2 paquets différents, on lie les paquets
            if (t1(tabdir(iar,1)) .ne. t1(tabdir(iar,2))) then
                mini = min(t1(tabdir(iar,1)) , t1(tabdir(iar,2)))
                maxi = max(t1(tabdir(iar,1)) , t1(tabdir(iar,2)))
                do ino = 1, nbno
                    if (t1(ino) .eq. maxi) t1(ino) = mini
                end do
            endif
        endif
    end do
!
! --- Tableau T2: paquets d'arêtes
!
    do iar = 1, nbar
        ASSERT(t1(tabdir(iar, 1)) .eq. t1(tabdir(iar, 2)))
        t2(iar)=t1(tabdir(iar,1))
    end do
!
! --- Ecriture des égalités sur les paquets. La 1ère arête du paquet porte l'égalité
!
    do iar = 1, nbar
        if(tabreleq(iar)) cycle
        if(tabpaq(iar)) cycle
!       Ici l'arête 'iar' est la première d'un paquet qui porte pas d'égalité    
        nreleq = nreleq + 1
        liseqt(nreleq,1) = noeud(tabdir(iar,1))
        liseqt(nreleq,2) = noeud(tabdir(iar,2))
        tabreleq(iar) = .true.
!       Marquage de toutes les arêtes du même paquet que 'iar'
        do jar = 1, nbar
            if(tabreleq(jar)) cycle
            if(tabpaq(jar)) cycle
            if(t2(jar) .eq. t2(iar)) then
                tabpaq(jar) = .true.
            endif
        enddo
    enddo
!
!   Nombre de paquets
!
    npaq = nreleq 
!
!   Attention: nreleq risque d'évoluer dans la suite de la routine, mais pas npaq
!
! --- Fin de la partie sur les composantes connexes
!******************************************************************************************
! --- Marquage (tabhyp) des arêtes hyperstatiques, càd arêtes de score > 1
!******************************************************************************************
!
    do
! --- Calcul des scores d'arêtes
        do iar = 1, nbar
            if(tabhyp(iar)) cycle
            scorar(iar) = min(scorno(tabdir(iar,1)),scorno(tabdir(iar,2)))
        end do
! --- Recherche de la "best arête"
        maxi = -1
        if(nbar.eq.0) maxi = 1
        do iar = 1, nbar
            if(tabhyp(iar)) cycle
            if (scorar(iar) .lt. maxi) cycle
            if (scorar(iar) .gt. maxi) then
                maxi = scorar(iar)
                bestar = iar
            else
!               ici, scorar(iar) = maxi
!               tabcr(ia) est-il egal a tabcr(bestar) ?
                near = abs(tabcr(iar)-tabcr(bestar)) .le. (atol + tabcr(bestar)*rtol)
! 
                if (tabcr(iar) .gt. tabcr(bestar) .and. .not. near) then
                    bestar = iar
                endif
            endif
        end do
! --- "Best arête" trouvée
        ASSERT(maxi .ge. 1)
        if (maxi .eq. 1) then
! --- Il ne reste que des arêtes de score 1
            exit
        endif
! --- Il s'agit d'une arête hyperstatique
        nbarhy = nbarhy + 1
        tabhyp(bestar) = .true. 
        scorno(tabdir(bestar,1)) = scorno(tabdir(bestar,1)) - 1
        scorno(tabdir(bestar,2)) = scorno(tabdir(bestar,2)) - 1
    end do
!
!******************************************************************************************
! --- Détection des noeuds CONNECTANTs et arêtes CONNECTEes
!******************************************************************************************
!
    call xneuvi(nbarz, nbar, nbno, tabdir, scorno,noeud, crack, tabhyp)
!
! --- Y A-T-IL DES ARETES CONNECTÉES ?
!
    call jeexin(crack(1:8)//'.CONNECTANT', ier)
    if (ier .eq. 0) then
        ncta = 0
    else
        call jeveuo(crack(1:8)//'.CONNECTANT', 'L', vi=connectant)
        call jelira(crack(1:8)//'.CONNECTANT', 'LONMAX', ncta)
        call jeveuo(crack(1:8)//'.CONNECTES ', 'L', jcntes)
    endif
!
! --- Boucle pour lier les Lagrange si l'interface est proche d'un sommet
!
    do ino = 1, ncta/3
        nunoA = connectant(3*(ino-1)+1)
        lonA = abs(zr(jlnsv-1+nunoA))
        ncte = connectant(3*(ino-1)+2)
        jcnte2 = connectant(3*(ino-1)+3)
!       Recherche de la distance minimale interface-sommet sur le paquet
        lon_min = 10.d0
        do jno = 1, ncte
            nunoB = zi(jcntes-1 + jcnte2 + jno)
            lonB = abs(zr(jlnsv-1+nunoB))
            if ((lonA+lonB) .gt. 0.d0) lon = lonA/(lonA+lonB)
            if ((lonA+lonB) .le. 0.d0) lon = 1.d0
            if (lon .lt. lon_min) lon_min = lon        
        enddo
!       Si on est proche du sommet connectant: relations d'égalité sur tout le paquet
        if (lon_min .lt. seuil) then
!           Marquer le sommet du paquet afin de modifier les statuts des arêtes
!           hyperstatiques qu'il lie
            do iino = 1, nbno
                if (noeud(iino) .eq. nunoA) marqno(iino) = .true. 
            enddo
            do jno = 1, ncte
                nunoB = zi(jcntes-1 + jcnte2 + jno)
!               Chercher l'indice 'iar' de l'arête nunoA-nunoB
                do iar = 1, nbar
                    if (((noeud(tabdir(iar,1)).eq.nunoA) .and. &
                         (noeud(tabdir(iar,2)).eq.nunoB)) .or.&
                        ((noeud(tabdir(iar,2)).eq.nunoA) .and. &
                         (noeud(tabdir(iar,1)).eq.nunoB))) then
!                       Vérifier si l'arête porte déjà une relation d'égalité
                        if (tabreleq(iar)) cycle
!                       Ecrire la relation d'égalité car elle n'existe pas encore
                        nreleq = nreleq + 1
                        liseqt(nreleq,1) = nunoA
                        liseqt(nreleq,2) = nunoB
                        tabreleq(iar) = .true.
!                       Mettre à 0 le statut (.AI) des arêtes du paquet sauf la première
                        if (jno .ne. 1) tabait(iar) = 0 
                    endif
                enddo
!               Marquer également tous les noeuds du paquets
                do iino = 1, nbno
                    if (noeud(iino) .eq. nunoB) marqno(iino) = .true. 
                enddo              
            enddo
        endif
    enddo
!
! --- Boucle sur les arêtes indépendantes non hyperstatiques
!
    do iar = 1, nbar
        if(tabhyp(iar) .or. tabreleq(iar)) cycle
        if((scorno(tabdir(iar,1)).gt.1) .or. (scorno(tabdir(iar,2)).gt.1)) cycle
!       On est sur une arête indépendante. Regardons si elle vérifie le critère "seuil"
        nunoA = noeud(tabdir(iar,1))
        nunoB = noeud(tabdir(iar,2))
        lonA = abs(zr(jlnsv-1+nunoA))
        lonB = abs(zr(jlnsv-1+nunoB))
        if ((lonA+lonB) .gt. 0.d0) lon = lonA/(lonA+lonB)
        if ((lonA+lonB) .le. 0.d0) lon = 1.d0
!       Vérifier qu'on prend la distance par rapport au sommet le plus proche
        if((lon .gt. 0.5d0) .and. (lon .ne. 1.d0)) lon = 1.d0-lon
!       Si on est inférieur au seuil: écriture de relation d'égalité
        if(lon .lt. seuil) then
            nreleq = nreleq + 1
            liseqt(nreleq,1) = nunoA
            liseqt(nreleq,2) = nunoB
!       Marquer l'arête et ses noeuds
            marqno(tabdir(iar,1)) = .true.
            marqno(tabdir(iar,2)) = .true.
        endif
    enddo
!
! --- Modifier les statuts (.AI) des arêtes hyperstatiques en fonction des paquets qu'elles lient
!
    do iar = 1, nbar
        if (.not. tabhyp(iar)) cycle
!       Vérifier qu'on est pas sur un noeud. Un noeud intersecté est forcément "vital"
        ASSERT(tabdir(iar,1).ne.tabdir(iar,2))         
        if (marqno(tabdir(iar,1)) .and. marqno(tabdir(iar,1))) then
!           Les deux noeuds de l'arête sont "marqués". Le statut change à 0
            tabait(iar) = 0
        elseif (marqno(tabdir(iar,1)) .or. marqno(tabdir(iar,1))) then
!           Un seul des deux noeuds est "marqué". Le statut se met à 0 et l'arête porte une égalité
            tabait(iar) = 0
            ASSERT(.not.tabreleq(iar))
            nreleq = nreleq + 1
            liseqt(nreleq,1) = noeud(tabdir(iar,1))
            liseqt(nreleq,2) = noeud(tabdir(iar,2))
            tabreleq(iar) = .true.
        endif
    enddo
!
! --- Fin du travail sur les relations d'égalité
!******************************************************************************************
! --- Stockage du tableau temporaire: tabait dans tabai (5ème composante de TOPOFAC.AI)
!******************************************************************************************
!
    if (l_ainter) then
        call jeexin(tabai,ier) 
        ASSERT(ier.eq.0)
        if (nbar.ne.0) then
            call wkvect(tabai, 'V V I', nbar*3, jtabai)
            do iar = 1, nbar
                zi(jtabai-1+3*(iar-1)+1) = noeud(tabdir(iar,1))
                zi(jtabai-1+3*(iar-1)+2) = noeud(tabdir(iar,2))
                zi(jtabai-1+3*(iar-1)+3) = tabait(iar)
            enddo
        endif
    endif
!
! --- Stockage du tableau temporaire: liseqt
!
    if (nreleq .gt. 0) then
        call jeexin(sdline_crack,ier)
        if(ier.ne.0) call jedetr(sdline_crack)
        call wkvect(sdline_crack, 'G V I', nreleq*2, jlis1)
        do ieq = 1, nreleq
            zi(jlis1-1+2*(ieq-1)+1)=liseqt(ieq,1)
            zi(jlis1-1+2*(ieq-1)+2)=liseqt(ieq,2)
        end do
    endif
!
    !ASSERT(.false.)
!
    call jedetr(crack(1:8)//'.CONNECTANT')
    call jedetr(crack(1:8)//'.CONNECTES ')
    call jedetr(cnsln)
    call jedema()
end subroutine
