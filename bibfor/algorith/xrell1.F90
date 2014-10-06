subroutine xrell1(tabl_node   , nb_edge, node_sele, nb_node_sele, sdline_crack)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
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
    integer, intent(in) :: nb_edge
    integer, intent(in) :: nb_node_sele
    integer, intent(in) :: tabl_node(3, nb_edge)
    integer, intent(inout) :: node_sele(nb_edge)
    character(len=14), intent(in) :: sdline_crack
!
! --------------------------------------------------------------------------------------------------
!
! XFEM - Contact definition
!
! Create list of linear relations (algorithm 1)
!
! --------------------------------------------------------------------------------------------------
!
! (VOIR BOOK VI 15/07/05)
!    - CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
! --------------------------------------------------------------------------------------------------
!
! In  sdline_crack   : name of datastructure of linear relations for crack
! In  nb_edge        : number of cut edges
! In  tabl_node      : table of nodes for edges (middle et vertex nodes)
! In  node_sele      : selected nodes
! In  nb_node_sele   : number of selected nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, j, in, dimeq, ia, ext, libre, k, eq(100), tabno(nb_edge, 3), ie
    integer :: liseqt(nb_edge, 2), nreleq, jlis1
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    do i = 1, nb_edge
        do j = 1, 3
            tabno(i,j)=tabl_node(j,i)
        end do
    end do
    nreleq = 0
!
! --- CREATION DU TABLEAU TEMPORAIRE DES RELATION D'EGALITE : LISEQT
!
    do i = 1, nb_node_sele
        in = node_sele(i)
        dimeq = 0
        do ia = 1, nb_edge
            do j = 1, 2
!           ON CHERCHE LES ARETES EMANANTES
                if (tabno(ia,j) .eq. in) then
                    ext=tabno(ia,3-j)
!             ON REGARDE SI L'AUTRE EXTREMITE EST LIBRE
                    libre=1
                    do k = 1, nb_node_sele
                        if (ext .eq. node_sele(k)) libre=0
                    end do
                    if (libre .eq. 1) then
                        dimeq=dimeq+1
                        eq(dimeq)=ia
                    endif
                endif
            end do
        end do
        do ie = 1, dimeq
            nreleq=nreleq+1
            liseqt(nreleq,1)=tabno(eq(ie) , 1)
            liseqt(nreleq,2)=tabno(eq(ie) , 2)
        end do
    end do
!
! --- STOCKAGE DE LISEQT
!
    if (nreleq .gt. 0) then
        call wkvect(sdline_crack, 'G V I', nreleq*2, jlis1)
        do ie = 1, nreleq
            zi(jlis1-1+2*(ie-1)+1)=liseqt(ie,1)
            zi(jlis1-1+2*(ie-1)+2)=liseqt(ie,2)
        end do
    endif
!
    call jedema()
end subroutine
