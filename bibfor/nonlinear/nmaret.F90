subroutine nmaret(nbarvz   , nb_node    , nb_dim     , sdline_crack, nb_node_sele,&
                  list_node, list_node_1, list_node_2)
!
! aslint: disable=W1306
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xrell2.h"
    integer :: nbarvz, nb_node, nb_dim, nb_node_sele
    character(len=14) :: sdline_crack
    character(len=24) :: list_node_1, list_node_2, list_node
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
!
!
! ----------------------------------------------------------------------
!
! INITIALISATION DU PILOTAGE DDL_IMPO OU LONG_ARC - FORMULATION XFEM
!
! RENVOIE L'ENSEMBLE DES ARETES PILOTEES A PARTIR DES NOEUDS ENTRES PAR
! L'UTILISATEUR ET D'UNE LISTE D'ARETES VITALES
! SI L'UTILISATEUR N'A RIEN ENTRE, RENVOIE UN ENSEMBLE D'ARETES
! INDEPENDANTES
!
! ----------------------------------------------------------------------
!
!
! IN  NBARVI : NOMBRE D ARETES VITALES
! IN  NNO    : NOMBRE DE NOEUDS ENTRES PAR L'UTILISATEUR (EVT 0)
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NLISEQ : LISTE DES RELATIONS D EGALITE
! IN  NUMNOD : LISTE DES NUMEROS DES NOEUDS UTILISATEUR
! OUT  NBNO  : NOMBRE D ARETES FINALEMENT PILOTEES
! OUT  GRO1  : LISTE DES NOEUDS EXTREMITES 1 DES ARETES PILOTEES
! OUT  GRO1  : LISTE DES NOEUDS EXTREMITES 2 DES ARETES PILOTEES
!
!
!
!
    integer :: tabnoz(3, nbarvz), tabut(3, nbarvz), nbarvi
    real(kind=8) :: tabcoz(nb_dim, nbarvz), tabcrz(nbarvz)
    integer :: jgro1, jgro2, j, jgro, i, l, nreleq, repere, jlis1
    integer :: effac, nar, narm, iret, vali(2)
    aster_logical :: noeuad
    character(len=19) :: nlise2
    aster_logical :: l_create_group, l_pilo
!
! ----------------------------------------------------------------------
!
    nbarvi=nbarvz
    call jeveuo(sdline_crack, 'L', jlis1)
    call jeexin(list_node, iret)
    if (nb_node .ge. 1 .and. iret .ne. 0) call jeveuo(list_node, 'L', jgro)
    do i = 1, nbarvi
        tabnoz(1,i)=zi(jlis1-1+2*(i-1)+1)
        tabnoz(2,i)=zi(jlis1-1+2*(i-1)+2)
        tabnoz(3,i)=i
        do j = 1, nb_dim
            tabcoz(j,i)=0.d0
        end do
        tabcrz(i)=i
    end do
    
!
    if (nb_node .ge. 1 .and. iret .ne. 0) then
        do i = 1, nb_node
            tabut(1,i)=zi(jgro-1+i)
        end do
        if (nb_node .gt. nbarvi) then
            call utmess('F', 'PILOTAGE_62')
        endif

        do j = 1, nb_node
            noeuad=.false.
            do i = 1, nbarvi
!
                if (tabut(1,j) .eq. tabnoz(1,i)) then
                    do l = 1, nb_node
                        if ((l.ne.j) .and. (tabut(1,l).eq.tabnoz(2,i))) then
                            vali(1)=tabut(1,j)
                            vali(2)=tabut(1,l)
                            call utmess('F', 'PILOTAGE_63', ni=2, vali=vali)
                        endif
                    end do
                    tabut(2,j)=tabnoz(2,i)
                    tabut(3,j)=tabnoz(3,i)
                    noeuad=.true.
                else if (tabut(1,j).eq.tabnoz(2,i)) then
                    do l = 1, nb_node
                        if ((l.ne.j) .and. (tabut(1,l).eq.tabnoz(1,i))) then
                            vali(1)=tabut(1,j)
                            vali(2)=tabut(1,l)
                            call utmess('F', 'PILOTAGE_63', ni=2, vali=vali)
                        endif
                    end do
                    tabut(2,j)=tabnoz(1,i)
                    tabut(3,j)=tabnoz(3,i)
                    noeuad=.true.
                endif
            end do
            if (.not.noeuad) then
                call utmess('A', 'PILOTAGE_61', si=tabut(1, j))
            endif
        end do
        do i = 1, nb_node
            do j = 1, 3
                tabnoz(j,i)=tabut(j,i)
            end do
        end do
        nbarvi=nb_node
    endif

!
    l_create_group = .true.
    l_pilo = .true.
    nlise2 = '&&NMARET.LISEQ'
    call xrell2(tabnoz, nb_dim, nbarvi, tabcoz, tabcrz,&
                l_create_group, nlise2, l_pilo)
    nar=nbarvi
    call jeexin(nlise2, iret)
    if (iret .ne. 0) then
        call jeveuo(nlise2, 'L', jlis1)
        call jelira(nlise2, 'LONMAX', ival=nreleq)
        nreleq = nreleq/2
        ASSERT(nreleq.eq.nar)
        if (nreleq .gt. 0) then
            do i = 1, nreleq
                repere=0
                effac=zi(jlis1-1+2*(i-1)+2)
                do j = 1, nar
                    if (effac .eq. tabnoz(3,j)) then
                        repere=j
                    endif
                end do
                if (repere .gt. 0) then
                    if (repere .lt. nar) then
                        narm = nar-1
                        do l = repere, narm
                            tabnoz(1,l) = tabnoz(1,l+1)
                            tabnoz(2,l) = tabnoz(2,l+1)
                            tabnoz(3,l) = tabnoz(3,l+1)
                        end do
                    endif
                    tabnoz(1,nar)=0
                    tabnoz(2,nar)=0
                    tabnoz(3,nar)=0
                    nar=nar-1
                endif
            end do
        endif
    endif
    nb_node_sele = nar
    call wkvect(list_node_1, 'V V I', nb_node_sele, jgro1)
    call wkvect(list_node_2, 'V V I', nb_node_sele, jgro2)
!
    do i = 1, nb_node_sele
        zi(jgro1-1+i)=tabnoz(1,i)
        zi(jgro2-1+i)=tabnoz(2,i)
    end do
    call jedetr(nlise2)
end subroutine
