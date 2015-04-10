subroutine xcesrd(ces, chsnpg)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/cesexi.h"

    character(len=19) :: ces, chsnpg
!     -----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: patrick.massin at edf.fr
! -----------------------------------------------------------------
!  BUT : reduire un champ elementaire simple (CHAM_ELEM_S), de type ELGA,
!        defini sur la famille "XFEM", de sorte que parmis l'ensemble des
!        points de Gauss possibles, seuls les oints de Gauss des sous-
!        elements portent une valeur non "nulle"  
! -----------------------------------------------------------------
!
! CES INOUT     K19 :  NOM DU CHAMP A CHANGER
!                     TYPE AUTORISE POUR CHIN : ELGA
!
! CHSNPG INT    K19 : NOM DU CHAM_ELEM_S CONTENANT LE NOMBRE DE POINTS
!                     DE GAUSS UTILISES DANS CHAQUE ELEMENT CONNAISSANT
!                     LA FAMILLE "XFEM"

    integer :: nbma, nbpt, nbsp, nbcmp, npg
    integer :: ima, ipt, isp, icmp, iadnpg, iad
    integer :: jcesdnpg, jceslnpg, jcesd, jcesl, jcesv
    character(len=3) :: tsca
    character(len=8) :: nomgd
    aster_logical :: lreal, lfonc
    integer, pointer :: cesvnpg(:) => null()
!
    lreal=.false.
    lfonc=.false.
!
!   recuperation du nom de grandeur et du type de scalaire associe
    call dismoi('NOM_GD', ces, 'CHAMP', repk=nomgd)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
!   recherche du type de valeur a matrre a 0 :
!      - valeur reelle
!      - objet fonction
    if (nomgd.eq.'NEUT_F') then
       ASSERT(tsca.eq.'K8')
       lfonc=.true.
    else if (tsca.eq.'R') then
       lreal=.true.
    else
       ASSERT(.false.)
    endif
!
!   ouverture du champ simple contenant le nombre de points de Gauss
!   reellement utilise pour chaque element (0 si la famille XFEM
!   n'est pas definie)
    call jeveuo(chsnpg//'.CESD', 'L', jcesdnpg)
    call jeveuo(chsnpg//'.CESL', 'L', jceslnpg)
    call jeveuo(chsnpg//'.CESV', 'L', vi=cesvnpg)
!
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESL', 'L', jcesl)
    call jeveuo(ces//'.CESV', 'E', jcesv)

    nbma = zi(jcesdnpg-1+1)
    do ima=1, nbma
        call cesexi('C', jcesdnpg, jceslnpg, ima, 1, 1, 1, iadnpg)
        ASSERT(abs(iadnpg).gt.0)
        if (iadnpg.le.0) cycle
!
!       recuperation du nombre de point de Gauss de l'element 
        npg=cesvnpg(iadnpg)
!
        nbpt = zi(jcesd-1+5+4*(ima-1)+1)
        nbsp = zi(jcesd-1+5+4*(ima-1)+2)
        nbcmp = zi(jcesd-1+5+4*(ima-1)+3)
!
!       on met a 0 les points dont on n'a pas besoin
        do ipt=npg+1, nbpt
            do isp=1, nbsp
                do icmp=1, nbcmp 
                    call cesexi('C', jcesd, jcesl, ima, ipt, isp, icmp, iad)
                    if (iad.le.0) cycle
                    if (lreal) zr(jcesv-1+iad)  = 0.d0
                    if (lfonc) zk8(jcesv-1+iad) = '&FOZERO'
                enddo
            enddo
        enddo
    enddo
end subroutine
