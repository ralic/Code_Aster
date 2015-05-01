subroutine pteequ(prof_chno    , base, neq, igds, nb_cmp_field,&
                  field_to_cata)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=19), intent(in) :: prof_chno
    integer, intent(in) :: neq
    integer, intent(in) :: igds
    integer, intent(in) :: nb_cmp_field
    integer, pointer, intent(in) :: field_to_cata(:)
    character(len=1), intent(in) :: base
!
! --------------------------------------------------------------------------------------------------
!
! Create DEEQ object for field
!
! --------------------------------------------------------------------------------------------------
!
! In  base           : JEVEUX base to create PROF_CHNO object
! In  prof_chno      : name of PROF_CHNO object
! In  neq            : number of equations
! In  igds           : index of GRANDEUR used to numbering
! In  nb_cmp_field   : number of components in field
! In  field_to_cata  : pointer to converter from local components (field) to global (catalog)
!
! Object   : PROF_CHNO.DEEQ 
! Dimension: vector of size (2*neq)
! Contains : for ieq = 1,neq
!
!   DEEQ((ieq-1)*2+1)
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST PHYS.:
!           +NUMERO DU NOEUD
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN LAGRANGE DE BLOCAGE :
!           +NUMERO DU NOEUD PHYS. BLOQUE
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN LAGRANGE DE LIAISON :
!           0
!   DEEQ((ieq-1)*2+2)
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST PHYS.:
!           + NUM. DANS L'ORDRE DU CATAL. DES GRAND. DE LA CMP CORRESPONDANT A L'EQUATION IDDL.
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN LAGRANGE DE BLOCAGE :
!           - NUM. DANS L'ORDRE DU CATAL. DES GRAND. DE LA CMP CORRESPONDANT AU BLOCAGE.
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN LAGRANGE DE LIAISON :
!           0
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cmp_fieldmx, nec, nb_ligr, l, jprno, i_cmp_field
    integer :: nb_node, i_node, iddl, iadg, i_cmp_cata, i_equa
    character(len=24) :: prno, nueq, deeq
    integer, pointer :: p_nueq(:) => null()
    integer, pointer :: p_deeq(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    prno      = prof_chno(1:19)//'.PRNO'
    nueq      = prof_chno(1:19)//'.NUEQ'
    deeq      = prof_chno(1:19)//'.DEEQ'
!
! - Information about GRANDEUR
!
    call jelira(jexnum('&CATA.GD.NOMCMP', igds), 'LONMAX', nb_cmp_fieldmx)
    nec     = nbec(igds)
    ASSERT(nb_cmp_fieldmx .ne. 0)
    ASSERT(nec .ne. 0)
!
! - Create .DEEQ object
!
    call jedetr(deeq)
    call wkvect(deeq, base//' V I', 2*neq, vi = p_deeq)
!
! - Access to PRNO object
!
    call jelira(prno, 'NMAXOC', nb_ligr)
! - It's field: no "tardif" element/node, only mesh
    ASSERT(nb_ligr.eq.1)
    call jelira(jexnum(prno, 1), 'LONMAX', l)
    ASSERT(l.gt.0)
    call jeveuo(jexnum(prno, 1), 'L', jprno)
!
! - Access to NUEQ object
!
    call jeveuo(nueq, 'L', vi = p_nueq)
!
    nb_node = l/(nec+2)
    do i_node = 1, nb_node
        iddl = zi(jprno-1+ (i_node-1)* (nec+2)+1) - 1
        iadg = jprno - 1 + (i_node-1)* (nec+2) + 3
        do i_cmp_field = 1, nb_cmp_field
            i_cmp_cata = field_to_cata(i_cmp_field)
            if (exisdg(zi(iadg),i_cmp_cata)) then
                iddl = iddl + 1
                i_equa = p_nueq(iddl)
                p_deeq(2*(i_equa-1)+1) = i_node
                p_deeq(2*(i_equa-1)+2) = i_cmp_cata
            endif
        end do
    end do
!
    call jedema()
end subroutine
