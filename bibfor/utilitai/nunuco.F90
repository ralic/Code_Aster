subroutine nunuco(numedd, defico, lcont, sdnuco)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
    character(len=24) :: numedd, defico
    aster_logical :: lcont
    character(len=24) :: sdnuco
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DE LA SD POUR REPERAGE DDL DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  DEFICO : NOM DE LA SD DEFINITION DU CONTACT
! IN  LCONT  : IL Y A DU CONTACT
! IN  SDNUCO : NOM DE LA S.D. DDL DE CONTACT
!
!
!
!
    character(len=8) :: nomgd, modele, noma
    character(len=24) :: nolili
    aster_logical :: lxfcm
    integer :: nec, nbnoeu, ncmpmx
    integer :: nlili, neq
    integer :: ico
    integer :: ilagc, ilagf1, ilagf2
    integer :: ino, i, k, inoc, ival, iadg
    integer :: jcmp, ianueq, iaprno
    integer :: ifm, niv
    integer :: jnuco
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- MODELE XFEM ?
!
    lxfcm = .false.
    if (lcont) then
        lxfcm = cfdisl(defico,'FORMUL_XFEM')
    endif
!
! --- MODELE ASSOCIE AU NUME_DDL
!
    call dismoi('NOM_MODELE', numedd, 'NUME_DDL', repk=modele)
!
! --- NOM DU MAILLAGE
!
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
!
! --- NOMBRE DE NOEUDS DU MAILLAGE
!
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoeu)
!
! --- NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR DEPL_R
!
    nomgd = 'DEPL_R'
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmpmx)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmp)
!
! --- LOCALISATION DE LAGS_C DANS LA LISTE DES DDLS ASSOCIES
! --- A LA GRANDEUR DEPL_R
!
    ilagc = indik8(zk8(jcmp),'LAGS_C' ,1,ncmpmx)
    ilagf1 = indik8(zk8(jcmp),'LAGS_F1',1,ncmpmx)
    ilagf2 = indik8(zk8(jcmp),'LAGS_F2',1,ncmpmx)
    ASSERT(ilagc.ne.0)
    ASSERT(ilagf1.ne.0)
    ASSERT(ilagf2.ne.0)
!
! --- RECUPERATION DU NOMBRE D'INCONNUES DU MODELE
!
    call jelira(numedd(1:14)//'.NUME.NUEQ', 'LONUTI', neq)
    call wkvect(sdnuco, 'V V I', neq, jnuco)
!
! --- RECUPERATION DU .PRNO ASSOCIE AU MAILLAGE
!
    call jelira(numedd(1:14)//'.NUME.PRNO', 'NMAXOC', nlili)
    k = 0
    do i = 1, nlili
        call jenuno(jexnum(numedd(1:14)//'.NUME.LILI', i), nolili)
        if (nolili(1:8) .ne. '&MAILLA ') goto 40
        k = i
 40     continue
    end do
    ASSERT(k.ne.0)
!
    call jeveuo(jexnum(numedd(1:14)//'.NUME.PRNO', k), 'L', iaprno)
!
! --- TABLEAU DES NUMEROS D'EQUATIONS
!
    call jeveuo(numedd(1:14)//'.NUME.NUEQ', 'L', ianueq)
!
! --- AFFECTATION DU TABLEAU DES NUMEROS DES INCONNUES
!
    inoc = 0
    if (lxfcm) goto 999
    do ino = 1, nbnoeu
        inoc = inoc + 1
        ival = zi(iaprno+(ino-1)*(nec+2)+1-1)
        iadg = iaprno+(ino-1)*(nec+2)+3-1
        if (exisdg(zi(iadg),ilagc)) then
            ico = 0
            do i = 1, ilagc-1
                if (exisdg(zi(iadg),i)) ico = ico + 1
            end do
            zi(jnuco+ival+ico-1) = 1
        endif
        if (exisdg(zi(iadg),ilagf1)) then
            ico = 0
            do i = 1, ilagf1-1
                if (exisdg(zi(iadg),i)) ico = ico + 1
            end do
            zi(jnuco+ival+ico-1) = 1
        endif
        if (exisdg(zi(iadg),ilagf2)) then
            ico = 0
            do i = 1, ilagf2-1
                if (exisdg(zi(iadg),i)) ico = ico + 1
            end do
            zi(jnuco+ival+ico-1) = 1
        endif
    end do
!
999 continue
!
    call jedema()
end subroutine
