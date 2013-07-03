subroutine nurenu(nu, base)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mpicm0.h"
#include "asterfort/mpicm2.h"
#include "asterfort/mpippv.h"
#include "asterfort/wkvect.h"
    character(len=14) :: nu
    character(len=2) :: base
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
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  NUME_DDL - RENUMEROTATION POUR MATR_DISTR AVEC PETSC
!  --         ----
! ----------------------------------------------------------------------
!
!   LE BUT DE CETTE ROUTINE EST DE CREER L'OBJET .NUML.NLGP QUI
!    FOURNIT UNE NUMEROTATION GLOBALE CONTIGUE DU PDV PETSC
!
! IN  :
!   NU      K14  NOM DU NUME_DDL
!   BASE    K2   BASE(1:1) : BASE POUR CREER LE NUME_DDL
!                    (SAUF LE PROF_CHNO)
!                BASE(2:2) : BASE POUR CREER LE PROF_CHNO
!
    integer :: rang, nbproc, jpddl, jnequl, neql, iddl, nbrddl, jnbddl
    integer :: iproc, nbddpr, jnequg, neqg, jnulg, decals, decald, iaux
    integer :: jordjo, njoint, numpro, nbddlj, jjoint, jnewnu, numddl
    integer :: num
!
    integer(kind=4) :: ibid4
!
    real(kind=8) :: rbid
    parameter    (rbid=0.d0)
!
    character(len=4) :: chnbjo
    character(len=24) :: kbid, nonbdd, nojoin
    parameter    (nonbdd='&&NUPODD.NBDDL')
!
    call jemarq()
!
    call mpicm0(rang, nbproc)
!
    call jeveuo(nu//'.NUML.PDDL', 'L', jpddl)
    call jeveuo(nu//'.NUML.NEQU', 'L', jnequl)
    call jeveuo(nu//'.NUME.NEQU', 'L', jnequg)
    neql=zi(jnequl)
    neqg=zi(jnequg)
!
    nbrddl=0
    do 10 iddl = 0, neql-1
        if (zi(jpddl+iddl) .eq. rang) nbrddl=nbrddl+1
10  end do
!
    call wkvect(nonbdd, 'V V I', nbproc, jnbddl)
    zi(jnbddl+rang)=nbrddl
    call mpicm2('MPI_SUM', nonbdd)
    nbddpr=zi(jnbddl)
    do 20 iproc = 1, nbproc-1
        zi(jnbddl+iproc)=zi(jnbddl+iproc)+nbddpr
        nbddpr=zi(jnbddl+iproc)
20  end do
    call assert(neqg.eq.nbddpr)
!
    call wkvect(nu//'.NUML.NLGP', base(1:1)//' V I', neql, jnulg)
    decals=0
    if (rang .ne. 0) decals=zi(jnbddl+rang-1)
    call jedetr(nonbdd)
!
    decald=1
    do 30 iddl = 0, neql-1
        if (zi(jpddl+iddl) .eq. rang) then
            zi(jnulg+iddl)=decals+decald
            decald=decald+1
        endif
30  end do
!
    call jeveuo(nu//'.NUML.JOIN', 'L', jordjo)
    call jelira(nu//'.NUML.JOIN', 'LONMAX', njoint, kbid)
!
    do 40, iaux=0,njoint-1
    numpro=zi(jordjo+iaux)
    if (numpro .eq. -1) goto 40
!
    num=iaux+1
    call codent(num, 'G', chnbjo)
    nojoin=nu//'.NUML.'//chnbjo
    call jeveuo(nojoin, 'L', jjoint)
    call jelira(nojoin, 'LONMAX', nbddlj, kbid)
    call wkvect('&&NURENU.TMP', 'V V I', nbddlj, jnewnu)
    if (rang .lt. numpro) then
!     !!! VERIFIER QU'ON EST OK SUR LES NUM GLOBAUX
        do 50, iddl=0,nbddlj-1
        numddl=zi(jjoint+iddl)
        zi(jnewnu+iddl)=zi(jnulg+numddl-1)
50      continue
        call mpippv('MPI_SEND', 'I', nbddlj, zi(jnewnu), ibid4,&
                    rbid, numpro, iaux)
    else if (rang.gt.numpro) then
!     !!! VERIFIER QU'ON EST OK SUR LES NUM GLOBAUX
        call mpippv('MPI_RECV', 'I', nbddlj, zi(jnewnu), ibid4,&
                    rbid, numpro, iaux)
        do 60, iddl=0,nbddlj-1
        numddl=zi(jjoint+iddl)
        zi(jnulg+numddl-1)=zi(jnewnu+iddl)
60      continue
    else
        call assert(.false.)
    endif
    call jedetr('&&NURENU.TMP')
    40 end do
!
    call jedema()
!
end subroutine
