subroutine nugrco(nu, base)
    implicit none
#include "aster_types.h"
#include "jeveux.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
!  NUME_DDL - CREATION DU GRAPH DE COMMUNICATION
!  --                     --       --
! ----------------------------------------------------------------------
!
!   ON CREE LE GRAPH DE COMMUNICATION QUI PERMETTRA DANS LES
!    ROUTINES PETSC DE SAVOIR QUEL PROCESSEUR DOIT COMMUNIQUER
!    AVEC QUEL AUTRE
!
! IN  :
!   NU      K14  NOM DU NUME_DDL
!   BASE    K2   BASE(1:1) : BASE POUR CREER LE NUME_DDL
!                    (SAUF LE PROF_CHNO)
!                BASE(2:2) : BASE POUR CREER LE PROF_CHNO
!
    integer :: rang, nbproc, jpddl, jcomm1, iddl, jnequl, neql, jgraco
    integer :: iproc, nbedge, iaux, jmasqu, jtmp, nmatch, iproc1
    integer :: iproc2, posit, jordjo, num, jnequg, neqg, jnugl, nulodd
    integer :: jpospr, nbddlj, jjoint, curpos, numpro, jjoin2, jnulg
    integer :: iddlg, iddll
!
    integer(kind=4) :: ibid4
!
    real(kind=8) :: rbid
    parameter    (rbid=0.d0)
!
    character(len=4) :: chnbjo
    character(len=24) :: nojoin, nogrco
    mpi_int :: mrank, msize
    parameter    (nogrco='&&NUGRCO.GRAPH_COMM')
!
    call jemarq()
!
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
!
    call jeveuo(nu//'.NUML.NUGL', 'L', jnugl)
    call jeveuo(nu//'.NUML.NULG', 'L', jnulg)
    call jeveuo(nu//'.NUML.PDDL', 'L', jpddl)
    call jeveuo(nu//'.NUML.NEQU', 'L', jnequl)
    call jeveuo(nu//'.NUME.NEQU', 'L', jnequg)
    neql=zi(jnequl)
    neqg=zi(jnequg)
    call wkvect('&&NUGRCO.COMM1', 'V V I', nbproc, jcomm1)
!
!---- DETERMINATION DE QUI COMMUNIQUE AVEC QUI
    do 10 iddl = 0, neql-1
        numpro=zi(jpddl+iddl)
        ASSERT(numpro.lt.nbproc)
        zi(jcomm1+numpro)=zi(jcomm1+numpro)+1
10  end do
    zi(jcomm1+rang)=0
!
    call wkvect(nogrco, 'V V I', nbproc*nbproc, jgraco)
    do 20 iproc = 0, nbproc-1
        if (zi(jcomm1+iproc) .ne. 0) then
            zi(jgraco+iproc+rang*nbproc)=1
            zi(jgraco+rang+iproc*nbproc)=1
        endif
20  end do
    call mpicm2('MPI_SUM', nogrco)
!
!---- RECHERCHE DES COUPLAGES DANS LE GRAPH
    nbedge=0
    do 50, iaux = 1,nbproc*nbproc
    if (zi(jgraco+iaux-1) .eq. 1) nbedge=nbedge+1
    50 end do
    nbedge=nbedge/2
!
!---- RECHERCHE DES COUPLAGES MAXIMAUX
    call wkvect('&&NUGRCO.MASQUE', 'V V I', nbproc*nbproc, jmasqu)
    call wkvect('&&NUGRCO.TMP', 'V V I', nbproc, jtmp)
    nmatch=1
60  continue
    do 30, iproc1 = 0,nbproc-1
    do 40, iproc2 = 0,nbproc-1
    posit=iproc1*nbproc+iproc2
    if (zi(jgraco+posit) .eq. 1 .and. zi(jtmp+iproc1) .eq. 0 .and. zi(jtmp+iproc2) .eq. 0) then
        zi(jgraco+posit)=0
        zi(jmasqu+posit)=nmatch
        posit=iproc2*nbproc+iproc1
        zi(jgraco+posit)=0
        zi(jmasqu+posit)=nmatch
        nbedge=nbedge-1
        zi(jtmp+iproc1)=1
        zi(jtmp+iproc2)=1
    endif
40  continue
    30 end do
    nmatch=nmatch+1
    do 70, iaux = 0,nbproc-1
    zi(jtmp+iaux)=0
    70 end do
    if (nbedge .gt. 0) goto 60
    call jedetr('&&NUGRCO.TMP')
!
!---- CREATION DU GRAPH
    nmatch=nmatch-1
    call wkvect(nu//'.NUML.JOIN', base(1:1)//' V I', nmatch, jordjo)
    call wkvect('&&NUGRCO.POSPROC', 'V V I', 2*nbproc, jpospr)
    do 80, iaux = 0,nmatch-1
    zi(jordjo+iaux)=-1
    80 end do
    do 90, iaux = 0,nbproc-1
    num=zi(jmasqu+rang*nbproc+iaux)
    ASSERT(num.le.nmatch)
    if (num .ne. 0) then
        zi(jordjo+num-1)=iaux
!
        call codent(num, 'G', chnbjo)
        nojoin=nu//'.NUML.'//chnbjo
        nbddlj=zi(jcomm1+iaux)
        if (nbddlj .ne. 0) then
            call wkvect(nojoin, base(1:1)//' V I', nbddlj, jjoint)
            zi(jpospr+2*iaux)=jjoint
            zi(jpospr+2*iaux+1)=0
        else
            zi(jpospr+2*iaux)=-1
            zi(jpospr+2*iaux+1)=-1
        endif
    else
        zi(jpospr+2*iaux)=-1
        zi(jpospr+2*iaux+1)=-1
    endif
    90 end do
!
    do 100, iddl=0,neqg-1
    nulodd=zi(jnugl+iddl)
    if (nulodd .ne. 0) then
        numpro=zi(jpddl+nulodd-1)
        if (numpro .ne. rang) then
            jjoint=zi(jpospr+2*numpro)
            ASSERT(jjoint.ne.-1)
!
            curpos=zi(jpospr+2*numpro+1)
            zi(jjoint+curpos)=nulodd
            zi(jpospr+2*numpro+1)=zi(jpospr+2*numpro+1)+1
        endif
    endif
    100 end do
!
    do 110, iaux=0,nmatch-1
    numpro=zi(jordjo+iaux)
    if (numpro .eq. -1) goto 110
!
    if (rang .gt. numpro) then
        nbddlj=zi(jpospr+2*numpro+1)
        call mpippv('MPI_SEND', 'I', 1, nbddlj, ibid4,&
                    rbid, numpro, iaux)
!
        jjoint=zi(jpospr+2*numpro)
        call wkvect('&&NUGRCO.TMP', 'V V I', nbddlj, jjoin2)
        do 120, iddl=0,nbddlj-1
        iddlg=zi(jnulg+zi(jjoint+iddl)-1)
        ASSERT(iddlg.ne.0)
        zi(jjoin2+iddl)=iddlg
120      continue
!
        call mpippv('MPI_SEND', 'I', nbddlj, zi(jjoin2), ibid4,&
                    rbid, numpro, iaux)
        call jedetr('&&NUGRCO.TMP')
    else if (rang.lt.numpro) then
        call mpippv('MPI_RECV', 'I', 1, nbddlj, ibid4,&
                    rbid, numpro, iaux)
        call wkvect('&&NUGRCO.TMP', 'V V I', nbddlj, jjoin2)
!
        num=iaux+1
        call codent(num, 'G', chnbjo)
        nojoin=nu//'.NUML.'//chnbjo
        call wkvect(nojoin, base(1:1)//' V I', nbddlj, jjoint)
!
        call mpippv('MPI_RECV', 'I', nbddlj, zi(jjoin2), ibid4,&
                    rbid, numpro, iaux)
        do 130, iddl=0,nbddlj-1
        iddll=zi(jnugl+zi(jjoin2+iddl)-1)
        ASSERT(iddll.ne.0)
        zi(jjoint+iddl)=iddll
130      continue
        call jedetr('&&NUGRCO.TMP')
    else
        ASSERT(.false.)
    endif
    110 end do
!
    call jedetr('&&NUGRCO.COMM1')
    call jedetr(nogrco)
    call jedetr('&&NUGRCO.MASQUE')
    call jedetr('&&NUGRCO.POSPROC')
!
    call jedema()
!
end subroutine
