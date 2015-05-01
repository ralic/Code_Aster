subroutine verif_bord(modele,ligrel)
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/liglma.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/assert.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/utmess.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexatr.h"
!
    character(len=*), intent(in) :: modele
    character(len=*), intent(in) :: ligrel
!
!-----------------------------------------------------------------------
!   But :
!     Emettre des alarmes si le ligrel ne contient pas toutes les mailles
!     de bord necessaires.
!
!   Entrees:
!     modele     :  sd_modele
!     ligrel     :  sous-ligrel du modele
!
!
!    Algorithme :
!      On parcourt toutes les mailles du modele : ima
!         Si ima n'appartient pas au ligrel
!            Si tous les noeuds de ima sont des noeuds du ligrel => Alarme
!
!-----------------------------------------------------------------------
    character(len=8) :: modele_, noma
    character(len=19) :: ligrel_, ligrmo
    character(len=24) :: valk(4)
    integer :: nbmamo,nbmalg,numa,kma,nbmat
    integer :: iconx1,iconx2,nno,nuno,kno,nbnot

    character(len=24) :: linumamo = '&&VERIF_BORD.NUMAMO'
    character(len=24) :: linutemo = '&&VERIF_BORD.NUTEMO'
    character(len=24) :: linumalg = '&&VERIF_BORD.NUMALG'
    character(len=24) :: linutelg = '&&VERIF_BORD.NUTELG'

    integer, pointer :: numamo(:) => null()
    integer, pointer :: numalg(:) => null()
    integer, pointer :: eximalg(:) => null()
    integer, pointer :: exinolg(:) => null()

#define nbno(imail) zi(iconx2+imail) - zi(iconx2+imail-1)
#define connex(imail,j) zi(iconx1-1+zi(iconx2+imail-1)+j-1)


!-----------------------------------------------------------------------
!
    call jemarq()
    modele_=modele
    ligrmo=modele_//'.MODELE'
    ligrel_=ligrel

    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmat)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnot)
    call jeveuo(noma//'.CONNEX', 'L', iconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', iconx2)

    call liglma(ligrmo, nbmamo, linumamo, linutemo)
    call liglma(ligrel_, nbmalg, linumalg, linutelg)
    call jeveuo(linumamo,'L',vi=numamo)
    call jeveuo(linumalg,'L',vi=numalg)


!   -- 1. Calcul de eximalg et exinolg :
!      eximalg(numa) = 1 : la maille numa existe dans ligrel
!      exinolg(nuno) = 1 : le noeud numo existe dans ligrel
!   ----------------------------------------------------------
    AS_ALLOCATE(vi=eximalg,size=nbmat)
    AS_ALLOCATE(vi=exinolg,size=nbnot)
    eximalg=0
    exinolg=0
    do kma=1,nbmalg
        numa=numalg(kma)
        eximalg(numa)=1
        nno=nbno(numa)
        do kno=1,nno
           nuno=connex(numa,kno)
           ASSERT(nuno.gt.0 .and. nuno.le.nbnot)
           exinolg(nuno)=1
        enddo
    enddo

!   -- 2. boucle sur les mailles de modele :
!   ----------------------------------------
B1: do kma=1,nbmamo
        numa=numamo(kma)
        if (eximalg(numa).eq.1) cycle B1

        nno=nbno(numa)
        do kno=1,nno
           nuno=connex(numa,kno)
           if (exinolg(nuno).eq.0) cycle B1
        enddo
        valk(1)=modele
        call jenuno(jexnum(noma//'.NOMMAI', numa), valk(2))
        call utmess('A','CALCULEL4_74',nk=2,valk=valk)
    enddo  B1


!   -- menage :
!   -----------
    call jedetr(linumamo)
    call jedetr(linutemo)
    call jedetr(linumalg)
    call jedetr(linutelg)
    AS_DEALLOCATE(vi=eximalg)
    AS_DEALLOCATE(vi=exinolg)

    call jedema()
end subroutine
