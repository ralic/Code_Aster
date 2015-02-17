subroutine lgphmo(ma, ligrel, pheno, modeli)
    implicit none
#include "jeveux.h"
#include "asterfort/adalig.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: ma
    character(len=*) :: ligrel, pheno, modeli
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!     BUT: CREER LE LIGREL LIGREL SUR TOUTES LES MAILLES DU MAILLAGE MA
!     ON UTILISE LES E.F. DE LA MODELISATION MODELI DU PHENOMENE PHENO
! ----------------------------------------------------------------------
!
!
!
!
    integer :: nbgrel, nbma, nbtm,   ima, tm
    integer :: te,  igr, ico, jphmod, kmod, jlgrf
    integer :: nbel, jnbno
    character(len=19) :: ligr19, phen1
    integer, pointer :: liel(:) => null()
    integer, pointer :: typmail(:) => null()
    integer, pointer :: litm(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
    ligr19=ligrel
    phen1=pheno
!
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
    call jenonu(jexnom('&CATA.'//phen1(1:13)//'.MODL', modeli), kmod)
    ASSERT(kmod.gt.0)
    call jeveuo(jexnum('&CATA.'//phen1, kmod), 'L', jphmod)
!
!
!     -- ON PARCOURT LA CONNECTIVITE POUR DETERMINER LES ENSEMBLES DE
!        DE MAILLES DE MEME TYPE
    call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbtm)
    call jeveuo(ma//'.TYPMAIL', 'L', vi=typmail)
    AS_ALLOCATE(vi=litm, size=nbtm)
    nbel=0
    do ima = 1, nbma
        tm= typmail(ima)
        ASSERT(tm.gt.0)
        te= zi(jphmod-1+tm)
        if (te .gt. 0) then
            nbel=nbel+1
            litm(tm)=litm(tm)+1
        endif
    end do
!
!
!     -- CALCUL DE NBGREL :
    nbgrel=0
    do tm = 1, nbtm
        if (litm(tm) .gt. 0) nbgrel=nbgrel+1
    end do
!
!
!     -- ALLOCATION ET REMPLISSAGE DE L'OBJET .LIEL :
!     -------------------------------------------------------
    call jecrec(ligr19//'.LIEL', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbgrel)
    call jeecra(ligr19//'.LIEL', 'LONT', nbel+nbgrel, ' ')
    call jeveuo(ligr19//'.LIEL', 'E', vi=liel)
!
    igr=0
    ico=0
    do tm = 1, nbtm
        if (litm(tm) .gt. 0) then
            igr=igr+1
            te= zi(jphmod-1+tm)
            ASSERT(te.gt.0)
            nbel=0
            do ima = 1, nbma
                if (typmail(ima) .eq. tm) then
                    nbel=nbel+1
                    ico=ico+1
                    liel(ico)=ima
                endif
            end do
            ASSERT(nbel.gt.0)
            call jecroc(jexnum(ligr19//'.LIEL', igr))
            call jeecra(jexnum(ligr19//'.LIEL', igr), 'LONMAX', nbel+1)
            ico=ico+1
            liel(ico)=te
        endif
    end do
!
!
!     -- OBJET .LGRF :
!     ----------------
    call wkvect(ligr19//'.LGRF', 'V V K8', 2, jlgrf)
    zk8(jlgrf-1+1)=ma
!
!
!     -- OBJET .NBNO :
!     ----------------
    call wkvect(ligr19//'.NBNO', 'V V I', 1, jnbno)
    zi(jnbno-1+1)=0
!
!
!     -- ON "ADAPTE" LA TAILLE DES GRELS DU LIGREL :
    call adalig(ligr19)
!
    AS_DEALLOCATE(vi=litm)
    call jedema()
end subroutine
