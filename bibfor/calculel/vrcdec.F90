subroutine vrcdec()
    implicit none
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
!-----------------------------------------------------------------------
! BUT: CALCULER LE DECALAGE DES DIFFERENTES FAMILLES DE PG UTILISEES
!      DANS LA FAMILLE "LISTE" MATER.
!      CECI PERMET DE GAGNER DU TEMPS DANS RCVARC
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/indk32.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
    integer :: nbcvrc, jvcnom
    common /caii14/nbcvrc,jvcnom
!
    integer :: nfpgmx
    parameter (nfpgmx=10)
    integer :: nfpg, jfpgl, decala(nfpgmx), km, kp, kr, iredec
    common /caii17/nfpg,jfpgl,decala,km,kp,kr,iredec
!
    character(len=16) :: option, nomte, nomtm, pheno, modeli
    common /cakk01/option,nomte,nomtm,pheno,modeli
!
    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg
!
    integer :: jtmfpg, kfpgl, kpgmat, nbpg
    integer :: nuflpg, nufgpg, k
    character(len=8) :: fapg, kbid, elrefe
    character(len=32) :: noflpg
! ---------------------------------------------------------------
!
!
    iredec=0
    call jenonu(jexnom('&CATA.TE.NOFPG_LISTE', nomte//'MATER'), kfpgl)
    if (kfpgl .eq. 0) then
        nfpg=0
        jfpgl=0
        goto 9999
    endif
!
    call jeveuo('&CATA.TM.TMFPG', 'L', jtmfpg)
    call jeveuo(jexnum('&CATA.TE.FPG_LISTE', kfpgl), 'L', jfpgl)
    call jelira(jexnum('&CATA.TE.FPG_LISTE', kfpgl), 'LONMAX', nfpg, kbid)
    nfpg=nfpg-1
    ASSERT(nfpg.le.nfpgmx)
    kpgmat=0
    elrefe= zk8(jfpgl-1+nfpg+1)
    do 1,k=1,nfpg
    decala(k)=kpgmat
    fapg=zk8(jfpgl-1+k)
    noflpg = nomte//elrefe//fapg
    nuflpg = indk32(zk32(jpnlfp),noflpg,1,nblfpg)
    nufgpg = zi(jnolfp-1+nuflpg)
    ASSERT(nufgpg.gt.0)
    nbpg=zi(jtmfpg-1+nufgpg)
    kpgmat=kpgmat+nbpg
    1 end do
!
!     -- REMISE A ZERO DE KM,KP,KR POUR QUE RCVARC N'UTILISE PAS LE
!        RESULTAT D'UN TECACH INAPPROPRIE
    km=0
    kp=0
    kr=0
!
!
9999  continue
end subroutine
