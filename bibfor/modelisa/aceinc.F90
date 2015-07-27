subroutine aceinc(noma, nomo, ntyele, nbocc, ivr, &
                  locaco, locagb, locamb, zjdlm, lmax, ier)
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
    use cara_elem_parameter_module
    implicit none
#include "asterf_types.h"
    integer :: ntyele(*), nbocc(*), ivr(*), zjdlm(*), lmax, ier
    aster_logical :: locaco, locagb, locamb
    character(len=8) :: noma, nomo
!
! --------------------------------------------------------------------------------------------------
!
!     AFFE_CARA_ELEM
!     INCREMENTATION DES COMPTEURS D'APPELS A NOCART
!        POUR LES DISCRETS, COQUES, DEFI_ARCS, CABLES
!     VERIFICATION QUE TOUS LES ELEMENTS DU MODELE ONT ETE AFFECTES
!        PAR DES CARACTERISTIQUES.
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/vafcar.h"
#include "asterfort/wkvect.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: ii, ioc, ixma, jj, iid, iif
    integer :: jdgm, jdls, jdme
    integer :: mcl, nbmagr, nbmail
    integer :: ncar, ncara, ng, nm
    integer :: nummai, nutyel
! --------------------------------------------------------------------------------------------------
    integer, parameter :: nbcar = 100
    integer :: iarg
    character(len=6) :: kioc
    character(len=8) :: car(nbcar)
    character(len=24) :: mlgnma, mlggma
    character(len=24) :: modmai, nommai
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    locaco = .false.
    locagb = .false.
    locamb = .false.
!
!   reconstruction des noms jeveux du concept maillage et modele
    modmai = nomo//'.MAILLE'
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
    call jelira(mlgnma, 'NOMMAX', nbmail)
    call jeexin(modmai, ixma)
    if (ixma .ne. 0) call jeveuo(modmai, 'L', jdme)
!
    call wkvect('&&TMPINC', 'V V K24', lmax, jdls)
!
    cmcl1: do mcl = 1, ACE_NB_MCLEF
        if (mcl .eq. ACE_RIGI_MISS_3D) cycle cmcl1
        if (mcl .eq. ACE_MULTIFIBRE)   cycle cmcl1
        do ioc = 1, nbocc(mcl)
            call codent(ioc, 'G', kioc)
            ng = 0
            nm = 0
            if ( mcl.eq.ACE_RIGI_PARASOL .or. &
                 mcl.eq.ACE_MASS_AJOU ) then
                call getvem(noma, 'GROUP_MA_POI1', ACE_MCLEF(mcl), 'GROUP_MA_POI1', ioc,&
                            iarg, lmax, zk24(jdls), ng)
                if (ng .eq. 0) then
                    call getvem(noma, 'GROUP_MA_SEG2', ACE_MCLEF(mcl), 'GROUP_MA_SEG2', ioc,&
                                iarg, lmax, zk24(jdls), ng)
                endif
            else if ( mcl.eq.ACE_MASS_REP ) then
                call getvtx(ACE_MCLEF(mcl), 'GROUP_MA_POI1', iocc=ioc, nbval=lmax, &
                            vect=zk24(jdls), nbret= ng )
            else
                call getvem(noma,'GROUP_MA',ACE_MCLEF(mcl),'GROUP_MA',ioc,iarg,lmax,zk24(jdls),ng)
                call getvem(noma,'MAILLE',  ACE_MCLEF(mcl),'MAILLE',  ioc,iarg,lmax,zk24(jdls),nm)
            endif
            if (mcl.eq.ACE_POUTRE     .or. mcl.eq.ACE_DISCRET .or. mcl.eq.ACE_ORIENTATION .or. &
                mcl.eq.ACE_DISCRET_2D .or. mcl.eq.ACE_RIGI_PARASOL) then
                call getvtx(ACE_MCLEF(mcl), 'CARA', iocc=ioc, nbval=nbcar, vect=car, nbret=ncar)
                if (ncar .gt. 0) ncara = ncar
            endif
!           GROUP_MA = mailles dans la liste des groupes de mailles
            if (ng .gt. 0) then
                if (mcl.eq.ACE_COQUE)    locaco = .true.
                if (mcl.eq.ACE_GRILLE)   locagb = .true.
                if (mcl.eq.ACE_MEMBRANE) locamb = .true.
                do ii = 1, ng
                    call jeveuo(jexnom(mlggma, zk24(jdls+ii-1)), 'L', jdgm)
                    call jelira(jexnom(mlggma, zk24(jdls+ii-1)), 'LONUTI', nbmagr)
                    do jj = 1, nbmagr
                        nummai = zi(jdgm+jj-1)
                        call jenuno(jexnum(mlgnma, nummai), nommai)
                        nutyel = zi(jdme+nummai-1)
                        if (mcl.ne.ACE_ORIENTATION) zjdlm(nummai) = -mcl
                        call vafcar('MAILLE', mcl, nommai, nutyel, ntyele, car, ncara,&
                                    ivr, kioc, ier)
                    enddo
                enddo
            endif
!           MAILLE = mailles de la liste de mailles
            if (nm .gt. 0) then
                if (mcl.eq.ACE_COQUE)    locaco = .true.
                if (mcl.eq.ACE_GRILLE)   locagb = .true.
                if (mcl.eq.ACE_MEMBRANE) locamb = .true.
                do ii = 1, nm
                    nommai = zk24(jdls+ii-1)
                    call jenonu(jexnom(mlgnma, nommai), nummai)
                    nutyel = zi(jdme+nummai-1)
                    if (mcl.ne.ACE_ORIENTATION) zjdlm(nummai) = -mcl
                    call vafcar('MAILLE', mcl, nommai, nutyel, ntyele, car, ncara,&
                                ivr, kioc, ier)
                enddo
            endif
!
        enddo
    enddo cmcl1
!
! --- VERIFICATION QUE TOUS LES ELEMENTS SONT AFFECTES :
!     --------------------------------------------------
    do nummai = 1, nbmail
        call jenuno(jexnum(mlgnma, nummai), nommai)
        if (nbocc(ACE_POUTRE) .ne. 0) then
            iid = 1
            iif = ACE_NB_POUTRE
            do ii = iid, iif
                if (zjdlm(nummai) .eq. ntyele(ii)) then
                    call utmess('A', 'MODELISA_38', sk=nommai)
                endif
            enddo
        endif
        if (nbocc(ACE_DISCRET).ne.0 .or. nbocc(ACE_RIGI_PARASOL).ne.0 .or. &
            nbocc(ACE_MASS_AJOU).ne.0 .or. nbocc(ACE_DISCRET_2D).ne.0) then
            iid = ACE_NB_POUTRE+1
            iif = ACE_NB_POUTRE+ACE_NB_DISCRET
            do ii = iid, iif
                if (zjdlm(nummai) .eq. ntyele(ii)) then
                    call utmess('A', 'MODELISA_39', sk=nommai)
                endif
            enddo
        endif
        if (nbocc(ACE_CABLE).ne.0) then
            iid = ACE_NB_POUTRE+ACE_NB_DISCRET+ACE_NB_COQUE+1
            iif = ACE_NB_POUTRE+ACE_NB_DISCRET+ACE_NB_COQUE+ACE_NB_CABLE
            do ii = iid, iif
                if (zjdlm(nummai) .eq. ntyele(ii)) then
                    call utmess('A', 'MODELISA_40', sk=nommai)
                endif
            enddo
        endif
        if (nbocc(ACE_BARRE).ne.0) then
            iid = ACE_NB_POUTRE+ACE_NB_DISCRET+ACE_NB_COQUE+ACE_NB_CABLE+1
            iif = ACE_NB_POUTRE+ACE_NB_DISCRET+ACE_NB_COQUE+ ACE_NB_CABLE+ACE_NB_BARRE
            do ii = iid, iif
                if (zjdlm(nummai) .eq. ntyele(ii)) then
                    call utmess('A', 'MODELISA_41', sk=nommai)
                endif
            enddo
        endif
        if (nbocc(ACE_RIGI_MISS_3D).ne.0) then
            iid=ACE_NB_POUTRE+ACE_NB_DISCRET+ACE_NB_COQUE+ACE_NB_CABLE+ACE_NB_BARRE+ACE_NB_MASSIF+1
            iif=ACE_NB_TYPE_ELEM
            do ii = iid, iif
                if (zjdlm(nummai) .eq. ntyele(ii)) then
                    call utmess('A', 'MODELISA_42', sk=nommai)
                endif
            enddo
        endif
    enddo
!
    call jedetr('&&TMPINC')
!
    call jedema()
end subroutine
