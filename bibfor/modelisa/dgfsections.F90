subroutine dgfsections(nboccsec, iinbgf, tousgroupesnom, tousgroupesnbf, maxmailgrp, &
                       ulnbnoeuds, ulnbmailles, nbfibres1)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!                O P E R A T E U R    DEFI_GEOM_FIBRE
!
!   Pré-traitement du mot clef SECTION
!
! --------------------------------------------------------------------------------------------------
!
! person_in_charge: jean-luc.flejou at edf.fr
!
    implicit none
!
    integer :: nboccsec, iinbgf, maxmailgrp, ulnbnoeuds, ulnbmailles, nbfibres1
    integer           :: tousgroupesnbf(*)
    character(len=24) :: tousgroupesnom(*)
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/codent.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer           :: ioc, nbv, jdtm, jmaill, nbmagr, jdo, nbmaills, nbnoeuds, nummai, nutyma
    integer           :: nttri3, ntqua4, ntseg2, ntpoi1
    character(len=7)  :: k7bid
    character(len=8)  :: nomas,ktyma
    character(len=24) :: mlgtms
!
    character(len=24) :: valk(3)
!
    character(len=16) :: limcls(3), ltymcl(3)
    data limcls/'MAILLE_SECT','GROUP_MA_SECT','TOUT_SECT'/
    data ltymcl/'MAILLE','GROUP_MA','TOUT'/
!
! --------------------------------------------------------------------------------------------------
!
!   Récupération des types mailles TRI3, QUAD4, SEG2, POI1
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3'), nttri3)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD4'), ntqua4)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'),  ntseg2)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'),  ntpoi1)
!
    do ioc = 1, nboccsec
        iinbgf = iinbgf + 1
        call getvtx('SECTION', 'GROUP_FIBRE', iocc=ioc, scal=tousgroupesnom(iinbgf))
!
        call getvid('SECTION', 'MAILLAGE_SECT', iocc=ioc, scal=nomas, nbret=nbv)
!       Type de maille dans le maillage associé
        mlgtms = nomas//'.TYPMAIL'
        call jeveuo(mlgtms, 'L', jdtm)
!       nombre de fibres = nombre de mailles concernées
        call reliem(' ', nomas, 'NU_MAILLE', 'SECTION', ioc,&
                    3, limcls, ltymcl, '&&OP0119.GRPMAILL', nbmaills)
        call reliem(' ', nomas, 'NU_NOEUD',  'SECTION', ioc,&
                    3, limcls, ltymcl, '&&OP0119.GRPNOEUD', nbnoeuds)
        ulnbnoeuds = ulnbnoeuds + nbnoeuds
        call jeveuo('&&OP0119.GRPMAILL', 'L', jmaill)
!       Les mailles : TRIA3 ou QUA4
!           - les SEG2 sont exclus, ils peuvent servir à la construction
!           - arrêt dans les autres cas
        nbmagr = 0
        do jdo = 1, nbmaills
            nummai = zi(jmaill+jdo-1)
            nutyma = zi(jdtm+nummai-1)
            if (nutyma.eq.ntseg2) cycle
            if ((nutyma.eq.nttri3).or.(nutyma.eq.ntqua4)) then
                nbmagr = nbmagr + 1
                ulnbmailles = ulnbmailles +1
                nbfibres1   = nbfibres1 + 1
                tousgroupesnbf(iinbgf) = tousgroupesnbf(iinbgf) + 1
            else
                call codent(nummai, 'G', k7bid)
                call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), ktyma)
                valk(1)=nomas
                valk(2)=k7bid
                valk(3)=ktyma
                call utmess('F', 'MODELISA6_27', nk=3, valk=valk)
            endif
        enddo
        maxmailgrp = max(maxmailgrp,nbmagr)
    enddo

end