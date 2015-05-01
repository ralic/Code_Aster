subroutine xpivit(jcesd, jcesv, jcesl, ifiss, cncte,&
                  ndim, nummae, iface, xpc, ypc,&
                  nvit, group, naret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/xxmmvd.h"
    character(len=24) :: cncte
    integer :: jcesd(10), jcesv(10), jcesl(10)
    integer :: ndim, nummae, iface, ifiss
    real(kind=8) :: xpc, ypc
    integer :: nvit, group, naret
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (CONTACT - GRANDS GLISSEMENTS)
!
! SERT À DEFINIR SI LE POINT D'INTEGRATION EST VITAL OU NON
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC L'I.F.P.
!
! ----------------------------------------------------------------------
!
!  JCES*(2)  : POINTEURS DE LA SD SIMPLE DES INFOS SUR ARETES COUPEES
!  JCES*(4)  : POINTEURS DE LA SD SIMPLE DE CONNECTIVITÉ DES FACETTES
! IN  NDIM  : DIMENSION DU MODELE
! IN NUMMAE : POSITION DE LA MAILLE ESCLAVE
! IN IFACE  : NUMERO LOCAL DE LA FACETTE ESCLAVE
! IN XPC    : COORDONNEE X DU POINT D'INTEGRATION DE CONTACT SUR
!             LA MAILLE ESCLAVE
! IN YPC    : COORDONNEE Y DU POINT D'INTEGRATION DE CONTACT SUR
!             LA MAILLE ESCLAVE
!
! OUT NVIT  : VAUT 1 SI LE POINT D'INTEGRATION EST SUR UNE ARETE
!             VITALE (0 SINON)
! OUT GROUP : NUMERO DE GROUPE SI LE POINT EST SUR UNE ARETE
!             APPARTENANT À UN GROUPE D'ARETES CONNECTÉES (0 SINON)
! OUT NARET : NUMERO D'ARETE DU GROUPE SI LE POINT DE CONTACT EST
!             SUR UNE ARETE APPARTENANT À UN GROUPE (0 SINON)
!
!
!
!
    integer :: zxain, pint, aret, ier, jcnte, ncte, narcon, iad
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    zxain = xxmmvd('ZXAIN')
    pint = 0
    aret = 0
    nvit = 0
    group = 0
    naret = 0
!
! --- RECUPERATION DE QUELQUES DONNEES
!
    call jeexin(cncte, ier)
    if (ier .ne. 0) then
        call jeveuo(cncte, 'L', jcnte)
        call jelira(cncte, 'LONMAX', narcon)
    else
        narcon = 0
    endif
!
! --- ON REGARDE SI LE POINT D'INTEGRATION EST SUR UNE ARETE
!
    if (ndim .eq. 2) then
        if (xpc .eq. -1) then
            pint = 1
        else if (xpc.eq.1) then
            pint = 2
        endif
    else if (ndim.eq.3) then
        if ((xpc.eq.0) .and. (ypc.eq.0)) then
            pint = 1
        else if ((xpc.eq.1).and.(ypc.eq.0)) then
            pint = 2
        else if ((xpc.eq.0).and.(ypc.eq.1)) then
            pint = 3
        endif
    endif
!
! --- SI IL EST SUR UNE ARETE
!
    if (pint .ne. 0) then
!
! --- ON RECUPERE LE NUMERO DU POINT D'INTERSECTION COUPEE CORESPONDANT
!
        call cesexi('S', jcesd(4), jcesl(4), nummae, 1,&
                    ifiss, ndim*( iface-1)+pint, iad)
        ASSERT(iad.gt.0)
        pint = zi(jcesv(4)-1+iad)
!
! --- ON RECUPERE LE NUMERO D'ARETE COUPÉE ET L'INFO VITAL OU PAS DE
! --- CETTE ARETE COUPEE
!
        call cesexi('S', jcesd(2), jcesl(2), nummae, 1,&
                    ifiss, zxain*(pint- 1)+1, iad)
        ASSERT(iad.gt.0)
        aret = nint(zr(jcesv(2)-1+iad))
        call cesexi('S', jcesd(2), jcesl(2), nummae, 1,&
                    ifiss, zxain*(pint- 1)+5, iad)
        ASSERT(iad.gt.0)
        nvit = nint(zr(jcesv(2)-1+iad))
!
        if ((nvit.eq.1) .and. (aret.ne.0)) then
!
! --- ON REGARDE SI LE PT DE CONTACT EST SUR UNE ARETE CONNECTEE
!
            do 10 ncte = 1, narcon/4
                if ((zi(jcnte-1+4*(ncte-1)+3).eq.nummae) .and.&
                    (zi( jcnte-1+4*(ncte-1)+4).eq.aret)) then
!
! --- ON NOTE SON NUMERO DE GROUPE ET SON NUMERO D'ARETE DANS LE GROUPE
!
                    group = zi(jcnte-1+4*(ncte-1)+2)
                    naret = zi(jcnte-1+4*(ncte-1)+1)
                    if (naret .ne. 1) then
                        nvit = 0
                        goto 10
                    endif
                endif
10          continue
        endif
    endif
!
    call jedema()
end subroutine
