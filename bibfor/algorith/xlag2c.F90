subroutine xlag2c(model, sdline_crack, jnbpt, mesh)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cncinv.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=8) :: model, mesh
    character(len=14) :: sdline_crack
    integer :: jnbpt
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION)
!
!    DETERMINATION DES NUMÉROS DE LAGRANGES ASSOCIÉS AUX NOEUDS
!    POUR LES RELATIONS D'ÉGALITÉES DANS LE CAS MULTI-HEAVISIDE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO  : NOMBRE MAXIMUM D'ARETES COUPEES PAR LA FISSURE
! IN NLISEQ : LISTE REL. LIN.
! IN JNBPT  : POINTEUR DU COMPTAGE DES FISSURE
! IN MESH   : NOM DU MAILLAGE
!
!
!
!
    integer :: ier, jliseq, neq, jlisla, i, nuno, ima, ino, ifiss, nbnoma
    integer :: jcesd,  jcesl, iad, jmasup, nmasup, j, jconx2, k
    character(len=19) :: heavno, cnxinv
    integer, pointer :: cesv(:) => null()
    integer, pointer :: connex(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DE NLISEQ
!
    call jeexin(sdline_crack, ier)
    if (ier .eq. 0) goto 999
    call jeveuo(sdline_crack, 'L', jliseq)
    call jelira(sdline_crack, 'LONMAX', neq)
!
! --- RECUPÉRATION DE HEAVNO
!
    heavno = '&&XLAG2S.HEAVNO'
    call celces(model//'.HEAVNO', 'V', heavno)
    call jeveuo(heavno//'.CESD', 'L', jcesd)
    call jeveuo(heavno//'.CESV', 'L', vi=cesv)
    call jeveuo(heavno//'.CESL', 'L', jcesl)
!
!     CONNECTIVITE INVERSEE
!
    cnxinv = '&&XLAG2S.CNCINV'
    call cncinv(mesh, [0], 0, 'V', cnxinv)
!
! --- CREATION DE LA SD FISS.LISEQ_LAGR
!
    call wkvect(sdline_crack(1:14)//'_LAGR', 'G V I', neq, jlisla)
!
    do i = 1, neq
        nuno = zi(jliseq-1+i)
        zi(jlisla-1+i) = 1
!       RECUPERATION DES MAILLES CONTENANT LE NOEUD
        call jeveuo(mesh//'.CONNEX', 'L', vi=connex)
        call jeveuo(jexatr(mesh//'.CONNEX', 'LONCUM'), 'L', jconx2)
        call jelira(jexnum(cnxinv, nuno), 'LONMAX', nmasup)
        call jeveuo(jexnum(cnxinv, nuno), 'L', jmasup)
!       ON VERIFIE SI LE NOEUD N'EST PAS ORPHELIN
        if (zi(jmasup) .eq. 0) goto 200
!
        do j = 1, nmasup
!       BOUCLE SUR LES MAILLES CONTENANT LE NOEUD
           ima = zi(jmasup-1+j)
           nbnoma = zi(jconx2+ima) - zi(jconx2+ima-1)
           do k = 1, nbnoma
!       BOUCLE SUR LES NOEUDS DE LA MAILLE
              ino = connex(zi(jconx2+ima-1)+k-1)
              if (ino.eq.nuno) then
                 ifiss = zi(jnbpt-1+ima)
                 call cesexi('C', jcesd, jcesl, ima, k,&
                             ifiss, 1, iad)
                 if (iad .gt. 0) then
                    zi(jlisla-1+i) = cesv(iad)
                 endif
              endif
           end do
        end do
 200    continue
    end do
!
    call jedetr(cnxinv)
    call detrsd('CHAM_ELEM_S', heavno)
999 continue
!
    call jedema()
end subroutine
