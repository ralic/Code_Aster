subroutine xlag2c(model, sdline_crack, jnbpt)
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
!
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=8) :: model
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
!
!
!
!
    integer :: ier, jliseq, neq, jlisla, i, nuno, ima, ino, ifiss
    integer :: jcnsl,  jcesd,  jcesl, iad
    character(len=19) :: noxfem, heavno
    integer, pointer :: cesv(:) => null()
    integer, pointer :: cnsv(:) => null()
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
! --- RECUPÉRATION DE NOXFEM
!
    noxfem = '&&XLAG2S.NOXFEM'
    call cnocns(model//'.NOXFEM', 'V', noxfem)
    call jeveuo(noxfem//'.CNSL', 'L', jcnsl)
    call jeveuo(noxfem//'.CNSV', 'L', vi=cnsv)
!
! --- RECUPÉRATION DE HEAVNO
!
    heavno = '&&XLAG2S.HEAVNO'
    call celces(model//'.HEAVNO', 'V', heavno)
    call jeveuo(heavno//'.CESD', 'L', jcesd)
    call jeveuo(heavno//'.CESV', 'L', vi=cesv)
    call jeveuo(heavno//'.CESL', 'L', jcesl)
!
! --- CREATION DE LA SD FISS.LISEQ_LAGR
!
    call wkvect(sdline_crack(1:14)//'_LAGR', 'G V I', neq, jlisla)
!
    do i = 1, neq
        nuno = zi(jliseq-1+i)
        ASSERT(zl(jcnsl-1+2*nuno))
        ima = cnsv(2*(nuno-1)+1)
        ino = cnsv(2*(nuno-1)+2)
        ifiss = zi(jnbpt-1+ima)
        call cesexi('C', jcesd, jcesl, ima, ino,&
                    ifiss, 1, iad)
        if (iad .gt. 0) then
            zi(jlisla-1+i) = cesv(iad)
        else
            zi(jlisla-1+i) = 1
        endif
    end do
!
    call detrsd('CHAM_NO_S', noxfem)
    call detrsd('CHAM_ELEM_S', heavno)
999 continue
!
    call jedema()
end subroutine
