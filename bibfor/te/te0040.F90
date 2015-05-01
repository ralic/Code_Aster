subroutine te0040(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/caurtg.h"
#include "asterfort/cosiro.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/pk2cau.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/vdrepe.h"
#include "asterfort/vdsiro.h"
#include "asterfort/vectan.h"
#include "asterfort/vectgt.h"
#include "asterfort/elno_coq3d.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!     ----------------------------------------------------------------
!     CALCUL DES OPTIONS DES ELEMENTS DE COQUE 3D
!     OPTIONS : EPSI_ELNO
!               SIEF_ELNO
!               SIGM_ELNO
!          -----------------------------------------------------------
!
!
!
!
!-----------------------------------------------------------------------
    integer :: icompo, iinpg
    integer :: ioutno, iret
    integer :: jcara, jgeom
    integer :: lzi, lzr, nbcou, nso
!
!
!-----------------------------------------------------------------------
!
!
    integer :: jmat, jnbspi
    integer :: nb1, nb2, npgsr, npgsn
!
!
!
!
    aster_logical :: lgreen
!
! ----------------------------------------------------------------------
!
!
    lgreen=.false.
!
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1=zi(lzi-1+1)
    nb2=zi(lzi-1+2)
    npgsr=zi(lzi-1+3)
    npgsn=zi(lzi-1+4)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
    if (nomte .eq. 'MEC3QU9H') then
        nso=4
    else if (nomte.eq.'MEC3TR7H') then
        nso=3
    endif
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PCACOQU', 'L', jcara)
!
!
    if (option .eq. 'EPSI_ELNO') then
        call jevech('PDEFOPG', 'L', iinpg)
        call jevech('PDEFONO', 'E', ioutno)
!
        else if ((option.eq.'SIEF_ELNO') .or. (option.eq.'SIGM_ELNO'))&
        then
        call cosiro(nomte, 'PCONTRR', 'L', 'UI', 'G',&
                    iinpg, 'S')
!
        call jevech('PSIEFNOR', 'E', ioutno)
        call tecach('ONN', 'PCOMPOR', 'L', iret, iad=icompo)
        if (icompo .ne. 0) then
            if (zk16(icompo+2) .eq. 'GROT_GDEP') then
                lgreen = .true.
            endif
        endif
    else
        ASSERT(.false.)
    endif
!
!
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou=zi(jnbspi-1+1)
!
!
!---  EXTRAPOLATION VERS LES NOEUDS SOMMETS
!
    call jevete('&INEL.'//nomte//'.B', ' ', jmat)
!
    call elno_coq3d(option, nomte, nb1, nb2, npgsr,&
                    npgsn, nso, nbcou, zr(jgeom), zr(jcara),&
                    zr(iinpg), zr(ioutno), zr(lzr), zr(jmat), lgreen)
    !
end subroutine
