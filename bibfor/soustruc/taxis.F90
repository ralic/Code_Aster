subroutine taxis(noma, indic, nbma)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbliva.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: noma
    integer :: indic(*)
    integer :: nbma
! ----------------------------------------------------------------------
!     BUT: VERIFIER QUE LES COORDONNEES SONT POSITIVES
!
!     IN: NOMA   : NOM DU MAILLAGE
!         INDIC  : INDICATEUR DES MAILLES A TRAITER.
!           NBMA : NOMBRE DE MAILLES
!
!     VARIABLES LOCALES:
!     ------------------
    real(kind=8) :: valr(1), xmax, toler, r8b
    complex(kind=8) :: c16b
!
    character(len=8) :: k8b, k8a
    character(len=24) :: valk(2)
    character(len=19) :: tablg
!
!-----------------------------------------------------------------------
    integer :: iacnex, ier, ima, ino, jvale, nbnoma, numno, iret, ibid
!
!-----------------------------------------------------------------------
    call jemarq()
!
!     TOLERANCE POUR DES ABSCISSES TRES LEGEREMENT < 0 : 1.E-6*X_MAX
    call ltnotb(noma, 'CARA_GEOM', tablg)
    call tbliva(tablg, 0, ' ', ibid, r8b,&
                c16b, k8b, k8b, r8b, 'X_MAX',&
                k8b, ibid, xmax, c16b, k8b,&
                iret)
    ASSERT(iret.eq.0)
    toler=-1.d-6*abs(xmax)
!
!
!     -- ON PARCOURE LA LISTE DES MAILLES ET ON TESTE LES NOEUDS
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jvale)
    ier=0
    do 2 ima = 1, nbma
        if (indic(ima) .ne. 0) then
            call jeveuo(jexnum(noma//'.CONNEX', ima), 'L', iacnex)
            call jelira(jexnum(noma//'.CONNEX', ima), 'LONMAX', nbnoma)
            do 3 ino = 1, nbnoma
                numno=zi(iacnex-1+ino)-1
                if (zr(jvale+3*numno) .lt. toler) then
                    call jenuno(jexnum(noma//'.NOMNOE', numno+1), k8b)
                    call jenuno(jexnum(noma//'.NOMMAI', ima ), k8a)
                    if (ier .eq. 0) then
                        call utmess('F+', 'SOUSTRUC_88')
                    endif
                    valk (1) = k8b
                    valk (2) = k8a
                    valr (1) = zr(jvale+3*numno)
                    call utmess('F+', 'SOUSTRUC_89', nk=2, valk=valk, sr=valr(1))
                    ier = ier + 1
                endif
 3          continue
        endif
 2  end do
    if (ier .ne. 0) then
        call utmess('F', 'SOUSTRUC_90')
    endif
!
    call jedema()
end subroutine
