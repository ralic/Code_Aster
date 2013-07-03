subroutine rftabl(tabres)
    implicit   none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvtx.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/foattr.h"
#include "asterfort/foimpr.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ordonn.h"
#include "asterfort/tbexfo.h"
#include "asterfort/tbimfi.h"
#include "asterfort/tbliva.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=*) :: tabres
!     ------------------------------------------------------------------
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
!     OPERATEUR "RECU_FONCTION"   MOT CLE "TABLE"
!     ------------------------------------------------------------------
    integer :: ibid, n2, n3, n4, nparfi, iret
    integer :: ifm, niv
    real(kind=8) :: r8b
    complex(kind=8) :: c16b
    character(len=8) :: k8b, interp, prolgd
    character(len=16) :: nomcmd, typcon, parax, paray
    character(len=19) :: nomfon, newtab, newta1
    character(len=24) :: nopara, nomf
    character(len=24) :: valk(2)
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomfon, typcon, nomcmd)
!
    call getvtx(' ', 'PARA_X', 0, iarg, 1,&
                parax, n2)
    call getvtx(' ', 'PARA_Y', 0, iarg, 1,&
                paray, n3)
    call getvtx(' ', 'NOM_PARA_TABL', 0, iarg, 1,&
                nopara, n4)
!
    interp = 'NON NON '
    prolgd = 'EE      '
!
    newtab = tabres
!
!     ------------------------------------------------------------------
!
!                 --- TRAITEMENT DU MOT CLE "FILTRE" ---
!
!     ------------------------------------------------------------------
    call getfac('FILTRE', nparfi)
    if (nparfi .ne. 0) then
        newta1 = '&&OP0177.FILTRE '
        call tbimfi(nparfi, newtab, newta1, iret)
        if (iret .ne. 0) call u2mess('F', 'UTILITAI7_11')
        newtab = newta1
    endif
!     ------------------------------------------------------------------
!
    if (n2+n3 .ne. 0) then
!
        call tbexfo(newtab, parax, paray, nomfon, interp,&
                    prolgd, 'G')
!
    else if (n4 .ne. 0) then
!
        call tbliva(newtab, 0, k8b, ibid, r8b,&
                    c16b, k8b, k8b, r8b, nopara,&
                    k8b, ibid, r8b, c16b, nomf,&
                    iret)
        if (iret .ne. 0) then
            valk(1) = nopara
            valk(2)(1:19) = newtab
            call u2mesk('F', 'MODELISA2_91', 2, valk)
        endif
        call copisd('FONCTION', 'G', nomf, nomfon)
!
    else
        call u2mess('F', 'UTILITAI4_27')
    endif
!
!
    if (nparfi .ne. 0) call detrsd('TABLE', newta1)
!
    call foattr(' ', 1, nomfon)
!
!     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
!         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
    call ordonn(nomfon, 0)
!
    call titre()
    if (niv .gt. 1) call foimpr(nomfon, niv, ifm, 0, k8b)
!
    call jedema()
end subroutine
