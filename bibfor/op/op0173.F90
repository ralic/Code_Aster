subroutine op0173()
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!
!     COMMANDE:  EXTR_TABLE
!
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterc/putvir.h"
#include "asterc/putvrr.h"
#include "asterfort/tbcopi.h"
#include "asterfort/tbimfi.h"
#include "asterfort/tbliva.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
    integer :: ibid, n1, iret, nparfi, vali
    real(kind=8) :: r8b, valr
    complex(kind=8) :: cbid, valc
    character(len=8) :: k8b, nomres, ctype, table
    character(len=16) :: nomcmd, concep, typesd
    character(len=19) :: newtab, newta1
    character(len=24) :: para
    character(len=80) :: valk
!     ------------------------------------------------------------------
!
    call jemarq()
    cbid=(0.d0,0.d0)
    r8b=0.d0
    ibid=0
!
    call getres(nomres, concep, nomcmd)
!
    call getvid(' ', 'TABLE', scal=table, nbret=n1)
    newtab = table
!
    call getvtx(' ', 'NOM_PARA', scal=para, nbret=n1)
!
    call getvtx(' ', 'TYPE_RESU', scal=typesd, nbret=n1)
!
    call getfac('FILTRE', nparfi)
    if (nparfi .ne. 0) then
        newta1 = '&&OP0173.FILTRE '
        call tbimfi(nparfi, newtab, newta1, iret)
        if (iret .ne. 0) then
            call utmess('F', 'UTILITAI7_11')
        endif
        newtab = newta1
    endif
!
    call tbliva(newtab, 0, k8b, [ibid], [r8b],&
                [cbid], k8b, k8b, [r8b], para,&
                ctype, vali, valr, valc, valk,&
                iret)
    if (iret .eq. 0) then
    else if (iret .eq. 1) then
        call utmess('F', 'CALCULEL4_43')
    else if (iret .eq. 2) then
        call utmess('F', 'CALCULEL4_44')
    else if (iret .eq. 3) then
        call utmess('F', 'CALCULEL4_45')
    else
        call utmess('F', 'CALCULEL4_46')
    endif
!
    if (typesd .eq. 'MATR_ASSE_GENE_R') then
!          ------------------------------
        call copisd('MATR_ASSE_GENE', 'G', valk, nomres)
!
    else if (typesd .eq. 'MATR_ELEM_DEPL_R') then
!          ------------------------------
        call copisd('MATR_ELEM', 'G', valk, nomres)
!
    else if (typesd .eq. 'VECT_ELEM_DEPL_R') then
!          ------------------------------
        call copisd('VECT_ELEM', 'G', valk, nomres)
!
        elseif ( typesd .eq. 'CHAM_GD_SDASTER' .or. typesd .eq.&
    'CHAM_NO_SDASTER' .or. typesd .eq. 'CARTE_SDASTER' .or. typesd&
    .eq. 'CHAM_ELEM' ) then
!          ----------------------------------------
        call copisd('CHAMP_GD', 'G', valk, nomres)
!
    else if (typesd .eq. 'MODE_MECA') then
!          ------------------------------
        call copisd('RESULTAT', 'G', valk, nomres)
!
        elseif ( typesd .eq. 'FONCTION_SDASTER' .or. typesd .eq.&
    'FONCTION_C' .or. typesd .eq. 'NAPPE_SDASTER' ) then
!          ------------------------------
        call copisd('FONCTION', 'G', valk, nomres)
!
    else if (typesd .eq. 'TABLE_SDASTER') then
        call tbcopi('G', valk, nomres)
!
    else if (typesd .eq. 'ENTIER') then
        call putvir(vali)
!
    else if (typesd .eq. 'REEL') then
        call putvrr(valr)
!
    else
        call utmess('F', 'CALCULEL4_47', sk=typesd)
    endif
!
    if (typesd .eq. 'REEL' .and. typesd .eq. 'ENTIER') then
        call titre()
    endif
!
    call jedema()
!
end subroutine
