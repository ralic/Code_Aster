subroutine op0063()
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
!        RECUPERATION RESULTATS DE MODE_NON_LINE
!-----------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterc/r8vide.h"
#include "asterfort/tbliva.h"
#include "asterfort/transft.h"
#include "asterfort/vprecu.h"
    integer :: iordr, ival(1), ibid, iret, neq
    integer :: nbpara, nbtrou, nbmode, npari, nparr, npark, lvect
    integer :: ktbnp, nblig
    integer :: inbpt
    character(len=4) :: nomsym(1)
    character(len=8) :: k8b, modein, nomres, modenl, kvide
    character(len=16) :: lipar(1), typres, nomcmd, k16bid, tres
    character(len=19) :: typmod
    character(len=24) :: k24bid, kvec
    real(kind=8) :: rvide
    real(kind=8) :: r8b
    complex(kind=8) :: c16b, cvide
!
    call jemarq()
    call getres(nomres, typres, nomcmd)
!
    kvide = '????????'
    rvide = r8vide()
    cvide = dcmplx(rvide,rvide)
!
    call getvid(' ', 'MODE_NON_LINE', scal=modenl)
!
    call jeveuo(modenl//'           .TBNP', 'L', ktbnp)
    nbpara = zi(ktbnp )
    nblig = zi(ktbnp+1)
!
    call getvis(' ', 'NUME_ORDRE', scal=iordr)
    call getvis(' ', 'NB_INST', scal=inbpt)
!
    ival(1) = iordr
    lipar(1) = 'NUME_ORDRE'
!
    call tbliva(modenl, 1, lipar, ival, [r8b],&
                [c16b], k8b, k8b, [r8b], 'NOM_SD',&
                k8b, ibid, r8b, c16b, modein,&
                iret)
!
    call getvtx(' ', 'TYPE_RESU', scal=tres)
!
    if (tres .eq. 'MODE_MECA') then
        call copisd('RESULTAT', 'G', modein, nomres)
    else
        kvec = '&&_COEFF_FOURIER'
        nomsym(1) = 'DEPL'
        nbpara = 0
        nbtrou = -1
        call vprecu(modein, nomsym(1), nbtrou, [ibid], kvec,&
                    nbpara, k16bid, k24bid, k24bid, k24bid,&
                    neq, nbmode, typmod, npari, nparr,&
                    npark)
        call jeveuo(kvec, 'L', lvect)
        call transft(modein, kvec, neq, inbpt, nomres)
        call jedetr(kvec)
    endif
!
    call jedema()
!
end subroutine
