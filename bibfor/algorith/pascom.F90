subroutine pascom(meca, sddyna, sddisc)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
    character(len=8) :: meca
    character(len=19) :: sddyna, sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE DYNA_NON_LINE (UTILITAIRE)
!
! EVALUATION DU PAS DE TEMPS DE COURANT POUR LE MODELE
! PRISE EN COMPTE D'UNE BASE MODALE
!
! ----------------------------------------------------------------------
!
!
!
! IN  MECA   : BASE MODALE (MODE_MECA)
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE (CF NDLECT)
! IN  SDDISC : SD DISCRETISATION
!
!
!
!
    integer :: n1, i, ibid
    integer :: iad, nbinst
    integer :: nbmode
    integer :: iorol
    real(kind=8) :: dtcou, phi, dt, r8b
    character(len=8) :: k8bid, stocfl
    integer, pointer :: ordr(:) => null()
    real(kind=8), pointer :: ditr(:) => null()
!
! ---------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
!
!     INITIALISATION DE DTCOU
!
    dtcou = -1.d0
!
!     --- RECUPERATION DES FREQUENCES PROPRES
!
    call jelira(meca//'           .ORDR', 'LONUTI', nbmode)
    call jeveuo(meca//'           .ORDR', 'L', vi=ordr)
    iorol = ordr(1)
    call rsadpa(meca, 'L', 1, 'OMEGA2', iorol,&
                0, sjv=iad, styp=k8bid)
    if (zr(iad) .lt. 0.d0 .or. abs(zr(iad)) .lt. r8prem( )) then
        dtcou = 1.d0 / r8prem( )
    else
        dtcou = 1.d0 / sqrt(zr(iad))
    endif
    do 21 i = 1, nbmode-1
        iorol = ordr(1+i)
        call rsadpa(meca, 'L', 1, 'OMEGA2', iorol,&
                    0, sjv=iad, styp=k8bid)
        if (zr(iad) .lt. 0.d0 .or. abs(zr(iad)) .lt. r8prem( )) then
            dt = 1.d0 / r8prem( )
        else
            dt = 1.d0 / sqrt(zr(iad))
        endif
!       DT = 1.D0 / SQRT(ZR(IAD))
        if (dt .lt. dtcou) dtcou = dt
21  end do
!
    call getvtx('SCHEMA_TEMPS', 'STOP_CFL', iocc=1, scal=stocfl, nbret=n1)
!
!     VERIFICATION DE LA CONFORMITE DE LA LISTE D'INSTANTS
    call utdidt('L', sddisc, 'LIST', ibid, 'NBINST',&
                r8b, nbinst, k8bid)
    call jeveuo(sddisc//'.DITR', 'L', vr=ditr)
!
    if (ndynlo(sddyna,'DIFF_CENT')) then
        dtcou =dtcou/(2.d0)
        call utmess('I', 'DYNAMIQUE_7', sr=dtcou)
    else
        if (ndynlo(sddyna,'TCHAMWA')) then
            phi=ndynre(sddyna,'PHI')
            dtcou = dtcou/(phi*2.d0)
            call utmess('I', 'DYNAMIQUE_8', sr=dtcou)
        else
            call utmess('F', 'DYNAMIQUE_1')
        endif
    endif
!
    do 20 i = 1, nbinst-1
        if (ditr(i+1)-ditr(i) .gt. dtcou) then
            if (stocfl(1:3) .eq. 'OUI') then
                call utmess('F', 'DYNAMIQUE_2')
            else
                call utmess('A', 'DYNAMIQUE_2')
            endif
        endif
20  end do
!
    call jedema()
!
end subroutine
