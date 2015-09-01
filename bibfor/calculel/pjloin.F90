subroutine pjloin(nbnod,nbnodm,m2,geom2,nbmax,tino2m,tdmin2,lino_loin)
    implicit none

    integer, intent(in) :: nbnod, nbnodm,nbmax, tino2m(nbmax),lino_loin(*)
    real(kind=8), intent(in) :: tdmin2(nbmax)
    real(kind=8), intent(in) :: geom2(*)
    character(len=8), intent(in) :: m2

#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvtx.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/crea_maillage.h"
#include "asterfort/irmail.h"
#include "asterfort/ulnume.h"
#include "asterfort/ulopen.h"
#include "asterfort/gcncon.h"
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
!     BUT :
!       Emettre (eventuellement) le message d'alarme de projection sur
!       des mailles lointaines
!       Cette routien sert a mettre en commun du fortran pour plusieurs routines
! ----------------------------------------------------------------------
!
    character(len=8) ::  alarme, madebug, k8bid,kico
    character(len=16) :: k16bid, nomcmd, formar
    integer ::  ibid
!
    integer :: vali(2)
    integer :: ii, ino2m, unite, ico=0
    real(kind=8) :: valr(4)
    character(len=80) :: valk(2)
    character(len=8) :: nono2
    character(len=80) :: fichier
    save ico
! --- DEB --------------------------------------------------------------
!
    call jemarq()
!
!
    alarme='OUI'
    call getres(k16bid, k16bid, nomcmd)
    if (nomcmd .eq. 'PROJ_CHAMP') then
        call getvtx(' ', 'ALARME', scal=alarme, nbret=ibid)
    endif

    if (alarme .eq. 'OUI') then
        ico=ico+1
        call codent(ico, 'D0', kico)
        do ii = 1, nbnod
            ino2m = tino2m(ii)
            call jenuno(jexnum(m2//'.NOMNOE', ino2m), nono2)
            valr(1) = geom2(3*(ino2m-1)+1)
            valr(2) = geom2(3*(ino2m-1)+2)
            valr(3) = geom2(3*(ino2m-1)+3)
            valr(4) = tdmin2(ii)
            call utmess('I', 'CALCULEL5_43', sk=nono2, nr=4, valr=valr)
        enddo
        vali(1) = nbnodm
        vali(2) = nbnod
        fichier='REPE_OUT/maillage_proj_loin_'//kico//'.med'
        valk(1) = fichier
        call utmess('A', 'CALCULEL5_48',ni=2,vali=vali,nk=1,valk=valk)

!       -- Creation et impression d'un "petit" maillage contenant juste les noeuds
!          lointains. Cela peut aider l'utilisateur a les visualiser.
        call gcncon('_',madebug)
        call crea_maillage(m2,madebug,'V',nbno=nbnodm,lino=lino_loin)

        unite = ulnume()
        if (unite.le.0) call utmess('F', 'UTILITAI5_10')
        call ulopen(unite, fichier, ' ', 'N', 'O')
        formar=' '
        call irmail('MED', unite, ibid, madebug, ASTER_FALSE , k8bid, ibid, 1, formar)
        call ulopen(-unite, k8bid, k8bid, k8bid, k8bid)
        call detrsd('MAILLAGE', madebug)
    endif

    call jedema()
end subroutine
