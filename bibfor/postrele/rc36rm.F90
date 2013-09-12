subroutine rc36rm()
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!     RECUPERATION DES DONNEES DE "RESU_MECA"
!
! IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
! IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/celces.h"
#include "asterfort/codent.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
    integer :: n1, iocc, iret, jord, jnume, nbordr, jcham, nbresu
    real(kind=8) :: prec
    integer :: vali(2)
    character(len=8) :: k8b, resu, crit
    character(len=16) :: motclf, nomsym
    character(len=24) :: knum, nomcha, chams0
    character(len=24) :: valk(7)
    integer :: iarg
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'RESU_MECA'
    knum = '&&RC3600.NUME_ORDRE'
!
    call getfac(motclf, nbresu)
!
    call wkvect('&&RC3600.NUME_CHAR', 'V V I  ', nbresu, jnume)
    call wkvect('&&RC3600.CHAMP', 'V V K24', nbresu, jcham)
!
    do 10, iocc = 1, nbresu, 1
!
    call getvis(motclf, 'NUME_CHAR', iocc=iocc, scal=zi(jnume+iocc-1), nbret=n1)
!
!
    call getvid(motclf, 'RESULTAT', iocc=iocc, scal=resu, nbret=n1)
    if (n1 .ne. 0) then
        call getvtx(motclf, 'NOM_CHAM', iocc=iocc, scal=nomsym, nbret=n1)
        call getvr8(motclf, 'PRECISION', iocc=iocc, scal=prec, nbret=n1)
        call getvtx(motclf, 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
        call rsutnu(resu, motclf, iocc, knum, nbordr,&
                    prec, crit, iret)
        if (iret .ne. 0) then
            vali (1) = iocc
            valk (1) = nomsym
            valk (2) = resu
            call u2mesg('F', 'POSTRCCM_20', 2, valk, 1,&
                        vali, 0, 0.d0)
        endif
        if (nbordr .ne. 1) then
            vali (1) = iocc
            valk (1) = nomsym
            valk (2) = resu
            call u2mesg('F', 'POSTRCCM_21', 2, valk, 1,&
                        vali, 0, 0.d0)
        endif
        call jeveuo(knum, 'L', jord)
        call rsexch('F', resu, nomsym, zi(jord), nomcha,&
                    iret)
        call jedetr(knum)
!
    else
        call getvid(motclf, 'CHAM_GD', iocc=iocc, scal=nomcha, nbret=n1)
!
    endif
!
    call codent(iocc, 'D0', k8b)
    chams0 = '&&RC3602.'//k8b
    call celces(nomcha, 'V', chams0)
    zk24(jcham+iocc-1) = chams0
!
    10 end do
!
    call jedema()
end subroutine
