subroutine crvrc2()
    implicit none
! ----------------------------------------------------------------------
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
!
!     COMMANDE:  CREA_RESU
!     CREE UNE STRUCTURE DE DONNEE DE TYPE "EVOL_THER"  CONTENANT
!     LA TEMPERATURE SUR LES COUCHES DES COQUES MULTICOUCHE A PARTIR
!     D'UN EVOL_THER CONTENANT TEMP/TEMP_INF/TEMP_SUP
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/cesvar.h"
#include "asterfort/detrsd.h"
#include "asterfort/exlima.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/u2mesk.h"
    integer :: nbfac, n1, nbinst, kinst, jordr1, iordr
    integer :: nbin, iret, jinst, jtemp, iexi
    real(kind=8) :: vinst
    character(len=8) :: kbid, resu, modele, model2, carele, paout, lpain(10)
    character(len=16) :: type, oper
    character(len=19) :: ligrel, chout, resu1, chtemp
    character(len=24) :: chcara(18), lchin(10)
    logical :: exicar
    integer :: iarg
!
!----------------------------------------------------------------------
    call jemarq()
!
    call getfac('PREP_VRC2', nbfac)
    if (nbfac .eq. 0) goto 20
    ASSERT(nbfac.eq.1)
!
!
    call getres(resu, type, oper)
    call getvid('PREP_VRC2', 'MODELE', iocc=1, scal=modele, nbret=n1)
    call getvid('PREP_VRC2', 'CARA_ELEM', iocc=1, scal=carele, nbret=n1)
    call getvid('PREP_VRC2', 'EVOL_THER', iocc=1, scal=resu1, nbret=n1)
!
!     -- ON VERIFIE QUE LE CARA_ELEM S'APPUIE BIEN SUR LE MODELE
    call jeexin(carele//'.CANBSP    .CELK', iexi)
    if (iexi .eq. 0) call u2mesk('F', 'CALCULEL4_14', 1, carele)
    call jeveuo(carele//'.CANBSP    .CELK', 'L', jtemp)
    model2=zk24(jtemp-1+1)(1:8)
    if (model2 .ne. modele) call u2mesk('F', 'CALCULEL4_15', 1, carele)
!
!
!
!
    call jelira(resu1//'.ORDR', 'LONUTI', nbinst)
    call jeveuo(resu1//'.ORDR', 'L', jordr1)
    ASSERT(nbinst.gt.0)
!
    call jeexin(resu//'           .DESC', iret)
    if (iret .ne. 0) then
        call u2mesk('F', 'CALCULEL7_6', 1, resu)
    else
        call rscrsd('G', resu, 'EVOL_THER', nbinst)
    endif
!
    paout = 'PTEMPCR'
    call mecara(carele, exicar, chcara)
!
    call exlima('PREP_VRC2', 1, 'G', modele, ligrel)
!
    lpain(1) = 'PNBSP_I'
    lchin(1) = chcara(1) (1:8)//'.CANBSP'
    lpain(2) = 'PTEMPER'
    lpain(3) = 'PCACOQU'
    lchin(3) = chcara(7)
    nbin = 3
!
!     -- BOUCLE SUR LES INSTANTS :
!     --------------------------------
    do 10,kinst = 1,nbinst
    iordr = zi(jordr1+kinst-1)
    call rsexch('F', resu1, 'TEMP', iordr, chtemp,&
                iret)
    lchin(2) = chtemp
!
    call rsexch(' ', resu, 'TEMP', iordr, chout,&
                iret)
    call cesvar(carele, ' ', ligrel, chout)
    call calcul('S', 'PREP_VRC', ligrel, nbin, lchin,&
                lpain, 1, chout, paout, 'G',&
                'OUI')
    call detrsd('CHAM_ELEM_S', chout)
    call rsnoch(resu, 'TEMP', iordr)
    call rsadpa(resu1, 'L', 1, 'INST', iordr,&
                0, jinst, kbid)
    vinst=zr(jinst)
    call rsadpa(resu, 'E', 1, 'INST', iordr,&
                0, jinst, kbid)
    zr(jinst) = vinst
!
    10 end do
!
!
20  continue
    call jedema()
end subroutine
