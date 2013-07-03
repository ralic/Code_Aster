subroutine lrcomm(resu, typres, nbordr, chmat, carael,&
                  modele)
    implicit  none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/copisd.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmdome.h"
#include "asterfort/nmdorc.h"
#include "asterfort/ntdoth.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbordr
    character(len=8) :: resu, chmat, carael, modele
    character(len=16) :: typres
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
!     BUT:
!       STOCKER EVENTUELLEMENT : MODELE, CHAM_MATER, CARA_ELEM, EXCIT
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   RESU     : NOM DE LA SD_RESULTAT
! IN   TYPRES   : TYPE DE LA SD_RESULTAT
! IN   CHMAT    : NOM DU CHAM_MATER
! IN   CARAEL   : NOM DU CARA_ELEM
! IN   MODELE   : NOM DU MODELE
!
! ......................................................................
!
!
!
!
!
    character(len=6) :: nompro
    parameter (nompro='LRCOMM')
!
    integer :: iordr, lordr, nexci, jpara
    integer :: i, iret, ibid, nbtrou
!
    real(kind=8) :: epsi, rbid
!
    character(len=8) :: crit, k8bid, blan8
    character(len=19) :: infcha, lischa, lisch2
    character(len=24) :: champ, noobj, fomult, k24b, compor, carcri, blan24
!
    complex(kind=8) :: cbid
!
    logical :: matcst, coecst
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    blan8 = ' '
    blan24 = ' '
    compor = blan24
!
    lischa = '&&'//nompro//'.LISCHA    '
!
    call rsorac(resu, 'LONUTI', ibid, rbid, k8bid,&
                cbid, epsi, crit, nbordr, 1,&
                nbtrou)
    if (nbordr .le. 0) call u2mess('F', 'UTILITAI2_97')
    call wkvect('&&'//nompro//'.NUME_ORDR', 'V V I', nbordr, lordr)
    call rsorac(resu, 'TOUT_ORDRE', ibid, rbid, k8bid,&
                cbid, epsi, crit, zi(lordr), nbordr,&
                nbtrou)
!
    if (chmat .ne. blan8) then
        do 10 i = 1, nbordr
            iordr=zi(lordr+i-1)
            call rsadpa(resu, 'E', 1, 'CHAMPMAT', iordr,&
                        0, jpara, k8bid)
            zk8(jpara)=chmat
10      continue
    endif
    if (carael .ne. blan8) then
        do 20 i = 1, nbordr
            iordr=zi(lordr+i-1)
            call rsadpa(resu, 'E', 1, 'CARAELEM', iordr,&
                        0, jpara, k8bid)
            zk8(jpara)=carael
20      continue
    endif
    if (modele .ne. blan8) then
        if (typres(1:9) .eq. 'EVOL_NOLI') then
            call nmdorc(modele, compor, carcri)
            if (compor .ne. blan24) then
                do 30 i = 1, nbordr
                    iordr=zi(lordr+i-1)
                    call rsexch(' ', resu, 'COMPORTEMENT', iordr, champ,&
                                iret)
                    if (iret .le. 100) then
                        call copisd('CHAMP_GD', 'G', compor(1:19), champ( 1:19))
                        call rsnoch(resu, 'COMPORTEMENT', iordr)
                    endif
30              continue
            endif
        endif
        do 40 i = 1, nbordr
            iordr=zi(lordr+i-1)
            call rsadpa(resu, 'E', 1, 'MODELE', iordr,&
                        0, jpara, k8bid)
            zk8(jpara)=modele
40      continue
    endif
    call getfac('EXCIT', nexci)
    if (nexci .gt. 0) then
        if (typres(1:4) .eq. 'DYNA' .or. typres(1:4) .eq. 'MODE') then
            call u2mesk('A', 'UTILITAI5_94', 1, typres)
            goto 60
        endif
        noobj ='12345678'//'.1234'//'.EXCIT.INFC'
        call gnomsd(' ', noobj, 10, 13)
        lisch2 = noobj(1:19)
        ibid=0
        if (typres .eq. 'EVOL_ELAS' .or. typres .eq. 'EVOL_NOLI') then
            call nmdome(k24b, k24b, k24b, lischa, blan8,&
                        ibid)
        else if (typres.eq.'EVOL_THER') then
            infcha = '&&'//nompro//'_INFCHA    '
            call ntdoth(k24b, k24b, k24b, fomult, matcst,&
                        coecst, infcha, blan8, ibid)
        endif
        do 50 i = 1, nbordr
            iordr=zi(lordr+i-1)
            call rsadpa(resu, 'E', 1, 'EXCIT', iordr,&
                        0, jpara, k8bid)
            zk24(jpara)=lisch2
50      continue
        call copisd(' ', 'G', lischa, lisch2)
    endif
60  continue
!
    call jedetr('&&'//nompro//'.NUME_ORDR')
!
    call jedema()
!
end subroutine
