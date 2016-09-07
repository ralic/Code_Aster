subroutine lrcomm(resu, typres, nbordr, chmat, carael,&
                  modele, noch, from_lire_resu)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvis.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmdoco.h"
#include "asterfort/nmdoch.h"
#include "asterfort/nmdorc.h"
#include "asterfort/ntdoch.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcomp.h"
#include "asterfort/wkvect.h"
    integer :: nbordr
    character(len=8) :: resu, chmat, carael, modele
    character(len=16) :: typres
    character(len=*) :: noch
    aster_logical, intent(in), optional :: from_lire_resu
! ----------------------------------------------------------------------
! ======================================================================bibfor/prepost/lrcomm.F90
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: iordr, lordr, nexci, jpara
    integer :: i, iret, ibid, nbtrou, tord(1), nume_plan
    real(kind=8) :: epsi, rbid
    character(len=8) :: crit, k8bid
    character(len=19) :: list_load, list_load_save, vari, ligrmo, list_load_resu
    character(len=24) :: champ, noobj, compor, carcri, mod24, car24
    complex(kind=8) :: cbid
    aster_logical :: l_etat_init, l_load_user
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    !blan8 = ' '
    compor = ' '
    l_etat_init = .false.
    l_load_user = .true.
!
    list_load      = '&&LRCOMM.LISCHA'
    list_load_resu = '&&LRCOMM.LISCHA'
!
    call rsorac(resu, 'LONUTI', ibid, rbid, k8bid,&
                cbid, epsi, crit, tord, 1,&
                nbtrou)
    nbordr=tord(1)            
    if (nbordr .le. 0) then
        call utmess('F', 'UTILITAI2_97')
    endif
    call wkvect('&&LRCOMM.NUME_ORDR', 'V V I', nbordr, lordr)
    call rsorac(resu, 'TOUT_ORDRE', ibid, rbid, k8bid,&
                cbid, epsi, crit, zi(lordr), nbordr,&
                nbtrou)
!
    if (chmat .ne. ' ') then
        do i = 1, nbordr
            iordr=zi(lordr+i-1)
            call rsadpa(resu, 'E', 1, 'CHAMPMAT', iordr,&
                        0, sjv=jpara)
            zk8(jpara)=chmat
        end do
    endif
!
    if (carael .ne. ' ') then
        do i = 1, nbordr
            iordr=zi(lordr+i-1)
            call rsadpa(resu, 'E', 1, 'CARAELEM', iordr,&
                        0, sjv=jpara)
            zk8(jpara)=carael
        end do
    endif
!
    if (typres .eq. 'MODE_EMPI') then
        call getvis(' ', 'NUME_PLAN', scal=nume_plan, nbret=iret)
        if (iret .eq. 0) then
            nume_plan = 0
        endif
        do i = 1, nbordr
            iordr=zi(lordr+i-1)
            call rsadpa(resu, 'E', 1, 'NUME_PLAN', iordr, 0, sjv=jpara)
            zi(jpara)=nume_plan
            call rsadpa(resu, 'E', 1, 'NOM_CHAM', iordr, 0, sjv=jpara)
            zk24(jpara)=noch
        end do
    endif
!
    if (modele .ne. ' ') then
        if (typres(1:9) .eq. 'EVOL_NOLI') then
            call nmdorc(modele, chmat, l_etat_init, compor, carcri)
            if (compor .ne. ' ') then
                do i = 1, nbordr
                    iordr=zi(lordr+i-1)
                    call rsexch(' ', resu, 'COMPORTEMENT', iordr, champ,&
                                iret)
                    if (iret .le. 100) then
                        call copisd('CHAMP_GD', 'G', compor(1:19), champ( 1:19))
                        call rsnoch(resu, 'COMPORTEMENT', iordr)
                    endif
                end do
            endif
        endif
        do i = 1, nbordr
            iordr=zi(lordr+i-1)
            call rsadpa(resu, 'E', 1, 'MODELE', iordr,&
                        0, sjv=jpara)
            zk8(jpara)=modele
        end do
    endif
!
    call getfac('EXCIT', nexci)
    if (nexci .gt. 0) then
        if (typres(1:4) .eq. 'DYNA' .or. typres(1:4) .eq. 'MODE') then
            call utmess('A', 'UTILITAI5_94', sk=typres)
            goto 60
        endif
        noobj ='12345678'//'.1234'//'.EXCIT.INFC'
        call gnomsd(' ', noobj, 10, 13)
        list_load_save = noobj(1:19)
        if (typres .eq. 'EVOL_ELAS' .or. typres .eq. 'EVOL_NOLI') then
            call nmdoch(list_load, l_load_user, list_load_resu)
        else if (typres.eq.'EVOL_THER') then
            call ntdoch(list_load)
        endif
        do i = 1, nbordr
            iordr=zi(lordr+i-1)
            call rsadpa(resu, 'E', 1, 'EXCIT', iordr,&
                        0, sjv=jpara)
            zk24(jpara)=list_load_save
        end do
        call copisd(' ', 'G', list_load, list_load_save)
    endif
 60 continue
!
! - Check comportment 
!
    if (typres(1:9) .eq. 'EVOL_NOLI') then
        do i = 1, nbordr
            iordr=zi(lordr+i-1)
            call rsexch(' ', resu, 'VARI_ELGA', iordr, vari,&
                        iret)
            if (iret .eq. 0) then
                call dismoi('NOM_LIGREL', modele, 'MODELE', repk=ligrmo)
                mod24 = modele
                car24 = carael
                call nmdoco(mod24, car24, compor)
                if (present(from_lire_resu)) then
                   call vrcomp(compor, vari, ligrmo, iret, type_stop = 'A',&
                               from_lire_resu = from_lire_resu)
                else
                   call vrcomp(compor, vari, ligrmo, iret, type_stop = 'A')
                endif 
                if (iret .eq. 1) then
                    call utmess('A', 'RESULT1_1')
                endif
            endif
        end do
    endif
!
    call jedetr('&&LRCOMM.NUME_ORDR')
!
    call jedema()
!
end subroutine
