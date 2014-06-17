subroutine rc36cm(iocc, etat, nbma, listma, nbchar,&
                  lichar, chmome)
    implicit none
#include "jeveux.h"
#include "asterfort/cesfus.h"
#include "asterfort/cesqua.h"
#include "asterfort/cesred.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: iocc, nbma, listma(*), nbchar, lichar(*)
    character(len=1) :: etat
    character(len=24) :: chmome
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
!     CALCUL DU TORSEUR PAR SOMMATION ALGEBRIQUE DES TORSEURS
!     CORRESPONDANT AUX DIFFERENTS CAS DE CHARGE DE LA SITUATION
!
! IN  : IOCC   : NUMERO D'OCCURRENCE DE SITUATION
! IN  : ETAT   : ETAT STABILISE A OU B POUR LE MESSAGE D'ERREUR
! IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
! IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
! IN  : NBCHAR : NOMBRE DE CAS DE CHARGE POUR UN ETAT STABILISE
! IN  : LICHAR : LISTE DES CAS DE CHARGE POUR UN ETAT STABILISE
! OUT : CHNOME : TORSEUR RESULTAT
!     ------------------------------------------------------------------
!
    integer ::   nbresu, nbcmp, icha, ir
    integer :: vali(2)
    logical :: seisme, autre
    character(len=8) :: nocmp(3)
    character(len=24) :: chams0
    complex(kind=8) :: cbid
    character(len=24), pointer :: lich(:) => null()
    logical, pointer :: licm(:) => null()
    real(kind=8), pointer :: licr(:) => null()
    character(len=24), pointer :: champ(:) => null()
    integer, pointer :: nume_char(:) => null()
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call jeveuo('&&RC3600.NUME_CHAR', 'L', vi=nume_char)
    call jeveuo('&&RC3600.CHAMP', 'L', vk24=champ)
    call jelira('&&RC3600.NUME_CHAR', 'LONMAX', nbresu)
!
    nbcmp = 3
    nocmp(1) = 'MT'
    nocmp(2) = 'MFY'
    nocmp(3) = 'MFZ'
!
    seisme = .false.
    autre = .false.
!
    AS_ALLOCATE(vk24=lich, size=nbchar)
    AS_ALLOCATE(vl=licm, size=nbchar)
    AS_ALLOCATE(vr=licr, size=nbchar)
!
    do 110, icha = 1, nbchar, 1
    do 112, ir = 1, nbresu, 1
    if (lichar(icha) .eq. nume_char(ir)) goto 114
112  continue
    vali (1) = iocc
    vali (2) = lichar(icha)
    call utmess('F', 'POSTRCCM_28', ni=2, vali=vali)
114  continue
    if (etat .eq. 'S') then
        seisme = .true.
    else
        autre = .true.
    endif
    lich(icha) = champ(ir)
    licm(icha) = .true.
    licr(icha) = 1.d0
    110 end do
!
    if (seisme .and. autre) then
        call utmess('F', 'POSTRCCM_29', si=iocc)
    endif
!
    if (nbchar .eq. 1) then
        chams0 = lich(1)
        call cesred(chams0,nbma,listma,nbcmp,nocmp,&
                    'V', chmome)
    else
!
        chams0='&&RC36CM.CHAMS0'
        if (autre) then
            call cesfus(nbchar, lich, licm, licr, [cbid],&
                        .false., 'V', chams0)
        else
            call cesqua(nbchar, lich, licm, 'V', chams0)
        endif
        call cesred(chams0,nbma,listma,nbcmp,nocmp,&
                    'V', chmome)
        call detrsd('CHAM_ELEM_S', chams0)
    endif
!
    AS_DEALLOCATE(vk24=lich)
    AS_DEALLOCATE(vl=licm)
    AS_DEALLOCATE(vr=licr)
!
    call jedema()
end subroutine
