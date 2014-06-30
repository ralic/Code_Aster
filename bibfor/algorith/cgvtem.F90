function cgvtem(resu, iord0)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: resu
    integer, intent(in) :: iord0
    logical(kind=1) :: cgvtem
!
! --------------------------------------------------------------------------------------------------
!
! CALC_G / verifications : 
!
! 1. savoir si la VARC TEMP est presente dans la sd cham_mater du RESULTAT
! 2. arreter le code en erreur <F> si une autre VARC que TEMP est presente
!
! --------------------------------------------------------------------------------------------------
!
! in  resu   : nom du RESULTAT
! in  iord0  : premier NUME_ORDRE dans resu
! out cgvtem : .true.  si la VARC TEMP est presente dans la sd cham_mater de resu
!              .false. sinon
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, jadmat, nvacr, ivarc
    character(len=8) :: chmat, k8b, other_varc
    logical(kind=1) :: exivrc, exitem
    character(len=8), pointer :: cvrcvarc(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - y a t il des VARC dans la sd cham_mater du resultat ?
!
    call rsadpa(resu, 'L', 1, 'CHAMPMAT', iord0, 0, sjv=jadmat, styp=k8b)
    chmat = zk8(jadmat)
    call jeexin(chmat//'.CVRCVARC', iret)
    exivrc = iret .ne. 0
    exitem = .false.
!
! - si oui, seule TEMP (exitem) est autorisee
!
    if (exivrc) then
!
        other_varc = ''
        call jelira(chmat// '.CVRCVARC', 'LONMAX', ival=nvacr)
        call jeveuo(chmat// '.CVRCVARC', 'L', vk8=cvrcvarc)
        do ivarc = 1, nvacr
            if ( cvrcvarc(ivarc) .eq. 'TEMP    ' ) then
                exitem = .true.
            else
                other_varc = cvrcvarc(ivarc)
            endif
        enddo
!
        if (other_varc.ne.'') call utmess('F', 'RUPTURE1_72', nk=2, valk=[other_varc,chmat])
!
    endif
!
! - valeur retournee par la fonction
!  
    cgvtem = exitem
!
    call jedema()
!
end function
