subroutine ccvrch(resuin, numor0, lforc_noda)
    implicit none
!     --- ARGUMENTS ---
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/dismoi.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: resuin
    integer :: numor0
    aster_logical :: lforc_noda
! ======================================================================
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
! person_in_charge: nicolas.sellenet at edf.fr
!
    integer :: nchalu, jpara
!
    character(len=8) :: k8b
    character(len=24) :: excisd, modele
!
    nchalu=0
!
    if (getexm('EXCIT','CHARGE') .eq. 1) call getfac('EXCIT', nchalu)
!
    call rsadpa(resuin, 'L', 1, 'EXCIT', numor0,&
                0, sjv=jpara, styp=k8b)
    excisd=zk24(jpara)
!
    if (excisd .eq. ' ') then
        if (nchalu .eq. 0) then
!
            call rsadpa(resuin, 'L', 1, 'MODELE', numor0,&
                        0, sjv=jpara, styp=k8b)
            modele=zk8(jpara)
            ! Si on n'a pas de modele dans la sd_resu, ca veut sans doute dire
            ! qu'on est en presence d'une sd issue de la dynamique
            ! Dans ce cas-la l'emission de l'alarme CALCCHAMP_6 a tout son sens
            if (modele.ne.' ') then
                call dismoi('EXI_POUX', modele, 'MODELE', repk=k8b)
            else
                k8b = 'OUI'
            endif
!
            if (k8b.eq.'OUI' .or. lforc_noda) then
                call utmess('A', 'CALCCHAMP_6', sk=resuin)
            endif
        else
            call utmess('A', 'CALCCHAMP_5', sk=resuin)
        endif
    endif
!
end subroutine
