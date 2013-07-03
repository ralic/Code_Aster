subroutine apvepa(sdappa)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/apinfi.h"
#include "asterfort/appari.h"
#include "asterfort/apzoni.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mess.h"
    character(len=19) :: sdappa
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT
!
! VERIFICATIONS DE L'APPARIEMENT
!
! ----------------------------------------------------------------------
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nbzone, ntpt, nbpt
    integer :: typapp
    integer :: izone, ip, i
    integer :: nonapp
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> VERIFICATION DE L''APPARIEMENT'
    endif
!
! --- INITIALISATIONS
!
    ip = 1
    nonapp = 1
    call appari(sdappa, 'APPARI_NBZONE', nbzone)
    call appari(sdappa, 'APPARI_NTPT', ntpt)
!
! --- BOUCLE SUR LES ZONES
!
    do 10 izone = 1, nbzone
!
! ----- INFORMATION SUR LA ZONE
!
        call apzoni(sdappa, izone, 'NBPT', nbpt)
!
! ----- BOUCLE SUR LES POINTS
!
        do 20 i = 1, nbpt
            call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
            if (typapp .le. 0) then
                nonapp = nonapp + 1
            endif
            ip = ip + 1
20      continue
10  end do
!
    if (nonapp .eq. ip) then
        call u2mess('A', 'APPARIEMENT_1')
    endif
!
    call jedema()
!
end subroutine
