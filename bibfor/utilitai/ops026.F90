subroutine ops026()
    implicit  none
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
!    OPERATEUR DEFI_FICHIER
!
!     ------------------------------------------------------------------
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterc/putvir.h"
#include "asterc/rmfile.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/uldefi.h"
#include "asterfort/ulimpr.h"
#include "asterfort/ulnomf.h"
#include "asterfort/ulnume.h"
#include "asterfort/ulopen.h"
    integer :: unite, ifm, niv, n1, nf, nu
    logical :: sortie
    character(len=1) :: kacc, ktyp
    character(len=8) :: action, acces, type
    character(len=16) :: knom
    character(len=255) :: fichie
    integer :: iarg
!     ------------------------------------------------------------------
!
    call infmaj()
    call infniv(ifm, niv)
!
    sortie = .false.
    unite = 999
    knom = ' '
    kacc = ' '
    ktyp = 'A'
    fichie = ' '
!
    call getvtx(' ', 'ACTION', 1, iarg, 1,&
                action, n1)
    call getvtx(' ', 'FICHIER', 1, iarg, 1,&
                fichie, nf)
    call getvis(' ', 'UNITE', 1, iarg, 1,&
                unite, nu)
    call getvtx(' ', 'ACCES', 1, iarg, 1,&
                acces, n1)
    if (n1 .ne. 0) kacc = acces(1:1)
    call getvtx(' ', 'TYPE', 1, iarg, 1,&
                type, n1)
    if (n1 .ne. 0) ktyp = type(1:1)
!
    if (action .eq. 'LIBERER ') then
!          ---------------------
        if (nu .eq. 0) then
! --------- L'ACCES AU FICHIER EST REALISE PAR NOM, IL FAUT VERIFIER
!           SA PRESENCE DANS LA STRUCTURE DE DONNEES
            unite = ulnomf ( fichie, kacc, ktyp )
            if (unite .lt. 0) then
                call u2mesk('A', 'UTILITAI3_33', 1, fichie)
                goto 999
            endif
        endif
        unite = -unite
!
        elseif ( (action .eq. 'ASSOCIER') .or. (action .eq. 'RESERVER') )&
    then
!               ---------------------
        if (nu .eq. 0 .and. nf .gt. 0) then
            sortie = .true.
            unite = ulnume()
            if (unite .lt. 0) then
                call u2mess('F', 'UTILITAI3_34')
            endif
        endif
!
    else
!
        call u2mesk('F', 'UTILITAI3_35', 1, action)
!
    endif
!
    if (ktyp .eq. 'A') then
        if (action .eq. 'RESERVER') then
            call ulopen(unite, fichie, knom, kacc, 'R')
        else
            call ulopen(unite, fichie, knom, kacc, 'O')
        endif
    else
        call uldefi(unite, fichie, knom, ktyp, kacc,&
                    'O')
    endif
!
!---- POUR DETRUIRE LE FICHIER SI CE DERNIER EST OUVERT EN NEW
!
    if (ktyp .ne. 'A') then
        if (kacc .eq. 'N') then
            call rmfile(fichie, 1)
        endif
    endif
!
    if (sortie) call putvir(unite)
!
999  continue
    if (niv .gt. 1) call ulimpr(ifm)
!
end subroutine
