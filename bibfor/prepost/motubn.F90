subroutine motubn(tabpus, dinst, nbsect)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/tbacce.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbliva.h"
#include "asterfort/tbnuli.h"
#include "asterfort/utmess.h"
    integer :: nbsect
    real(kind=8) :: dinst
    character(len=*) :: tabpus
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     OPERATEUR  "POST_USURE"
!     REMPLACEMENT DU TUBE PERCE PAR UN TUBE NEUF
!
! ----------------------------------------------------------------------
    character(len=24) :: valk
!
    integer :: ibid, i, iret, numeli
    integer :: vali
    real(kind=8) :: zero, lprec(2), acces(2)
    real(kind=8) :: valr
    complex(kind=8) :: c16b
    character(len=8) :: k8b, lcrit(2)
    character(len=19) :: nomta
    character(len=16) :: valek(2)
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomta = tabpus
    lprec(1) = 1.d-06
    lprec(2) = 1.d-06
    lcrit(1) = 'RELATIF '
    lcrit(2) = 'RELATIF '
    zero = 0.0d0
!
! --- LES PARAMETRES A REMETTRE A 0.
!        POUR L'INSTANT TRAITE:
!            V_USUR_TUBE , P_USUR_TUBE ,
!        POUR L'INSTANT TRAITE ET PAR SECTEUR:
!            V_USUR_TUBE_SECT , P_USUR_TUBE_SECT , V_USUR_TUBE_CUMU
!
    valek(1) = 'INST'
    acces(1) = dinst
!
    valek(2) = 'V_USUR_TUBE'
!
!     VERIFICATION DES PARAMETRES DE LA TABLE
    call tbexp2(nomta, 'INST')
    call tbexp2(nomta, 'SECTEUR')
    call tbexp2(nomta, 'V_USUR_TUBE')
    call tbexp2(nomta, 'P_USUR_TUBE')
    call tbexp2(nomta, 'V_USUR_TUBE_SECT')
    call tbexp2(nomta, 'P_USUR_TUBE_SECT')
    call tbexp2(nomta, 'V_USUR_TUBE_CUMU')
!
    call tbliva(nomta, 1, valek, [ibid], acces(1),&
                [c16b], k8b, lcrit(1), lprec(1), valek(2),&
                k8b, ibid, acces(2), c16b, k8b,&
                iret)
    if (iret .ne. 0) then
        valr = dinst
        valk = valek(2)
        call utmess('F', 'PREPOST5_57', sk=valk, sr=valr)
    endif
!
    call tbnuli(nomta, 2, valek, [ibid], acces,&
                [c16b], k8b, lprec, lcrit, numeli)
    if (numeli .le. 0) then
        valr = dinst
        valk = valek(2)
        call utmess('F', 'PREPOST5_58', sk=valk, sr=valr)
    endif
!
    call tbacce(nomta, numeli, valek(2), 'E', ibid,&
                zero, c16b, k8b)
!
    valek(2) = 'P_USUR_TUBE'
    call tbacce(nomta, numeli, valek(2), 'E', ibid,&
                zero, c16b, k8b)
!
    valek(2) = 'SECTEUR'
!
    do 20 i = 1, nbsect
!
        call tbnuli(nomta, 2, valek, [i], acces(1),&
                    [c16b], k8b, lprec(1), lcrit(1), numeli)
        if (numeli .le. 0) then
            valr = dinst
            vali = i
            call utmess('F', 'PREPOST5_59', si=vali, sr=valr)
        endif
!
        call tbacce(nomta, numeli, 'V_USUR_TUBE_SECT', 'E', ibid,&
                    zero, c16b, k8b)
!
        call tbacce(nomta, numeli, 'P_USUR_TUBE_SECT', 'E', ibid,&
                    zero, c16b, k8b)
!
        call tbacce(nomta, numeli, 'V_USUR_TUBE_CUMU', 'E', ibid,&
                    zero, c16b, k8b)
!
20  end do
!
    call jedema()
end subroutine
