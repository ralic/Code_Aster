subroutine op0023()
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     COMMANDE:  TEST_RESU
! ----------------------------------------------------------------------
!     REMARQUES:  RESU:( RESULTAT:
!                        PRECISION: ( PREC1 , PREC2 )          L_R8
!                        CRITERE  : ( CRIT1 , CRIT2 )          L_TXM
!     PREC1 ET CRIT1 SONT LA PRECISION ET LE CRITERE DU TEST
!     PREC2 ET CRIT2 SONT LA PRECISION ET LE CRITERE DE L'EXTRACTION
! ----------------------------------------------------------------------
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterc/r8nnem.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/iunifi.h"
#include "asterfort/trcart.h"
#include "asterfort/trchel.h"
#include "asterfort/trchno.h"
#include "asterfort/trgene.h"
#include "asterfort/trjeve.h"
#include "asterfort/trmail.h"
#include "asterfort/trresu.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
    real(kind=8) :: tstnan, resnan
    integer :: ific, nocc, n
    character(len=8) :: repons
    character(len=16) :: nomfi
!     ------------------------------------------------------------------
!     TEST DU MECANISME DE NAN
    call getvtx(' ', 'TEST_NAN', scal=repons, nbret=n)
    if (repons .eq. 'OUI') then
        tstnan = r8nnem ( )
        resnan = tstnan*1.d0
        if (isnan(resnan)) resnan = 0.d0
    endif

    call infmaj()

    nomfi = ' '
    ific = iunifi('RESULTAT')
    if (.not. ulexis( ific )) then
        call ulopen(ific, ' ', nomfi, 'NEW', 'O')
    endif
    write (ific,1000)


!     --- TRAITEMENT D'UN OBJET JEVEUX  ---
    call getfac('OBJET', nocc)
    if (nocc .ne. 0)  call trjeve(ific, nocc)


!     --- TRAITEMENT D'UN MAILLAGE ---
    call getfac('MAILLAGE', nocc)
    if (nocc .ne. 0)  call trmail(ific, nocc)


!     --- TRAITEMENT D'UN CHAM_NO ---
    call getfac('CHAM_NO', nocc)
    if (nocc .ne. 0)  call trchno(ific, nocc)


!     --- TRAITEMENT D'UN CHAM_ELEM ---
    call getfac('CHAM_ELEM', nocc)
    if (nocc .ne. 0)  call trchel(ific, nocc)


!     --- TRAITEMENT D'UNE CARTE ---
    call getfac('CARTE', nocc)
    if (nocc .ne. 0) call trcart(ific, nocc)


!     --- TRAITEMENT D'UN CONCEPT RESULTAT ---
    call getfac('RESU', nocc)
    if (nocc .ne. 0)  call trresu(ific, nocc)


!     --- TRAITEMENT D'UN CONCEPT GENE ---
    call getfac('GENE', nocc)
    if (nocc .ne. 0)  call trgene(ific, nocc)

    1000 format (/,80 ('-'))
end subroutine
