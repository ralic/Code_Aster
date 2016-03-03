subroutine rc32ma()
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/getvid.h"
#include "asterfort/rccome.h"
#include "asterfort/utmess.h"
#include "asterc/getfac.h"
#include "asterfort/wkvect.h"
#include "asterfort/getvr8.h"
#include "asterfort/rcvale.h"
#include "asterfort/jedema.h"
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE B3200 et ZE200
!     TRAITEMENT DU CHAM_MATER
!     RECUPERATION POUR CHAQUE ETAT STABILISE
!          DE  E, NU, ALPHA    SOUS ELAS
!          DE  E_REFE          SOUS FATIGUE
!          DE  M_KE, N_KE, SM  SOUS RCCM
!     ------------------------------------------------------------------
!
    character(len=8) :: mater, nocmp(7), nopa, nopb
    integer :: n1, icodre(7) ,nbsitu, ndim, jvala, jvalb
    integer :: iocc, na, nbpa, i, nb, nbpb
    real(kind=8) :: tempa, para(7), tempb
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
! --- le matériau contient-il tous les comportements nécessaires ?
    call getvid(' ', 'MATER', scal=mater, nbret=n1)
!
    call rccome(mater, 'ELAS', icodre(1))
    if (icodre(1) .eq. 1) then
        call utmess('F', 'POSTRCCM_7', sk='ELAS')
    endif
!
    call rccome(mater, 'FATIGUE', icodre(1))
    if (icodre(1) .eq. 1) then
        call utmess('F', 'POSTRCCM_7', sk='FATIGUE')
    endif
!
    call rccome(mater, 'RCCM', icodre(1))
    if (icodre(1) .eq. 1) then
        call utmess('F', 'POSTRCCM_7', sk='RCCM')
    endif
!
! --- ON STOCKE 7 VALEURS : E, NU, ALPHA, E_REFE, SM, M_KE, N_KE
!     POUR LES 2 ETATS STABILISES DE CHAQUE SITUATION
!
    nocmp(1) = 'E'
    nocmp(2) = 'NU'
    nocmp(3) = 'ALPHA'
    nocmp(4) = 'E_REFE'
    nocmp(5) = 'SM'
    nocmp(6) = 'M_KE'
    nocmp(7) = 'N_KE'
!
    call getfac('SITUATION', nbsitu)
    ndim = 7 * nbsitu
    call wkvect('&&RC3200.MATERIAU_A', 'V V R8', ndim, jvala)
    call wkvect('&&RC3200.MATERIAU_B', 'V V R8', ndim, jvalb)
!
    do 10, iocc = 1, nbsitu, 1
!
! ------ état stabilisé A
!
    call getvr8('SITUATION', 'TEMP_REF_A', iocc=iocc, scal=tempa, nbret=na)
    if (na .eq. 0) then
        nbpa = 0
        nopa = ' '
        tempa = 0.d0
    else
        nbpa = 1
        nopa = 'TEMP'
    endif
!
    call rcvale(mater, 'ELAS', nbpa, nopa, [tempa],&
                3, nocmp(1), para(1), icodre, 2)
!
    call rcvale(mater, 'FATIGUE', nbpa, nopa, [tempa],&
                1, nocmp(4), para(4), icodre, 2)
!
    call rcvale(mater, 'RCCM', nbpa, nopa, [tempa],&
                3, nocmp(5), para(5), icodre, 2)
!
    do 12 i = 1, 7
        zr(jvala-1+7*(iocc-1)+i) = para(i)
12  continue
!
! ------ état stabilisé B
!
    call getvr8('SITUATION', 'TEMP_REF_B', iocc=iocc, scal=tempb, nbret=nb)
    if (nb .eq. 0) then
        nbpb = 0
        nopb = ' '
        tempb = 0.d0
    else
        nbpb = 1
        nopb = 'TEMP'
    endif
!
    call rcvale(mater, 'ELAS', nbpb, nopb, [tempb],&
                3, nocmp(1), para(1), icodre, 2)
!
    call rcvale(mater, 'FATIGUE', nbpb, nopb, [tempb],&
                1, nocmp(4), para(4), icodre, 2)
!
    call rcvale(mater, 'RCCM', nbpb, nopb, [tempb],&
                3, nocmp(5), para(5), icodre, 2)
!
    do 14 i = 1, 7
        zr(jvalb-1+7*(iocc-1)+i) = para(i)
14  continue
!
    10 end do
!
    call jedema()
end subroutine
