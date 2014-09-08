subroutine fgtahe(kdomm, nbcycl, epsmin, epsmax, dom)
    implicit none
#include "jeveux.h"
#include "asterfort/fgtaep.h"
#include "asterfort/fgtaes.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/rccome.h"
#include "asterfort/rcpare.h"
#include "asterfort/utmess.h"
    character(len=*) :: kdomm
    real(kind=8) :: epsmin(*), epsmax(*)
    real(kind=8) :: dom(*)
    integer :: nbcycl
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     -----------------------------------------------------------------
!     ROUTINE CHAPEAU POUR LE CALCUL DU DOMMAGE PAR LOIS DE TAHERI
!     ------------------------------------------------------------------
! IN  KDOMM  : K   : LOI DE DOMMAGE TAHERI_MANSON/TAHERI_MIXTE
! IN  NBCYCL : I   : NOMBRE DE CYCLES
! IN  EPSMIN : R   : DEFORMATIONS MINIMALES DES CYCLES
! IN  EPSMAX : R   : DEFORMATIONS MAXIMALES DES CYCLES
! OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
!     ------------------------------------------------------------------
!
!
    integer :: icodwo, icodma, icodba, icodhs, icodre(3)
    character(len=8) :: nommat, nomfo1, nomnap, cara
    character(len=16) :: pheno
!
!-----------------------------------------------------------------------
    integer :: nbval
!-----------------------------------------------------------------------
    call jemarq()
!
! --- CALCUL DU DOMMAGE ELEMENTAIRE DE TAHERI_MANSON_COFFIN
!
    if (kdomm(1:13) .eq. 'TAHERI_MANSON') then
        call getvid(' ', 'MATER', nbval=0, nbret=nbval)
        if (nbval .eq. 0) then
            call utmess('F', 'FATIGUE1_8')
        endif
        call getvid(' ', 'TAHERI_FONC', nbval=0, nbret=nbval)
        if (nbval .eq. 0) then
            call utmess('F', 'FATIGUE1_9')
        endif
        call getvid(' ', 'TAHERI_NAPPE', nbval=0, nbret=nbval)
        if (nbval .eq. 0) then
            call utmess('F', 'FATIGUE1_10')
        endif
        call getvid(' ', 'MATER', scal=nommat, nbret=nbval)
        pheno = 'FATIGUE'
        call rccome(nommat, pheno, icodre(1))
        if (icodre(1) .eq. 1) then
            call utmess('F', 'FATIGUE1_24')
        endif
        cara = 'MANSON_C'
        call rcpare(nommat, pheno, cara, icodma)
        if (icodma .ne. 0) then
            call utmess('F', 'FATIGUE1_11')
        endif
        call getvid(' ', 'TAHERI_FONC', scal=nomfo1, nbret=nbval)
        call getvid(' ', 'TAHERI_NAPPE', scal=nomnap, nbret=nbval)
        call fgtaep(nommat, nomfo1, nomnap, nbcycl, epsmin,&
                    epsmax, dom)
    endif
!
! --- CALCUL DU DOMMAGE ELEMENTAIRE DE TAHERI_MIXTE
!
    if (kdomm(1:14) .eq. 'TAHERI_MIXTE') then
        call getvid(' ', 'MATER', nbval=0, nbret=nbval)
        if (nbval .eq. 0) then
            call utmess('F', 'FATIGUE1_12')
        endif
        call getvid(' ', 'TAHERI_NAPPE', nbval=0, nbret=nbval)
        if (nbval .eq. 0) then
            call utmess('F', 'FATIGUE1_10')
        endif
        call getvid(' ', 'MATER', scal=nommat, nbret=nbval)
        pheno = 'FATIGUE'
        call rccome(nommat, pheno, icodre(1))
        if (icodre(1) .eq. 1) then
            call utmess('F', 'FATIGUE1_24')
        endif
        cara = 'MANSON_C'
        call rcpare(nommat, pheno, cara, icodma)
        cara = 'WOHLER'
        call rcpare(nommat, pheno, cara, icodwo)
        cara = 'A_BASQUI'
        call rcpare(nommat, pheno, cara, icodba)
        cara = 'A0'
        call rcpare(nommat, pheno, cara, icodhs)
        if (icodma .ne. 0) then
            call utmess('F', 'FATIGUE1_13')
        endif
        if (icodwo .ne. 0 .and. icodba .ne. 0 .and. icodhs .ne. 0) then
            call utmess('F', 'FATIGUE1_14')
        endif
        call getvid(' ', 'TAHERI_NAPPE', scal=nomnap, nbret=nbval)
        call fgtaes(nommat, nomnap, nbcycl, epsmin, epsmax,&
                    dom)
    endif
!
    call jedema()
end subroutine
