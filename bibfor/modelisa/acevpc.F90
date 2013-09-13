subroutine acevpc(nbocc, nlm, nlg, ier)
    implicit none
#include "asterc/getres.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/utmess.h"
    integer :: nbocc, nlm, nlg, ier
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     VERIFICATION DES MOTS CLES POUR L'ELEMENT POUTRE COURBE
! ----------------------------------------------------------------------
! IN  : NBOCC  : NOMBRE D'OCCURENCE
! OUT : NLM    : NOMBRE TOTAL DE MAILLE
! OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
! ----------------------------------------------------------------------
    real(kind=8) :: xrc, xoa, xfl, xsi
    character(len=8) :: nomu
    character(len=16) :: concep, cmd
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ioc, na, nc, nf, ng, nm, np
    integer :: nr, ns, nsom
!-----------------------------------------------------------------------
    call getres(nomu, concep, cmd)
!
    nlm = 0
    nlg = 0
    do 10 ioc = 1, nbocc
        call getvtx('DEFI_ARC', 'GROUP_MA', iocc=ioc, nbval=0, nbret=ng)
        call getvtx('DEFI_ARC', 'MAILLE', iocc=ioc, nbval=0, nbret=nm)
        call getvr8('DEFI_ARC', 'RAYON', iocc=ioc, scal=xrc, nbret=nr)
        call getvr8('DEFI_ARC', 'ORIE_ARC', iocc=ioc, scal=xoa, nbret=na)
        call getvr8('DEFI_ARC', 'CENTRE', iocc=ioc, nbval=0, nbret=nc)
        call getvr8('DEFI_ARC', 'POIN_TANG', iocc=ioc, nbval=0, nbret=np)
        call getvr8('DEFI_ARC', 'COEF_FLEX', iocc=ioc, scal=xfl, nbret=nf)
        call getvr8('DEFI_ARC', 'INDI_SIGM', iocc=ioc, scal=xsi, nbret=ns)
!
        if (nr .ne. 0) then
            if (xrc .le. 0.d0) then
                call utmess('E', 'MODELISA_61')
                ier = ier + 1
            endif
        endif
        if (nc .ne. 0 .and. nc .ne. -3) then
            call utmess('E', 'MODELISA_62')
            ier = ier + 1
        endif
        if (np .ne. 0 .and. np .ne. -3) then
            call utmess('E', 'MODELISA_63')
            ier = ier + 1
        endif
        if (nf .ne. 0) then
            if (xfl .le. 0.d0) then
                call utmess('E', 'MODELISA_64')
                ier = ier + 1
            endif
        endif
        if (ns .ne. 0) then
            if (xsi .le. 0.d0) then
                call utmess('E', 'MODELISA_65')
                ier = ier + 1
            endif
        endif
!
        nsom = ng + nm
        if (nsom .eq. ng .or. nsom .eq. nm) then
            nlm = max(nlm,-nm)
            nlg = max(nlg,-ng)
        endif
!
10  end do
!
end subroutine
