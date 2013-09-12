subroutine ntdcom(evolsc)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=8) :: evolsc
!
! ----------------------------------------------------------------------
!
! COMMANDE THER_NON_LINE : VERIFICATION SYNTAXIQUE SPECIFIQUES AU
!                          SECHAGE
!                          RECUPERATION DE L'EVOL_THER
!
! ----------------------------------------------------------------------
!
    integer :: iocc, k, n1, ierd, nbcham
    character(len=8) :: k8b
    character(len=16) :: comp, motcle, k16bid, nomcmd, tysd
    logical :: lrela, lsech
    integer :: iarg
!
    data         motcle / 'COMP_THER_NL' /
! ----------------------------------------------------------------------
!
!
    call getres(k8b, k16bid, nomcmd)
!
    if (nomcmd .eq. 'THER_NON_LINE') then
        call getfac(motcle, iocc)
        lrela = .false.
        lsech = .false.
        do 100 k = 1, iocc
            call getvtx(motcle, 'RELATION', iocc=k, scal=comp, nbret=n1)
            if (comp(1:10) .eq. 'SECH_NAPPE') lsech = .true.
            if (comp(1:12) .eq. 'SECH_GRANGER') lsech = .true.
            if (comp(1:5) .ne. 'SECH_') lrela = .true.
100      end do
!
        if (lsech .and. lrela) then
            call u2mess('F', 'ALGORITH8_96')
        endif
!
        evolsc = ' '
        if (lsech) then
            call getvid(' ', 'EVOL_THER_SECH', nbval=0, nbret=n1)
            if (n1 .eq. 0) then
                call u2mess('F', 'ALGORITH8_97')
            else
                call getvid(' ', 'EVOL_THER_SECH', scal=evolsc, nbret=n1)
!
! ----------VERIFICATION DU CHAMP DE TEMPERATURE
!
                call gettco(evolsc, tysd)
                if (tysd(1:9) .ne. 'EVOL_THER') then
                    call u2mesk('F', 'ALGORITH8_98', 1, evolsc)
                else
                    call dismoi('F', 'NB_CHAMP_UTI', evolsc, 'RESULTAT', nbcham,&
                                k8b, ierd)
                    if (nbcham .le. 0) then
                        call u2mesk('F', 'ALGORITH8_99', 1, evolsc)
                    endif
                endif
            endif
        endif
!
    endif
!
!-----------------------------------------------------------------------
end subroutine
