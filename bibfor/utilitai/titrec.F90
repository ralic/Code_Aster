subroutine titrec(donnee, iligd, icold, nbtitr, mxpara,&
                  para, nbpara)
    implicit none
#include "asterc/getres.h"
#include "asterfort/lxscan.h"
    character(len=*) :: donnee(*), para(*)
    integer :: iligd, icold, nbtitr, mxpara, nbpara
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     EXTRACTION DES PARAMETRES
!     ------------------------------------------------------------------
! IN  MXPARA : I  : NOMBRE MAXIMUM DE PARAMETRES ATTENDUS
! OUT PARA   : K* : LISTE DES PARAMETRES RECUS
! OUT NBPARA : I  : NOMBRE DE PARAMETRES RECUS
!     ------------------------------------------------------------------
    integer :: ival, icol, ilig
    real(kind=8) :: rval
    character(len=80) :: cval
    character(len=8) :: k8bid
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iclass, ipara
!-----------------------------------------------------------------------
    nbpara = 0
    ilig = iligd
    icol = icold
 1  continue
    call lxscan(donnee(ilig), icol, iclass, ival, rval,&
                cval)
    if (iclass .eq. -1) then
!-DEL                WRITE(6,*) ' TITREC EOF '
        icol = 1
        ilig = ilig + 1
        if (ilig .le. nbtitr) goto 1
    else if (iclass .eq. 5 .and. cval(1:1) .eq. '(') then
!-DEL                WRITE(6,*) ' TITREC "(" '
        icold = icol
        iligd = ilig
11      continue
        call lxscan(donnee(iligd), icold, iclass, ival, rval,&
                    cval)
12      continue
        if (iclass .eq. -1) then
            icold = 1
            iligd = iligd + 1
            if (iligd .gt. nbtitr) then
!CC  EXPRESSION INCORRECTE
                nbpara = 0
                goto 20
            endif
            goto 11
        else if (iclass .eq. 3 .or. iclass .eq.4) then
            nbpara = nbpara + 1
            para(nbpara) = cval(1:ival)
!CC            CALL LXCAPS(PARA(NBPARA))
            call lxscan(donnee(iligd), icold, iclass, ival, rval,&
                        cval)
            if (iclass .eq. 5 .and. cval(1:1) .eq. ',') goto 11
            goto 12
        else if (iclass .eq. 5 .and. cval(1:1) .eq. ')') then
            if (nbpara .ne. mxpara) then
                nbpara = 0
            endif
            goto 20
        else
!CC  PARAMETRE INCORRECT
            nbpara = 0
        endif
20      continue
    endif
    if (nbpara .eq. 0) then
        do 30 ipara = 1, mxpara
            call getres(para(ipara), k8bid, k8bid)
            nbpara = nbpara + 1
30      continue
    endif
end subroutine
