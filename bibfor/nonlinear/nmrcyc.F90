function nmrcyc(sddisc, iterat, prec)
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
!
    implicit none
    logical(kind=1) :: nmrcyc
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmlere.h"
#include "asterfort/wkvect.h"
    integer :: iterat
    real(kind=8) :: prec
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!       REPERAGE DE CYCLES DANS UNE SEQUENCE DE RESIDUS
! ----------------------------------------------------------------------
! IN  SDDISC SD DISCRETISATION
! IN  ITERAT ITERATION COURANTE (DONT LE RESIDU N'EST PAS CALCULE)
! IN  PREC   TOLERANCE POUR LA RECHERCHE DES CYCLES (QQ POURCENTS)
!
!
!
!
    integer :: itemax, maxseq, lenseq, finseq, offset, jres
    real(kind=8) :: res1, res2
    character(len=24) :: residu
! ----------------------------------------------------------------------
    data residu /'&&NMRCYC.RESIDU'/
! ----------------------------------------------------------------------
!
!    INITIALISATION
    call jemarq()
    nmrcyc = .false.
    itemax = iterat-1
    maxseq = (itemax+1)/2
    if (maxseq .le. 1) goto 9999
!
!    LECTURE DES RESIDUS DE L'ITERATION 0 A ITERAT
    call wkvect(residu, 'V V R', itemax, jres)
    call nmlere(sddisc, 'L', 'VMAXI_TOUS', itemax, zr(jres))
!
!    RECHERCHE DE CYCLES DE LONGUEUR LENSEQ
    do 10 lenseq = 2, maxseq
        do 20 finseq = lenseq, itemax-lenseq
            do 30 offset = 0, lenseq-1
                res1 = zr(jres+itemax-offset)
                res2 = zr(jres+finseq-offset)
                if (abs(res1-res2)/max(res1,res2) .gt. prec) goto 1000
30          continue
            nmrcyc = .true.
            goto 2000
1000          continue
20      continue
10  end do
2000  continue
!
    call jedetr(residu)
9999  continue
    call jedema()
end function
