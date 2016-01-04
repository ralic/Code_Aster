function inpara(opt, te, statut, nopara)

use calcul_module, only : ca_iaopmo_, ca_iaopno_, ca_iaoptt_, ca_ilopmo_, ca_ilopno_, ca_lgco_

implicit none

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "jeveux.h"
    character(len=8) :: nopara
    character(len=3) :: statut
    integer :: opt, te
    integer :: inpara
!-----------------------------------------------------------------------
!     entrees:
!        opt    : option
!        te     : type_element
!        statut : in/out
!        nompara : nom d'1 parametre de l'option
!
!     sorties:
!        inpara : numero du parametre nompara pour(opt,te)
!                --> rend : 0 si le nompara n'est pas trouve
!
!-----------------------------------------------------------------------
    integer :: i, deb, fin, trouve, jj, optmod, optnom
    integer ::    nucalc
!-------------------------------------------------------------------

    jj = zi(ca_iaoptt_-1+ (te-1)*ca_lgco_+opt)
    if (jj .eq. 0) then
        inpara = 0
    else
        optmod = ca_iaopmo_ + zi(ca_ilopmo_-1+jj) - 1
        nucalc = zi(optmod-1+1)
        if (nucalc .le. 0) then
            inpara = 0
            goto 999
        endif
        optnom = ca_iaopno_ + zi(ca_ilopno_-1+jj) - 1
        if (statut .eq. 'IN ') then
            deb = 1
            fin = zi(optmod-1+2)

        else
            deb = zi(optmod-1+2) + 1
            fin = zi(optmod-1+2) + zi(optmod-1+3)
        endif

        trouve = 0
        do i = deb, fin
            if (nopara .eq. zk8(optnom-1+i)) then
                trouve = 1
                exit
            endif
        enddo

        if (trouve .eq. 0) then
            inpara = 0
            goto 999
        else
            inpara = i - deb + 1
            goto 999
        endif

    endif

999 continue
end function
