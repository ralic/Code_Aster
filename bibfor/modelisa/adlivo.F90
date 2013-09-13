subroutine adlivo(mv, is, nvtot, nvoima, nscoma,&
                  touvoi)
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
    implicit none
#include "asterfort/utmess.h"
    integer :: mv, is, nvtot
    integer :: nvoima, nscoma
    integer :: touvoi(1:nvoima, 1:nscoma+2)
    integer :: iv, nsco, isco
    logical :: trma, trso
!
!  AJOUTE A LA LISTE DE TOUS LES VOISINS DE LA MAILLE COURANTE MO
!  LA MAILLE MV ET LE SOMMET IS SI CETTE MAILE N EXISTE PAS DEJA
!
!       : M ,MAILLE VOISINE
!       : IS ,SOMMET LOCAL DE LA MAILLE COURANTE
!         QUI EST AUSSI SOMMET DE MV
!    VAR NVTOT : NOMBRE TOTAL DE VOI
!         TOUVOI : CES VOISINS :
!    CONTENU DU TABLEAU TOUVOI :
!     TOUVOI(IV,1) : MAILLE VOISINE
!     TOUVOI(IV,2) : NOMBRE DE SOMMETS COMMUNS
!     TOUVOI(IV,2+IS) : CES SOMMETS COMMUNS DANS NUMEROTATION DE M0
!
    trma=.false.
    do 30 iv = 1, nvtot
        if (mv .eq. touvoi(iv,1)) then
            trma=.true.
            trso=.false.
            nsco=touvoi(iv,2)
            do 10 isco = 1, nsco
                if (touvoi(iv,2+isco) .eq. is) then
                    trso=.true.
                    goto 20
!
                endif
10          continue
20          continue
            if (.not.trso) then
                nsco=nsco+1
                if (nsco .gt. nscoma) then
                    call utmess('F', 'VOLUFINI_4', si=nsco)
                endif
                touvoi(iv,2)=nsco
                touvoi(iv,2+nsco)=is
            endif
            goto 40
!
        endif
30  end do
40  continue
    if (.not.trma) then
        nvtot=nvtot+1
        if (nvtot .gt. nvoima) then
            call utmess('F', 'VOLUFINI_3', si=nvtot)
        endif
        touvoi(nvtot,1)=mv
        touvoi(nvtot,2)=1
        touvoi(nvtot,3)=is
    endif
!
end subroutine
