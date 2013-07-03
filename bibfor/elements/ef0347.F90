subroutine ef0347(nomte)
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
!     CALCUL DE EFGE_ELNO
!     ------------------------------------------------------------------
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
    character(len=16) :: nomte
    integer :: nc, i, npg
    integer :: icgp, icontn, iplouf
    logical :: okelem
!
!
! --- ------------------------------------------------------------------
!
    okelem=(nomte.eq.'MECA_POU_D_TG') .or.&
     &       (nomte.eq.'MECA_POU_D_T') .or. (nomte.eq.'MECA_POU_D_E')
    call assert(okelem)
!
    call elref4(' ', 'RIGI', iplouf, iplouf, iplouf,&
                npg, iplouf, iplouf, iplouf, iplouf)
    call assert((npg.eq.2) .or. (npg.eq.3))
!
    if (nomte .eq. 'MECA_POU_D_TG') then
        nc=7
    else
        nc=6
    endif
!
!
!
! --- ------------------------------------------------------------------
!        RECOPIE DES VALEURS AU POINT GAUSS 1 ET [2|3]
!        QUI CONTIENNENT DEJA LES EFFORTS AUX NOEUDS
!           NPG=2 : RECOPIE DES POINTS 1 ET 2
!           NPG=3 : RECOPIE DES POINTS 1 ET 3
    call jevech('PCONTRR', 'L', icgp)
    call jevech('PEFFORR', 'E', icontn)
    if (npg .eq. 2) then
        do 10 i = 1, nc
            zr(icontn-1+i)=zr(icgp-1+i)
            zr(icontn-1+i+nc)=zr(icgp-1+i+nc)
10      continue
    else
        do 20 i = 1, nc
            zr(icontn-1+i)=zr(icgp-1+i)
            zr(icontn-1+i+nc)=zr(icgp-1+i+nc+nc)
20      continue
    endif
!
end subroutine
