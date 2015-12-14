subroutine mecoe1(opt, te)

use calcul_module, only : ca_iamloc_, ca_iaoppa_, ca_iawlo2_, ca_igr_,&
     ca_ilmloc_, ca_nbgr_, ca_npario_

implicit none

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

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/modatt.h"
#include "asterfort/nbpara.h"
#include "asterfort/nopara.h"

    integer :: opt, te
!-----------------------------------------------------------------------
! but : initialisation de '&&CALCUL.IA_CHLO2'
! entrees:
!      opt : option
!      te  : type d'element
!-----------------------------------------------------------------------
    integer :: icode
    integer :: iparg, m2, modloc, nbpoin, nbscal, npara
    integer :: ipar
    character(len=8) :: nompar
!-----------------------------------------------------------------------

!   -- on met a "-1" lgcata pour les parametres inconnus
!      des type_element
    do iparg = 1,ca_npario_
        zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)=-1
    end do


    npara = nbpara(opt,te,'IN ')
    do ipar = 1, npara
        nompar = nopara(opt,te,'IN ',ipar)
        iparg = indik8(zk8(ca_iaoppa_),nompar,1,ca_npario_)
        m2 = modatt(opt,te,'IN ',ipar)
        modloc = ca_iamloc_ - 1 + zi(ca_ilmloc_-1+m2)
        icode = zi(modloc-1+1)
        nbscal = zi(modloc-1+3)
        if (icode .le. 3) then
            nbpoin = zi(modloc-1+4)
        else
            nbpoin = 0
        endif

        zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+1)=m2
        zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)=nbscal
        zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+3)=nbpoin
    end do


    npara = nbpara(opt,te,'OUT')
    do ipar = 1, npara
        nompar = nopara(opt,te,'OUT',ipar)
        iparg = indik8(zk8(ca_iaoppa_),nompar,1,ca_npario_)
        m2 = modatt(opt,te,'OUT',ipar)
        modloc = ca_iamloc_ - 1 + zi(ca_ilmloc_-1+m2)
        icode = zi(modloc-1+1)
        nbscal = zi(modloc-1+3)
        if (icode .le. 3) then
            nbpoin = zi(modloc-1+4)
        else
            nbpoin = 0
        endif

        zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+1)=m2
        zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)=nbscal
        zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+3)=nbpoin
    end do

end subroutine
