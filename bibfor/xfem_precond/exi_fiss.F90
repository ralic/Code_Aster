function exi_fiss(model)
    implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
!
    character(len=8) :: model
    aster_logical :: exi_fiss
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================

!     BUT : VERIFIE L EXISTENCE D UNE FISSURE XFEM AVEC FOND DANS LE MODELE
!     -----------------------------------------------------------------
!
! MODEL IN   K8 : NOM DU MODEL
!
#include "asterfort/jeveuo.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeexin.h"
!
    character(len=8) :: nomfis
    character(len=16) :: typdis
    integer :: jmofis, jnfiss, nfiss, ifiss, iret
!---------------------------------------------------------------------
!
!
    exi_fiss=.false._1
    nfiss=0
    call jeexin(model//'.FISS', iret)
    if (iret.le.0) goto 99
    call jeveuo(model//'.FISS','L',jmofis)
    call jeveuo(model//'.NFIS','L',jnfiss)
    nfiss = zi(jnfiss)
!

    do ifiss = 1,nfiss
        nomfis = zk8(jmofis-1+ifiss)
        call dismoi('TYPE_DISCONTINUITE', nomfis, 'FISS_XFEM', repk=typdis)     
        if(typdis.eq.'FISSURE') then
            exi_fiss=.true.
            exit
        endif
    enddo
!
99  continue
!
end function
