subroutine charth(load, vale_type)
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/adalig.h"
#include "asterfort/caddli.h"
#include "asterfort/caechp.h"
#include "asterfort/cagene.h"
#include "asterfort/cagrou.h"
#include "asterfort/caliag.h"
#include "asterfort/caliai.h"
#include "asterfort/calich.h"
#include "asterfort/calirc.h"
#include "asterfort/cbconv.h"
#include "asterfort/cbecha.h"
#include "asterfort/cbflnl.h"
#include "asterfort/cbflux.h"
#include "asterfort/cbgrai.h"
#include "asterfort/cbprca.h"
#include "asterfort/cbrayo.h"
#include "asterfort/cbsonl.h"
#include "asterfort/cbsour.h"
#include "asterfort/cormgi.h"
#include "asterfort/initel.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/verif_affe.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=4), intent(in) :: vale_type
    character(len=8), intent(in) :: load
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Treatment of loads for AFFE_CHAR_THER_*
!
! --------------------------------------------------------------------------------------------------
!
!
! In  vale_type : affected value type (real, complex or function)
! In  load      : name of load
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_dim, iret
    character(len=8) :: mesh, model
    character(len=16) :: keywordfact, command
    character(len=19) :: ligrch, ligrmo
    character(len=8), pointer :: p_ligrch_lgrf(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
!
! - Mesh, Ligrel for model, dimension of model
!
    command = 'AFFE_CHAR_THER'
    call cagene(load, command, ligrmo, mesh, nb_dim)
    model  = ligrmo(1:8)
    if (nb_dim .gt. 3) then
        call utmess('A', 'CHARGES2_4')
    endif
!
! - Ligrel for loads
!
    ligrch = load//'.CHTH.LIGRE'
!
    if (vale_type .eq. 'REEL') then
!
! ----- SOURCE
!
        call cbsour(load, mesh, ligrmo, nb_dim, vale_type)
!
! ----- SOUR_NL
!
        call cbsonl(load, mesh, ligrmo, nb_dim, vale_type)
!
! ----- CONVECTION
!
        call cbconv(load)
!
! ----- FLUX_REP
!
        call cbflux(load, mesh, ligrmo, nb_dim, vale_type)
!
! ----- FLUX_NL
!
        call cbflnl(load, mesh, ligrmo, vale_type)
!
! ----- RAYONNEMENT
!
        call cbrayo(load, mesh, ligrmo, vale_type)
!
! ----- ECHANGE
!
        call cbecha(load, mesh, ligrmo, nb_dim, vale_type)
!
! ----- ECHANGE_PAROI
!
        call caechp(load, ligrch, ligrmo, mesh, vale_type, &
                    nb_dim)
!
! ----- EVOL_CHAR
!
        call cbprca('THERMIQUE', load)
!
! ----- GRADIENT INITIAL
!
        call cbgrai(load, mesh, ligrmo, vale_type)
!
! ----- TEMP_IMPO
!
        keywordfact = 'TEMP_IMPO'
        call caddli(keywordfact, load, mesh, ligrmo, vale_type)
!
! ----- LIAISON_DDL
!
        call caliai(vale_type, load)
!
! ----- LIAISON_GROUP
!
        call caliag(vale_type, load)
!
! ----- LIAISON_UNIF
!
        call cagrou(load, mesh, vale_type)
!
! ----- LIAISON_CHAMNO
!
        call calich(load)
!
! ----- LIAISON_MAIL
!
        call calirc(load)
!
    else if (vale_type .eq. 'FONC') then
!
! ----- SOURCE
!
        call cbsour(load, mesh, ligrmo, nb_dim, vale_type)
!
! ----- SOUR_NL
!
        call cbsonl(load, mesh, ligrmo, nb_dim, vale_type)
!
! ----- CONVECTION
!
        call cbconv(load)
!
! ----- FLUX_REP
!
        call cbflux(load, mesh, ligrmo, nb_dim, vale_type)
!
! ----- FLUX_NL
!
        call cbflnl(load, mesh, ligrmo, vale_type)
!
! ----- RAYONNEMENT
!
        call cbrayo(load, mesh, ligrmo, vale_type)
!
! ----- ECHANGE
!
        call cbecha(load, mesh, ligrmo, nb_dim, vale_type)
!
! ----- ECHANGE_PAROI
!
        call caechp(load, ligrch, ligrmo, mesh, vale_type,&
                    nb_dim)
!
! ----- GRADIENT INITIAL
!
        call cbgrai(load, mesh, ligrmo, vale_type)
!
! ----- TEMP_IMPO
!
        keywordfact = 'TEMP_IMPO'
        call caddli(keywordfact, load, mesh, ligrmo, vale_type)
!
! ----- LIAISON_DDL
!
        call caliai(vale_type, load)
!
! ----- LIAISON_GROUP
!
        call caliag(vale_type, load)
!
! ----- LIAISON_UNIF
!
        call cagrou(load, mesh, vale_type)
    else
        ASSERT(.false.)
    endif
!
! - Update loads <LIGREL>
!
    call jeexin(ligrch//'.LGRF', iret)
    if (iret .ne. 0) then
        call adalig(ligrch)
        call cormgi('G', ligrch)
        call jeecra(ligrch//'.LGRF', 'DOCU', cval = 'THER')
        call initel(ligrch)
        call jeveuo(ligrch//'.LGRF', 'E', vk8 = p_ligrch_lgrf)
        p_ligrch_lgrf(2) = model
    endif

! - Audit assignments :
    call verif_affe(modele=model,sd=load)
!
end subroutine
