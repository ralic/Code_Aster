subroutine charme(load, vale_type)
!
    implicit none
!
#include "asterfort/adalig.h"
#include "asterfort/assert.h"
#include "asterfort/caarei.h"
#include "asterfort/cachre.h"
#include "asterfort/caddli.h"
#include "asterfort/caddlp.h"
#include "asterfort/cafaci.h"
#include "asterfort/cafond.h"
#include "asterfort/cafono.h"
#include "asterfort/cafthm.h"
#include "asterfort/cagene.h"
#include "asterfort/cagrou.h"
#include "asterfort/caimch.h"
#include "asterfort/caliag.h"
#include "asterfort/caliai.h"
#include "asterfort/calich.h"
#include "asterfort/calicp.h"
#include "asterfort/caliel.h"
#include "asterfort/calimc.h"
#include "asterfort/caliob.h"
#include "asterfort/calirc.h"
#include "asterfort/caliso.h"
#include "asterfort/calyrc.h"
#include "asterfort/caprec.h"
#include "asterfort/carbe3.h"
#include "asterfort/carota.h"
#include "asterfort/caveas.h"
#include "asterfort/caveis.h"
#include "asterfort/cbchei.h"
#include "asterfort/cbelec.h"
#include "asterfort/cbimpd.h"
#include "asterfort/cblapl.h"
#include "asterfort/cbonde.h"
#include "asterfort/cbondp.h"
#include "asterfort/cbpesa.h"
#include "asterfort/cbprca.h"
#include "asterfort/cbpres.h"
#include "asterfort/cbsint.h"
#include "asterfort/cbvitn.h"
#include "asterfort/char_crea_neum.h"
#include "asterfort/chveno.h"
#include "asterfort/cormgi.h"
#include "asterfort/initel.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/verif_affe.h"
#include "asterfort/utmess.h"
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
! Treatment of loads for AFFE_CHAR_MECA_*
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
! - Mesh, Ligrel for model, dimension of model
!
    command = 'AFFE_CHAR_MECA'
    call cagene(load, command, ligrmo, mesh, nb_dim)
    model = ligrmo(1:8)
    if (nb_dim .gt. 3) then
        call utmess('A', 'CHARGES2_4')
    endif
!
! - Ligrel for loads
!
    ligrch = load//'.CHME.LIGRE'
!
! --------------------------------------------------------------------------------------------------
!
!   Others loadings
!
! --------------------------------------------------------------------------------------------------
!
    if (vale_type .eq. 'REEL') then
!
! ----- LIAISON_INTERF
!
        call calimc(load)
!
! ----- RELA_CINE_BP
!
        call caprec(load, mesh, ligrmo, vale_type)
!
! ----- IMPE_FACE
!
        call cbimpd(load, mesh, ligrmo, vale_type)
!
! ----- VITE_FACE
!
        call cbvitn(load, mesh, ligrmo, vale_type)
!
! ----- ONDE_FLUI
!
        call cbonde(load, mesh, ligrmo, vale_type)
!
! ----- FLUX_THM_REP
!
        call cafthm(load, mesh, ligrmo, vale_type)
!
! ----- FORCE_SOL
!
        call caveis(load)
!
! --------------------------------------------------------------------------------------------------
    else if (vale_type .eq. 'COMP') then
!
! --------------------------------------------------------------------------------------------------
    else if (vale_type .eq. 'FONC') then
!
! ----- IMPE_FACE
!
        call cbimpd(load, mesh, ligrmo, vale_type)
!
! ----- VITE_FACE
!
        call cbvitn(load, mesh, ligrmo, vale_type)
!
! ----- ONDE_PLANE
!
        call cbondp(load, mesh, nb_dim, vale_type)
!
! ----- FLUX_THM_REP
!
        call cafthm(load, mesh, ligrmo, vale_type)
! --------------------------------------------------------------------------------------------------
    else
        ASSERT(.false.)
    endif
!
! --------------------------------------------------------------------------------------------------
!
!   Neumann loadings
!
! --------------------------------------------------------------------------------------------------
!
    if (vale_type .eq. 'REEL') then
!
! ----- PRES_REP/FORCE_TUYAU
!
        call cbpres(load, mesh, ligrmo, nb_dim, vale_type)
!
! ----- PRE_EPSI
!
        call cbchei(load, mesh, ligrmo, vale_type)
!
! ----- PRE_SIGM
!
        call cbsint(load, mesh, ligrmo, vale_type)
!
! ----- EFFE_FOND
!
        call cafond(load, ligrmo, mesh, nb_dim, vale_type)
!
! ----- EVOL_CHAR
!
        call cbprca('MECANIQUE', load)
!
! ----- PESANTEUR
!
        call cbpesa(load, mesh, nb_dim)
!
! ----- ROTATION
!
        call carota(load, mesh, vale_type)
!
! ----- FORCE_ELEC
!
        call cbelec(load, ligrmo, mesh)
!
! ----- INTE_ELEC
!
        call cblapl(load, ligrmo, mesh)
!
! ----- VECT_ASSE
!
        call caveas(load)
!
! ----- FORCE_NODALE
!
        call cafono(load, ligrch, mesh, ligrmo, vale_type)
!
! ----- FORCE_CONTOUR/FORCE_INTERNE/FORCE_ARETE/FORCE_FACE/FORCE_POUTRE/FORCE_COQUE
!
        call char_crea_neum(load, ligrmo, mesh, nb_dim, vale_type)
!
! --------------------------------------------------------------------------------------------------
    else if (vale_type .eq. 'COMP') then
!
! ----- FORCE_CONTOUR/FORCE_INTERNE/FORCE_ARETE/FORCE_FACE/FORCE_POUTRE/FORCE_COQUE
!
        call char_crea_neum(load, ligrmo, mesh, nb_dim, vale_type)
!
! --------------------------------------------------------------------------------------------------
    else if (vale_type .eq. 'FONC') then
!
! ----- PRES_REP/FORCE_TUYAU
!
        call cbpres(load, mesh, ligrmo, nb_dim, vale_type)
!
! ----- PRE_EPSI
!
        call cbchei(load, mesh, ligrmo, vale_type)
!
! ----- PRE_SIGM
!
        call cbsint(load, mesh, ligrmo, vale_type)
!
! ----- EFFE_FOND
!
        call cafond(load, ligrmo, mesh, nb_dim, vale_type)
!
! ----- FORCE_NODALE
!
        call cafono(load, ligrch, mesh, ligrmo, vale_type)
!
! ----- FORCE_CONTOUR/FORCE_INTERNE/FORCE_ARETE/FORCE_FACE/FORCE_POUTRE/FORCE_COQUE
!
        call char_crea_neum(load, ligrmo, mesh, nb_dim, vale_type)
! --------------------------------------------------------------------------------------------------
    else
        ASSERT(.false.)
    endif
!
! --------------------------------------------------------------------------------------------------
!
!   Kinematic conditions
!
! --------------------------------------------------------------------------------------------------
!
    if (vale_type .eq. 'REEL') then
!
! ----- DDL_POUTRE
!
        call caddlp(load, mesh, ligrmo, vale_type)
!
! ----- DDL_IMPO
!
        keywordfact = 'DDL_IMPO'
        call caddli(keywordfact, load, mesh, ligrmo, vale_type)
!
! ----- ARETE_IMPO
!
        call caarei(load, mesh, ligrmo, vale_type)
!
! ----- FACE_IMPO
!
        call cafaci(load, mesh, ligrmo, vale_type)
!
! ----- LIAISON_DDL
!
        call caliai(vale_type, load)
!
! ----- LIAISON_MAIL
!
        call calirc(load)
!
! ----- LIAISON_CYCL
!
        call calyrc(load)
!
! ----- LIAISON_ELEM
!
        call caliel(vale_type, load)
!
! ----- LIAISON_CHAMNO
!
        call calich(load)
!
! ----- CHAMNO_IMPO
!
        call caimch(load)
!
! ----- LIAISON_RBE3
!
        call carbe3(load)
!
! ----- LIAISON_OBLIQUE
!
        call caliob(load, mesh, ligrmo, vale_type)
!
! ----- LIAISON_GROUP
!
        call caliag(vale_type, load)
!
! ----- LIAISON_UNIF
!
        call cagrou(load, mesh, vale_type)
!
! ----- LIAISON_SOLIDE
!
        call caliso(load, mesh, ligrmo, vale_type)
!
! ----- LIAISON_COQUE
!
        call calicp(load, mesh, ligrmo, vale_type)
! --------------------------------------------------------------------------------------------------
    else if (vale_type .eq. 'COMP') then
!
! ----- DDL_IMPO
!
        keywordfact = 'DDL_IMPO'
        call caddli(keywordfact, load, mesh, ligrmo, vale_type)
!
! ----- LIAISON_DDL
!
        call caliai(vale_type, load)
! --------------------------------------------------------------------------------------------------
    else if (vale_type .eq. 'FONC') then
!
! ----- DDL_IMPO
!
        keywordfact = 'DDL_IMPO'
        call caddli(keywordfact, load, mesh, ligrmo, vale_type)
!
! ----- FACE_IMPO
!
        call cafaci(load, mesh, ligrmo, vale_type)
!
! ----- LIAISON_DDL
!
        call caliai(vale_type, load)
!
! ----- LIAISON_OBLIQUE
!
        call caliob(load, mesh, ligrmo, vale_type)
!
! ----- LIAISON_GROUP
!
        call caliag(vale_type, load)
!
! ----- LIAISON_UNIF
!
        call cagrou(load, mesh, vale_type)
!
! ----- LIAISON_SOLIDE
!
        call caliso(load, mesh, ligrmo, vale_type)
!
! ----- LIAISON_COQUE
!
        call calicp(load, mesh, ligrmo, vale_type)
! --------------------------------------------------------------------------------------------------
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
        call jeecra(ligrch//'.LGRF', 'DOCU', cval = 'MECA')
        call initel(ligrch)
        call jeveuo(ligrch//'.LGRF', 'E', vk8 = p_ligrch_lgrf)
        p_ligrch_lgrf(2) = model
    endif
!
! - Check mesh orientation (normals)
!
    if (vale_type .ne. 'COMP') then
        call chveno(vale_type, mesh, model)
    endif

! - Audit assignments :
    call verif_affe(modele=model,sd=load)
!
end subroutine
