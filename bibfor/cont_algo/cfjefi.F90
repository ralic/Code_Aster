subroutine cfjefi(mesh, disp_iter, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/caladu.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfimp1.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: disp_iter
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Compute final gaps
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  disp_iter        : displacement iteration
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: iliai, jdecal, nbddl
    real(kind=8) :: jeuini, jeuold, jeuinc
    real(kind=8) :: jexnew, jexold, jexinc
    aster_logical :: l_pena_cont, l_lagr_frot, l_frot
    character(len=24) :: sdcont_apcoef, sdcont_apddl, sdcont_appoin
    integer :: japcoe, japddl, japptr
    character(len=24) :: sdcont_apcofr
    integer :: japcof
    character(len=24) :: sdcont_jeuite, sdcont_jeux
    integer :: jjeuit, jjeux
    integer :: nbliai, nb_equa, model_ndim
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... CALCUL DES JEUX FINAUX'
    endif
!
! - Get contact parameters
!
    nbliai      = cfdisd(ds_contact%sdcont_solv,'NBLIAI' )
    nb_equa     = cfdisd(ds_contact%sdcont_solv,'NEQ' )
    model_ndim  = cfdisd(ds_contact%sdcont_solv,'NDIM' )
    l_pena_cont = cfdisl(ds_contact%sdcont_defi,'CONT_PENA' )
    l_lagr_frot = cfdisl(ds_contact%sdcont_defi,'FROT_LAGR' )
    l_frot      = cfdisl(ds_contact%sdcont_defi,'FROT_DISCRET')
!
! - Access to contact datastructures
!
    sdcont_appoin = ds_contact%sdcont_solv(1:14)//'.APPOIN'
    sdcont_apddl  = ds_contact%sdcont_solv(1:14)//'.APDDL'
    sdcont_apcoef = ds_contact%sdcont_solv(1:14)//'.APCOEF'
    call jeveuo(sdcont_appoin, 'L', japptr)
    call jeveuo(sdcont_apddl , 'L', japddl)
    call jeveuo(sdcont_apcoef, 'L', japcoe)
    if (l_frot) then
        sdcont_apcofr = ds_contact%sdcont_solv(1:14)//'.APCOFR'
        call jeveuo(sdcont_apcofr, 'L', japcof)
    endif
    sdcont_jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
    sdcont_jeux   = ds_contact%sdcont_solv(1:14)//'.JEUX'
    call jeveuo(sdcont_jeux  , 'L', jjeux)
    call jeveuo(sdcont_jeuite, 'E', jjeuit)
!
! - Access to displacements
!
    call jeveuo(disp_iter(1:19)//'.VALE', 'L', vr=vale)
!
! - Gap update
!
    do iliai = 1, nbliai
        jeuini = zr(jjeux+3*(iliai-1)+1-1)
        if (l_pena_cont) then
            zr(jjeuit+3*(iliai-1)+1-1) = jeuini
        else
            jdecal = zi(japptr+iliai-1)
            nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
            call caladu(nb_equa, nbddl, zr(japcoe+jdecal), zi(japddl+jdecal), vale,&
                        jeuinc)
            jeuold = zr(jjeuit+3*(iliai-1)+1-1)
            zr(jjeuit+3*(iliai-1)+1-1) = jeuold - jeuinc
            if (l_lagr_frot .and. model_ndim .eq. 2) then
                jexold = zr(jjeuit+3*(iliai-1)+2-1)
                call caladu(nb_equa, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), vale,&
                            jexinc)
                jexnew = jexold + jexinc
                zr(jjeuit+3*(iliai-1)+2-1) = jexnew
            endif
        endif
    end do
!
! - Print
!
    if (niv .ge. 2) then
        call cfimp1('FIN', mesh, ds_contact%sdcont_defi, ds_contact%sdcont_solv, ifm)
    endif
!
    call jedema()
!
end subroutine
