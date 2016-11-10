subroutine xmelem(mesh, model, ds_contact, list_func_acti)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/xmele1.h"
#include "asterfort/xmele2.h"
#include "asterfort/xmele3.h"
!
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
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: list_func_acti(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - CREATION CHAM_ELEM)
!
! CREATION DES CHAM_ELEM
!
! ----------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  ds_contact       : datastructure for contact management
!
    integer :: ifm, niv, contac, nfiss
    character(len=19) :: ligrel
    character(len=19) :: xdonco, xindco, xseuco, xmemco, xgliss, xcohes
    character(len=19) :: xindcp, xmemcp, xseucp, xcohep
    aster_logical :: lxthm
    integer, pointer :: nfis(:) => null()
    integer, pointer :: xfem_cont(:) => null()
    integer, parameter :: nfismx = 100
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<XFEM  > CREATION DES CHAM_ELEM'
    endif
    
! --- MODELE HM-XFEM ?

    lxthm=isfonc(list_func_acti,'THM')
    
!
! --- INITIALISATIONS
!
    ligrel = model//'.MODELE'
!
! --- NOMBRE DE FISSURES
!
    call jeveuo(model//'.NFIS', 'L', vi=nfis)
    nfiss = nfis(1)
    if (nfiss .gt. nfismx) then
        call utmess('F', 'XFEM_2', si=nfismx)
    endif
    if (nfiss .le. 0) then
        call utmess('F', 'XFEM_3')
    endif
!
! --- ON VA CHERCHER LE TYPE DE CONTACT: STANDARD OU MORTAR?
!
    call jeveuo(model//'.XFEM_CONT','L',vi=xfem_cont)
    contac = xfem_cont(1)
    xindco = ds_contact%sdcont_solv(1:14)//'.XFIN'
    xmemco = ds_contact%sdcont_solv(1:14)//'.XMEM'
    xindcp = ds_contact%sdcont_solv(1:14)//'.XFIP'
    xmemcp = ds_contact%sdcont_solv(1:14)//'.XMEP'
    xdonco = ds_contact%sdcont_solv(1:14)//'.XFDO'
    xseuco = ds_contact%sdcont_solv(1:14)//'.XFSE'
    xseucp = ds_contact%sdcont_solv(1:14)//'.XFSP'
    xgliss = ds_contact%sdcont_solv(1:14)//'.XFGL'
    xcohes = ds_contact%sdcont_solv(1:14)//'.XCOH'
    xcohep = ds_contact%sdcont_solv(1:14)//'.XCOP'
!
! ---
!
    if (lxthm) then 
       if(contac.eq.3) then
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xindco, 'PINDCOI', 'RIGI_CONT', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xindcp, 'PINDCOI', 'RIGI_CONT', list_func_acti)
       else if(contac.eq.2) then
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xindco, 'PINDCOI', 'RIGI_CONT_M', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xindcp, 'PINDCOI', 'RIGI_CONT_M', list_func_acti)
       endif
!
! --- SI CONTACT CLASSIQUE, CHAMPS COHESIFS COLLOCATION PTS GAUSS
!
       if(contac.eq.3) then
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xcohes, 'PCOHES', 'RIGI_CONT', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xcohep, 'PCOHES', 'XCVBCA', list_func_acti)
!
! --- SI CONTACT MORTAR, CHAMPS ELNO
!
       else if(contac.eq.2) then
           call xmele3(mesh, model, ligrel, nfiss,&
                       xcohes, 'PCOHES', 'RIGI_CONT_M', list_func_acti)
           call xmele3(mesh, model, ligrel, nfiss,&
                       xcohep, 'PCOHES', 'XCVBCA_MORTAR', list_func_acti)
       else
           ASSERT(.false.)
       endif
    else
       if(contac.eq.1.or.contac.eq.3) then
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xindco, 'PINDCOI', 'RIGI_CONT', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xmemco, 'PMEMCON', 'XCVBCA', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xindcp, 'PINDCOI', 'RIGI_CONT', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xmemcp, 'PMEMCON', 'XCVBCA', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xseuco, 'PSEUIL', 'RIGI_CONT', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xseucp, 'PSEUIL', 'XREACL', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xgliss, 'PGLISS', 'XCVBCA', list_func_acti)
       else if(contac.eq.2) then
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xindco, 'PINDCOI', 'RIGI_CONT_M', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xmemco, 'PMEMCON', 'XCVBCA_MORTAR', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xindcp, 'PINDCOI', 'RIGI_CONT_M', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xmemcp, 'PMEMCON', 'XCVBCA_MORTAR', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xseuco, 'PSEUIL', 'RIGI_CONT_M', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xseucp, 'PSEUIL', 'RIGI_CONT_M', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xgliss, 'PGLISS', 'XCVBCA_MORTAR', list_func_acti)
       endif
!
! --- SI CONTACT CLASSIQUE, CHAMPS COHESIFS COLLOCATION PTS GAUSS
!
       if(contac.eq.1.or.contac.eq.3) then
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xcohes, 'PCOHES', 'RIGI_CONT', list_func_acti)
           call xmele1(mesh, model, ds_contact, ligrel, nfiss,&
                       xcohep, 'PCOHES', 'XCVBCA', list_func_acti)
!
! --- SI CONTACT MORTAR, CHAMPS ELNO
!
       else if (contac.eq.2) then
           call xmele3(mesh, model, ligrel, nfiss,&
                       xcohes, 'PCOHES', 'RIGI_CONT_M', list_func_acti)
           call xmele3(mesh, model, ligrel, nfiss,&
                       xcohep, 'PCOHES', 'XCVBCA_MORTAR', list_func_acti)
       else
           ASSERT(.false.)
       endif
    endif
!
! ---
!
    call xmele2(mesh, model, ds_contact, ligrel, nfiss,&
                xdonco)
!
    call jedema()
!
end subroutine
