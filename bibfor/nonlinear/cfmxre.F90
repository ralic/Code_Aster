subroutine cfmxre(mesh  , model    , sdstat   , ds_contact , nume_inst,&
                  sddisc, hval_algo, hval_incr, hval_veasse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmve.h"
#include "asterfort/cfresu.h"
#include "asterfort/cnscno.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmmcpt.h"
#include "asterfort/mmmres.h"
#include "asterfort/nmchex.h"
#include "asterfort/xmmres.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24), intent(in) :: sdstat
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: hval_algo(*)
    character(len=19), intent(in) :: hval_veasse(*) 
    character(len=19), intent(in) :: hval_incr(*)
    character(len=8), intent(in) :: model
    integer, intent(in) :: nume_inst
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (POST_TRAITEMENT)
!
! REMPLIR LE CONT_NOEU POUR L'ARCHIVAGE DU CONTACT
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  model            : name of model
! In  mesh             : name of mesh
! In  nume_inst        : index of current step time
! In  sddisc           : datastructure for time discretization
! In  sdstat           : datastructure for statistics
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! In  hval_veasse      : hat-variable for vectors (node fields)
!
!
    integer :: ifm, niv
    aster_logical :: lctcc, lctcd, lxfcm, lexiv, lallv
    character(len=19) :: ddepla, depdel, depplu
    real(kind=8) :: inst(2), instan
    character(len=19) :: prno
    character(len=24) :: nochco
    integer :: jnochc
    character(len=19) :: cnsinr, cnsper, cnoinr
    integer :: ibid, iret
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    lctcd = cfdisl(ds_contact%sdcont_defi,'FORMUL_DISCRETE')
    lxfcm = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
    lexiv = cfdisl(ds_contact%sdcont_defi,'EXIS_VERIF')
    lallv = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
!
! --- NOM DES CHAM_NO
!
    nochco = ds_contact%sdcont_solv(1:14)//'.NOCHCO'
    call jeveuo(nochco, 'L', jnochc)
    cnsinr = zk24(jnochc+1-1)(1:19)
    cnoinr = zk24(jnochc+2-1)(1:19)
    cnsper = zk24(jnochc+3-1)(1:19)
!
! --- TOUT VERIF -> ON SAUTE
!
    if (lallv) then
        goto 50
    endif
!
! --- INSTANT
!
    instan = diinst(sddisc,nume_inst)
    inst(1) = diinst(sddisc,nume_inst)
    inst(2) = inst(1) - diinst(sddisc,nume_inst-1)
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(hval_algo, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(hval_algo, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', depplu)
!
! --- POST-TRAITEMENT
!
    if (lxfcm) then
        call xmmres(depdel, model, hval_veasse, cnsinr)
    else if (lctcc) then
        call mmmres(mesh, inst, ds_contact, depplu,&
                    depdel, sddisc, hval_veasse, cnsinr, cnsper)
    else if (lctcd) then
        call cfresu(mesh, nume_inst, inst, sddisc, ds_contact,&
                    depplu, depdel, ddepla, cnsinr,&
                    cnsper)
    else
        ASSERT(.false.)
    endif
!
! --- DECOMPTE NOMBRE DE LIAISONS
!
    if (lctcc) then
        call mmmcpt(mesh, sdstat, ds_contact, cnsinr)
    endif
!
! --- METHODE VERIF
!
 50 continue
!
    if (lexiv) then
        call cfmmve(mesh, ds_contact, hval_incr, instan)
    endif
!
! --- TRANSFO DU CHAM_NO_S EN CHAM_NO (AVEC UN PROF_CHNO CONSTANT !)
!
    call dismoi('PROF_CHNO', cnoinr, 'CHAM_NO', repk=prno, arret='C',&
                ier=iret)
    call cnscno(cnsinr, prno, 'NON', 'V', cnoinr,&
                'F', ibid)
!
    call jedema()
end subroutine
