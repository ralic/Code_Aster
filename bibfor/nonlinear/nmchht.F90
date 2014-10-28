subroutine nmchht(model    , nume_ddl, mate       , compor     , cara_elem  ,&
                  list_load, carcri  , varc_refe  , acti_func  , sdstat     ,&
                  sddyna   , sdtime  , sdcont_defi, sdcont_solv, sdunil_solv,&
                  hval_incr, sddisc  , parcon     , hval_algo  , hval_veasse,&
                  sdnume   , result)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/diinst.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecact.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmaint.h"
#include "asterfort/nmassv.h"
#include "asterfort/nmcalv.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmfint.h"
#include "asterfort/nmvcaf.h"
#include "asterfort/nmvcex.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rs_getlast.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
!
    integer, intent(in) :: acti_func(*)
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sdnume
    character(len=19), intent(in) :: list_load
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: nume_ddl
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: carcri
    character(len=24), intent(in) :: varc_refe
    character(len=24), intent(in) :: sdtime
    character(len=24), intent(in) :: sdstat
    character(len=19), intent(in) :: sddisc
    real(kind=8), intent(in) :: parcon(*)
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
    character(len=24), intent(in) :: sdunil_solv
    character(len=19), intent(in) :: hval_veasse(*)
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    character(len=8), intent(in) :: result
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL CHARGEMENT INITIAL POUR SCHEMAS MULTIPAS EN POURSUITE
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact definition datastructure for solving
! In  sdunil_defi      : name of unilateral condition datastructure (from DEFI_CONTACT)
! In  result           : name of result datastructure (EVOL_NOLI)
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  RESOCU : SD POUR LA RESOLUTION LIAISON_UNILATER
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: londe, llapl, ldidi, lreuse, lviss, lsstf
    character(len=8) :: k8bid
    character(len=16) :: k16bla
    character(len=19) :: matass
    character(len=19) :: vefint, vedido
    character(len=19) :: vefedo, veondp, vedidi, velapl, vesstf
    character(len=19) :: cnfedo, cndidi, cnfint
    character(len=19) :: cndido, cncine, cnviss
    character(len=19) :: cnondp, cnlapl, cnsstf
    character(len=24) :: codere
    character(len=19) :: varc_prev, varc_curr, insmoi, insplu
    real(kind=8) :: time_curr, time_prev
    integer :: iterat, ldccvg, nume_last, jinst
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    matass = ' '
    k8bid  = ' '
    k16bla = ' '
    iterat = 0
    codere = '&&NMCHHT.CODERE'
!
! - Active functionnalities
!
    londe  = ndynlo(sddyna,'ONDE_PLANE')
    lviss  = ndynlo(sddyna,'VECT_ISS')
    lsstf  = isfonc(acti_func,'SOUS_STRUC')
    llapl  = isfonc(acti_func,'LAPLACE')
    ldidi  = isfonc(acti_func,'DIDI')
    lreuse = isfonc(acti_func,'REUSE')
!
! - Get time
!
    time_prev = 0.d0
    time_prev = ndynre(sddyna,'INST_PREC')
    if (lreuse) then
        call rs_getlast(result, nume_last)
        call rsadpa(result, 'L', 1, 'INST_PREC', nume_last,&
                    0, sjv=jinst)
        time_prev = zr(jinst)
    endif
    time_curr = diinst(sddisc,0)
!
! - Create <CARTE> for time
!
    call nmchex(hval_incr, 'VALINC', 'COMMOI', varc_prev)
    call nmchex(hval_incr, 'VALINC', 'COMPLU', varc_curr)
    call nmvcex('INST', varc_prev, insmoi)
    call nmvcaf('INST', insmoi, .true._1, varc_curr)
    call nmvcex('INST', varc_curr, insplu)

    call mecact('V', insmoi, 'MODELE', model(1:8)//'.MODELE', 'INST_R',&
                ncmp=1, nomcmp='INST', sr=time_prev)
    call mecact('V', insplu, 'MODELE', model(1:8)//'.MODELE', 'INST_R',&
                ncmp=1, nomcmp='INST', sr=time_curr)
!
! - Get fields from hat-variables - 
!
    call ndynkk(sddyna, 'OLDP_VEFEDO', vefedo)
    call ndynkk(sddyna, 'OLDP_VEDIDO', vedido)
    call ndynkk(sddyna, 'OLDP_VEDIDI', vedidi)
    call ndynkk(sddyna, 'OLDP_VEFINT', vefint)
    call ndynkk(sddyna, 'OLDP_VEONDP', veondp)
    call ndynkk(sddyna, 'OLDP_VELAPL', velapl)
    call ndynkk(sddyna, 'OLDP_VESSTF', vesstf)
    call ndynkk(sddyna, 'OLDP_CNFEDO', cnfedo)
    call ndynkk(sddyna, 'OLDP_CNDIDO', cndido)
    call ndynkk(sddyna, 'OLDP_CNDIDI', cndidi)
    call ndynkk(sddyna, 'OLDP_CNFINT', cnfint)
    call ndynkk(sddyna, 'OLDP_CNONDP', cnondp)
    call ndynkk(sddyna, 'OLDP_CNLAPL', cnlapl)
    call ndynkk(sddyna, 'OLDP_CNCINE', cncine)
    call ndynkk(sddyna, 'OLDP_CNVISS', cnviss)
    call ndynkk(sddyna, 'OLDP_CNSSTF', cnsstf)
!
! - Internal forces
!
    call nmfint(model , mate     , cara_elem, varc_refe, compor,&
                carcri, acti_func, iterat   , sddyna   , sdstat,&
                sdtime, hval_incr, hval_algo, ldccvg   , codere,&
                vefint)
    call nmaint(nume_ddl, acti_func, sdcont_defi, hval_veasse, vefint,&
                cnfint  , sdnume)
!
! - Given displacements
!
    call nmcalv('CNDIDO', model    , list_load, mate     , cara_elem,&
                compor  , carcri   , nume_ddl , varc_refe, sdtime   ,&
                parcon  , time_prev, time_curr, hval_incr, hval_algo,&
                sddyna  , k16bla   , vedido)
    call nmassv('CNDIDO'   , model   , list_load, mate     , cara_elem  ,&
                compor     , nume_ddl, time_prev, time_curr, sdcont_solv,&
                sdunil_solv, sddyna  , sdtime   , hval_incr, varc_refe  ,&
                matass     , vedido  , cndido)
    if (ldidi) then
        call nmcalv('CNDIDI', model    , list_load, mate     , cara_elem,&
                    compor  , carcri   , nume_ddl , varc_refe, sdtime   ,&
                    parcon  , time_prev, time_curr, hval_incr, hval_algo,&
                    sddyna  , k16bla   , vedidi)
        call nmassv('CNDIDI'   , model   , list_load, mate     , cara_elem  ,&
                    compor     , nume_ddl, time_prev, time_curr, sdcont_solv,&
                    sdunil_solv, sddyna  , sdtime   , hval_incr, varc_refe  ,&
                    matass     , vedidi  , cndidi)
    endif
!
! - Laplace forces
!
    if (llapl) then
        call nmcalv('CNLAPL', model    , list_load, mate     , cara_elem,&
                    compor  , carcri   , nume_ddl , varc_refe, sdtime   ,&
                    parcon  , time_prev, time_curr, hval_incr, hval_algo,&
                    sddyna  , k16bla   , velapl)
        call nmassv('CNLAPL'   , model   , list_load, mate     , cara_elem  ,&
                    compor     , nume_ddl, time_prev, time_curr, sdcont_solv,&
                    sdunil_solv, sddyna  , sdtime   , hval_incr, varc_refe  ,&
                    matass     , velapl  , cnlapl)
    endif
!
! - Plane wave
!
    if (londe) then
        call nmcalv('CNONDP', model    , list_load, mate     , cara_elem,&
                    compor  , carcri   , nume_ddl , varc_refe, sdtime   ,&
                    parcon  , time_prev, time_curr, hval_incr, hval_algo,&
                    sddyna  , k16bla   , veondp)
        call nmassv('CNONDP'   , model   , list_load, mate     , cara_elem  ,&
                    compor     , nume_ddl, time_prev, time_curr, sdcont_solv,&
                    sdunil_solv, sddyna  , sdtime   , hval_incr, varc_refe  ,&
                    matass     , veondp, cnondp)
    endif
!
! - Substructuring
!
    if (lsstf) then
        call nmcalv('CNSSTF', model    , list_load, mate     , cara_elem,&
                    compor  , carcri   , nume_ddl , varc_refe, sdtime   ,&
                    parcon  , time_prev, time_curr, hval_incr, hval_algo,&
                    sddyna, k16bla, vesstf)
        call nmassv('CNSSTF'   , model   , list_load, mate     , cara_elem  ,&
                    compor     , nume_ddl, time_prev, time_curr, sdcont_solv,&
                    sdunil_solv, sddyna  , sdtime   , hval_incr, varc_refe  ,&
                    matass     , vesstf  , cnsstf)
    endif
!
! - FORCE_SOL
!
    if (lviss) then
        call nmassv('CNVISS'   , model   , list_load, mate     , cara_elem  ,&
                    compor     , nume_ddl, time_prev, time_curr, sdcont_solv,&
                    sdunil_solv, sddyna  , sdtime   , hval_incr, varc_refe  ,&
                    matass     , k16bla  , cnviss)
    endif
!
! - Neumann forces
!
    call nmcalv('CNFEDO', model    , list_load, mate     , cara_elem,&
                compor  , carcri   , nume_ddl , varc_refe, sdtime   ,&
                parcon  , time_prev, time_curr, hval_incr, hval_algo,&
                sddyna  , k16bla   , vefedo)
    call nmassv('CNFEDO'   , model   , list_load, mate     , cara_elem  ,&
                compor     , nume_ddl, time_prev, time_curr, sdcont_solv,&
                sdunil_solv, sddyna  , sdtime   , hval_incr, varc_refe  ,&
                matass     , vefedo  , cnfedo)
!
! - Given displacements (AFFE_CHAR_CINE)
!
    call nmassv('CNCINE'   , model   , list_load, mate     , cara_elem  ,&
                compor     , nume_ddl, time_prev, time_curr, sdcont_solv,&
                sdunil_solv, sddyna  , sdtime   , hval_incr, varc_refe  ,&
                matass     , k16bla  , cncine)
!
    call jedema()
end subroutine
