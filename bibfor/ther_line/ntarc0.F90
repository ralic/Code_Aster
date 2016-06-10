subroutine ntarc0(result, model     , mate     , cara_elem   , list_load_resu,&
                  para  , nume_store, time_curr, sdcrit_nonl_)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rssepa.h"
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
    character(len=8), intent(in) :: result
    integer, intent(in) :: nume_store
    real(kind=8), intent(in) :: time_curr
    real(kind=8), intent(in) :: para(*)
    character(len=19), intent(in) :: list_load_resu
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=19), optional, intent(in) :: sdcrit_nonl_
!
! --------------------------------------------------------------------------------------------------
!
! THER_* - Storing
!
! Storing parameters
!
! --------------------------------------------------------------------------------------------------
!
! IN  RESULT : NOM UTILISATEUR DU CONCEPT RESULTAT
! IN  MODELE : NOM DU MODELE
! IN  MATE   : CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  PARA   : PARAMETRES DU CALCUL
!               (1) THETA
!               (2) DELTAT
! IN  SDCRIT : VALEUR DES CRITERES DE CONVERGENCE
! IN  LISCH2 : NOM DE LA SD INFO CHARGE POUR STOCKAGE DANS LA SD
! IN  NUMARC : NUMERO D'ARCHIVAGE
! IN  INSTAN : VALEUR DE L'INSTANT
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jv_para
    real(kind=8), pointer :: v_crit_crtr(:) => null()
    character(len=16), pointer :: v_crit_crde(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Store time
!
    call rsadpa(result, 'E', 1, 'INST', nume_store, 0, sjv=jv_para)
    zr(jv_para) = time_curr
!
! - Store some parameters
!
    call rsadpa(result, 'E', 1, 'PARM_THETA', nume_store, 0, sjv=jv_para)
    zr(jv_para) = para(1)
    call rsadpa(result, 'E', 1, 'DELTAT'    , nume_store, 0, sjv=jv_para)
    zr(jv_para) = para(2)
!
! - Store non-linear criteria
!
    if (present(sdcrit_nonl_)) then
        call jeveuo(sdcrit_nonl_(1:19)//'.CRTR', 'L', vr   = v_crit_crtr)
        call jeveuo(sdcrit_nonl_(1:19)//'.CRDE', 'L', vk16 = v_crit_crde)
        call rsadpa(result, 'E', 1, v_crit_crde(1), nume_store, 0, sjv=jv_para)
        zi(jv_para) = nint(v_crit_crtr(1))
        call rsadpa(result, 'E', 1, v_crit_crde(2), nume_store, 0, sjv=jv_para)
        zi(jv_para) = nint(v_crit_crtr(2))
        call rsadpa(result, 'E', 1, v_crit_crde(3), nume_store, 0, sjv=jv_para)
        zr(jv_para) = v_crit_crtr(3)
        call rsadpa(result, 'E', 1, v_crit_crde(4), nume_store, 0, sjv=jv_para)
        zr(jv_para) = v_crit_crtr(4)
    endif
!
! - Store others
!
    call rssepa(result, nume_store, model, mate, cara_elem,&
                list_load_resu)
!
    call jedema()
end subroutine
