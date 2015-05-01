subroutine vrcomp(compor_curr, vari, ligrel_currz, iret, compor_prev,&
                  type_stop)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcom2.h"
#include "asterfort/vrcomp_chck_cmp.h"
#include "asterfort/vrcomp_chck_rela.h"
#include "asterfort/vrcomp_prep.h"
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
    character(len=*), intent(in) :: compor_curr
    character(len=*), intent(in) :: vari
    character(len=*), intent(in) :: ligrel_currz
    integer, intent(out) :: iret
    character(len=*), optional, intent(in) :: compor_prev
    character(len=1), optional, intent(in) :: type_stop
!
! --------------------------------------------------------------------------------------------------
!
! Check compatibility of comportments
!
! Is internal variable field is correct with comportment definition ?
!
! --------------------------------------------------------------------------------------------------
!
! In  vari          : internal variable field
! In  compor_curr   : current comportment
! In  ligrel_curr   : current LIGREL
! In  compor_prev   : previous comportment
!
! --------------------------------------------------------------------------------------------------
!
!
! ------------------------------------------------------------------
! BUT: VERIFIER LA COHERENCE DU CHAMP DE VARIABLES INTERNES "-" AVEC
!      LE COMPORTEMENT CHOISI.
!      ATTENTION : ON MODIFIE PARFOIS VARMOI POUR TENIR COMPTE DES
!      POSSIBLES CHANGEMENTS DE MODELE ET/OU DE COMPORTEMENT
!
!
!      VARMOI EST LE CHAMP DE VARIABLES INTERNES A L'INSTANT "-"
!      COMPOM EST LA CARTE DE COMPORTEMENT DE VARMOI (OU ' ')
!      COMPOP EST LA CARTE DE COMPOPTEMENT A L'INSTANT "+"
!      LIGREP EST LE LIGREL DU MODELE DE L'INSTANT "+"
!
!      - SI COMPOM = ' ' :
!           ON SE CONTENTE DE COMPARER LE NOMBRE DE V.I. DE VARMOI
!           AVEC LE NOMBRE ATTENDU DANS COMPOP.
!      - SI COMPOM /= ' ':
!           ON PEUT ALORS COMPARER LE NOM DES COMPORTEMENTS DES
!           INSTANTS "+" ET "-"
!           ON EXIGE QUE CES NOMS SOIENT IDENTIQUES OU BIEN QUE :
!             "-" : 'ELAS' OU 'SANS'  -> "+" : N'IMPORTE QUOI
!             "+" : 'ELAS' OU 'SANS'  -> "-" : N'IMPORTE QUOI
!
!
! ------------------------------------------------------------------
!     ARGUMENTS:
! COMPOM   IN/JXIN  K19 : CARTE DE COMPOPTEMENT "-"
! COMPOP   IN/JXIN  K19 : CARTE DE COMPOPTEMENT "+"
! COMPOP   EST AUSSI LE NOM DU CHAM_ELEM_S DE DCEL_I PERMETTANT DE
!          DE CONNAITRE LE NOMBRE DE SOUS-POINTS ET LE NOMBRE DE VARI
! VARMOI   IN/JXVAR K19 : SD CHAM_ELEM   (VARI_R) "-"
! TYPE_STOP IN   : COMPORTEMENT SI PROBLEME DETECTE
!                  'A' ALARME
!                  'E' ERREUR
! IRET      OUT  : CODE RETOUR
!                   0 SI PAS DE PROBLEME
!
! REMARQUES :
!  - VARMOI EST PARFOIS MODIFIE POUR ETRE COHERENT AVEC COMPOP
!           ON LE RECREE ALORS SUR LA BASE VOLATILE
!  - ON VERIFIE EGALEMENT LE NOMBRE DES SOUS-POINTS
!
!-----------------------------------------------------------------------
!
!   character(len=24) :: valk(3)
!     ------------------------------------------------------------------
!    integer ::   jdcell
!    integer :: jce2d,  jce2l, jce2k
!    integer :: iad1, iad2, nbma, nbspp, nbspm, ncmpp, ncmpm
!    integer :: ima, kma
!    integer :: iadp, jcoppl, jcoppd,   ip
!    integer :: iadm, jcopml, jcopmd,   im
!    integer :: vali(5), tounul, k, nbpgm, n1
!    character(len=8) :: noma, nomail
!    character(len=16) :: relcop, relcom
! !   , lig19p, lig19m
!    character(len=19) :: compor_prev_r, compor_curr_r
!    character(len=48) :: comp1, comp2
!
!    aster_logical :: modif, exip, exim
!    integer, pointer :: repm(:) => null()
!    integer, pointer :: repp(:) => null()
!    real(kind=8), pointer :: ce2v(:) => null()
!    character(len=16), pointer :: copmv(:) => null()
!    character(len=16), pointer :: coppv(:) => null()
!    integer, pointer :: dcelv(:) => null()
!    character(len=8), pointer :: copmk(:) => null()
!    character(len=8), pointer :: coppk(:) => null()
!
    aster_logical :: l_modif_vari
    character(len=1) :: stop_erre
    character(len=19) :: vari_r
    character(len=19) :: compor_curr_r
    character(len=19) :: compor_prev_r
    character(len=8) :: mesh_1, mesh_2
    character(len=48) :: comp_comb_1
    character(len=48) :: comp_comb_2
    aster_logical :: no_same_pg, no_same_spg
    aster_logical :: no_same_rela, no_same_cmp
    character(len=19) :: ligrel_curr, ligrel_prev
    character(len=19) :: dcel
    character(len=8), pointer :: dcelk(:) => null()
    character(len=8) :: mesh
    integer :: nb_elem
    integer, pointer :: cesd(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
!     -- MODIF : .TRUE. => IL FAUT MODIFIER VARMOI CAR CERTAINES
!        MAILLES ONT DISPARU OU SONT NOUVELLES OU ONT CHANGE DE
!        COMPORTEMENT
    l_modif_vari = .false.
    no_same_cmp = .false.
    no_same_spg = .false.
    no_same_pg = .false.
    no_same_rela = .false.
!
! - Error management
!
    iret = 0
    if (present(type_stop)) then
        stop_erre = type_stop
    else
        stop_erre = 'E'
    endif
!
! - Acces to reduced CARTE DCEL_I (see CESVAR) on current comportement
!
    dcel = compor_curr
    call jeveuo(dcel//'.CESD', 'L', vi=cesd)
    call jeveuo(dcel//'.CESK', 'L', vk8=dcelk)
    mesh = dcelk(1)
    nb_elem = cesd(1)
!
! - LIGREL
!
    ligrel_curr = ligrel_currz
    call dismoi('NOM_LIGREL', vari, 'CHAM_ELEM', repk=ligrel_prev)
!
! - Comportments can been mixed with each other
!
    comp_comb_1 = 'LEMAITRE        VMIS_ISOT_LINE  VMIS_ISOT_TRAC'
!
! - Comportments can been mixed with all other ones
!
    comp_comb_2 = 'ELAS            SANS            KIT_CG'
!
! - Chesk meshes
!
    call dismoi('NOM_MAILLA', compor_curr, 'CHAMP', repk=mesh_1)
    call dismoi('NOM_MAILLA', vari, 'CHAMP', repk=mesh_2)
    if (mesh_1 .ne. mesh_2) then
        call utmess('F', 'COMPOR2_24')
    endif
!
! - Prepare fields
!
    if (present(compor_prev)) then
        call vrcomp_prep(vari, vari_r, compor_curr, compor_curr_r, compor_prev,&
                         compor_prev_r)
    else
        call vrcomp_prep(vari, vari_r, compor_curr, compor_curr_r, ' ',&
                         compor_prev_r)
    endif
!
! - Check if comportments are the same (or compatible)
!
    if (present(compor_prev)) then
        call vrcomp_chck_rela(mesh, nb_elem, compor_curr_r, compor_prev_r, ligrel_curr,&
                              ligrel_prev, comp_comb_1, comp_comb_2, no_same_pg, no_same_rela,&
                              l_modif_vari)
    endif
    if (no_same_pg) then
        iret = 1
        call utmess(stop_erre, 'COMPOR2_28')
    endif
    if (no_same_rela) then
        iret = 1
        call utmess(stop_erre, 'COMPOR2_30')
    endif
!
! - Check if elements have the same number of internal variables and Gauss-subpoints
!
    call vrcomp_chck_cmp(mesh, nb_elem, compor_curr, compor_curr_r, compor_prev_r,&
                         vari_r, comp_comb_2, ligrel_curr, ligrel_prev, no_same_spg,&
                         no_same_cmp, l_modif_vari)
    if (no_same_spg) then
        iret = 1
        call utmess(stop_erre, 'COMPOR2_27')
    endif
    if (no_same_cmp) then
        iret = 1
        call utmess(stop_erre, 'COMPOR2_29')
    endif
!
! - Have to change internal variable field
!
    if (l_modif_vari) then
        call vrcom2(compor_curr, vari, ligrel_curr)
    endif
!
! - Clean
!
    call detrsd('CHAM_ELEM_S', vari_r)
    call detrsd('CHAM_ELEM_S', compor_prev_r)
    call detrsd('CHAM_ELEM_S', compor_curr_r)
!
    call jedema()
!
end subroutine
