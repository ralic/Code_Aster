subroutine nmcrsu(sddisc     , lisins, ds_conv     , ds_algopara, l_implex,&
                  l_cont_disc, solveu, sdcont_defi_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterc/r8vide.h"
#include "asterfort/crsvsi.h"
#include "asterfort/getvis.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedup1.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmcerr.h"
#include "asterfort/nmcrld.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/GetResi.h"
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
    character(len=19) :: sddisc, lisins, solveu
    type(NL_DS_Conv), intent(in) :: ds_conv
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    aster_logical :: l_implex, l_cont_disc
    character(len=24), optional, intent(in) :: sdcont_defi_
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
!
! CREATION SD DISCRETISATION - SUBDIVISION AUTO
!
! ----------------------------------------------------------------------
!
! In  ds_conv          : datastructure for convergence management
! In  ds_algopara      : datastructure for algorithm parameters
! IN  LISINS : SD_LIST_INST OU SD_LISTR8
! In  sddisc           : datastructure for time discretization
! IN  LCTCD  : .TRUE. SI CONTACT DISCRET
! IN  LIMPEX : .TRUE. SI IMPLEX
! IN  SOLVEU : SD SOLVEUR
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
!
! ----------------------------------------------------------------------
!
    character(len=16) :: metlis, modetp
    integer :: iret
    real(kind=8) :: pas_mini_elas, valr
    integer :: nb_adapt, i_adapt
    integer :: iter_glob_maxi, iter_glob_elas
    integer :: ifm, niv, itmx, vali
    aster_logical :: ldeco
    character(len=24) :: sdcont_defi
    real(kind=8) :: resi_glob_maxi, resi_glob_rela, inikry
    character(len=19) :: event_name
    character(len=16) :: typeco, nopara, decoup
    character(len=24) :: lisevr, lisevk, lisesu
    character(len=24) :: lisavr, lisavk, listpr, listpk
    character(len=24) :: tpsevr, tpsevk, tpsesu
    character(len=24) :: tpsavr, tpsavk, tpstpr, tpstpk
    character(len=24) :: tpsext
    integer :: jtpsex
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION SD SUBDIVISION'
    endif
!
! - Get parameters for convergence
!
    call GetResi(ds_conv, type = 'RESI_GLOB_RELA' , user_para_ = resi_glob_rela)
    call GetResi(ds_conv, type = 'RESI_GLOB_MAXI' , user_para_ = resi_glob_maxi)
    iter_glob_maxi = ds_conv%iter_glob_maxi
    iter_glob_elas = ds_conv%iter_glob_elas
!
! - Initializations
!
    if (present(sdcont_defi_)) then
        sdcont_defi = sdcont_defi_
    else
        sdcont_defi = ' '
    endif
    inikry = 0.9d0
    pas_mini_elas = 0.d0
    call utdidt('L', sddisc, 'LIST', 'NADAPT',&
                vali_ = nb_adapt)
    call utdidt('L', sddisc, 'LIST', 'METHODE',&
                valk_ = metlis)
!
! --- NOM SDS DE LA LISINS
!
    lisevr = lisins(1:8)//'.ECHE.EVENR'
    lisevk = lisins(1:8)//'.ECHE.EVENK'
    lisesu = lisins(1:8)//'.ECHE.SUBDR'
    lisavr = lisins(1:8)//'.ADAP.EVENR'
    lisavk = lisins(1:8)//'.ADAP.EVENK'
    listpr = lisins(1:8)//'.ADAP.TPLUR'
    listpk = lisins(1:8)//'.ADAP.TPLUK'
!
! --- NOM SDS DE LA SDDISC
!
    tpsevr = sddisc(1:19)//'.EEVR'
    tpsevk = sddisc(1:19)//'.EEVK'
    tpsesu = sddisc(1:19)//'.ESUR'
    tpsavr = sddisc(1:19)//'.AEVR'
    tpsavk = sddisc(1:19)//'.AEVK'
    tpstpr = sddisc(1:19)//'.ATPR'
    tpstpk = sddisc(1:19)//'.ATPK'
!
! --- LECTURE DE LA LISTE D'INSTANTS
!
    call gettco(lisins, typeco)
!
    if (typeco .eq. 'LISTR8_SDASTER') then
!
! ----- CREATION EVENEMENTS ERREURS: ARRET
!
        call nmcrld(sddisc)
    else if (typeco.eq.'LIST_INST') then
!
! ----- COPIE LOCALE DES OBJETS DE LA LISINS
!
        call jedup1(lisevr, 'V', tpsevr)
        call jedup1(lisevk, 'V', tpsevk)
        call jedup1(lisesu, 'V', tpsesu)
        if (nb_adapt .ne. 0) then
            call jedup1(lisavr, 'V', tpsavr)
            call jedup1(lisavk, 'V', tpsavk)
            call jedup1(listpr, 'V', tpstpr)
            call jedup1(listpk, 'V', tpstpk)
        endif
    endif
!
! --- DECOUPAGE ACTIVE
!
    call utdidt('L', sddisc, 'LIST', 'EXIS_DECOUPE',&
                valk_ = decoup)
    ldeco = decoup.eq.'OUI'
!
! - SI NEWTON/PREDICTION ='DEPL_CALCULE', ALORS ON INTERDIT LA SUBDIVISION
!
    if (ds_algopara%matrix_pred .eq. 'DEPL_CALCULE') then
        if (ldeco) then
            call utmess('F', 'SUBDIVISE_99')
        endif
    endif
!
! - SI ON DOIT DECOUPER - CAPTURE MATRICE SINGULIERE DANS SOLVEUR ET ECHEC DU SOLVEUR ITERATIF
!
    if (ldeco) then
        if (solveu(1:8) .ne. '&&OP0033') then
            call crsvsi(solveu)
        endif
    endif
!
! --- EN GESTION AUTO, AVEC UN CRITERE D'ADAPTATION EN SEUIL SUR
!     NB_ITER_NEWT, ON MET VALE = ITER_GLOB_MAXI/2 SI VALE N'A PAS
!     ETE RENSIGNE DANS DEFI_LIST_INST
!     ON NE CONSIDERE PAS LE CAS DE DE ITER_GLOB_ELAS CAR C'EST ACTIVE
!     (MATRICE SECANTE) QU'EN CAS DE DIFFICULTE
!
    if (metlis .eq. 'AUTO') then
        call getvis('CONVERGENCE', 'ITER_GLOB_MAXI', iocc=1, scal=itmx, nbret=iret)
        do i_adapt = 1, nb_adapt
            call utdidt('L', sddisc, 'ADAP', 'NOM_EVEN', index_ = i_adapt,&
                        valk_ = event_name)
            if (event_name .eq. 'SEUIL_SANS_FORMU') then
                call utdidt('L', sddisc, 'ADAP', 'NOM_PARA', index_ = i_adapt,&
                            valk_ = nopara)
                if (nopara .eq. 'NB_ITER_NEWT') then
                    call utdidt('L', sddisc, 'ADAP', 'VALE', index_ = i_adapt,&
                                vali_ = vali)
                    if (vali .eq. 0) then
                        vali = itmx / 2
                        valr = vali
                        call utdidt('E', sddisc, 'ADAP', 'VALE', index_ = i_adapt,&
                                    valr_ = valr)
                    endif
                endif
            endif
        end do
    endif
!
! --- VERIF COHERENCE AVEC IMPLEX
!
    if (metlis .eq. 'AUTO') then
        do i_adapt = 1, nb_adapt
            call utdidt('L', sddisc, 'ADAP', 'METHODE', index_ = i_adapt,&
                        valk_ = modetp)
            if (modetp .eq. 'IMPLEX') then
                if (.not.l_implex) then
                    call utmess('F', 'MECANONLINE6_4')
                endif
            endif
        end do
    endif
!
! --- CREATION SD STOCKAGE DES INFOS EN COURS DE CALCUL
!
    call nmcerr(sddisc, iter_glob_maxi, iter_glob_elas, pas_mini_elas, resi_glob_maxi,&
                resi_glob_rela, inikry, l_cont_disc   , sdcont_defi)
!
! --- OBJET POUR PROLONGEMENT DECOUPE
!
    tpsext = sddisc(1:19)//'.AEXT'
    call wkvect(tpsext, 'V V R', 3, jtpsex)
    zr(jtpsex-1+1) = r8vide()
    zr(jtpsex-1+2) = r8vide()
    zr(jtpsex-1+3) = r8vide()
!
    call jedema()
!
end subroutine
