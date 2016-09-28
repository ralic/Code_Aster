subroutine nmcoup(fami, kpg, ksp, ndim, typmod,&
                  imat, compor, mult_comp, lcpdb, carcri, timed,&
                  timef, neps, epsdt, depst, nsig,&
                  sigd, vind, option, angmas, nwkin,&
                  wkin, sigf, vinf, ndsde, dsde,&
                  nwkout, wkout, iret)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/kit_glrc_dm_vmis.h"
#include "asterfort/lcumfe.h"
#include "asterfort/lcumfp.h"
#include "asterfort/nmcpla.h"
#include "asterfort/utmess.h"
#include "asterfort/lc0065.h"
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
! aslint: disable=W1504
!
    integer :: imat, ndim, kpg, ksp, iret
    integer :: neps, nsig, nwkin, nwkout, ndsde
    character(len=16), intent(in) :: compor(*)
    character(len=16), intent(in) :: mult_comp
    real(kind=8), intent(in) :: carcri(*)
    real(kind=8) :: timed, timef
    real(kind=8) :: wkin(*), wkout(*)
    real(kind=8) :: epsdt(*), depst(*)
    real(kind=8) :: sigd(6), sigf(6)
    real(kind=8) :: vind(*), vinf(*), angmas(*)
    real(kind=8) :: dsde(*)
    aster_logical :: lcpdb
    character(len=16) :: option
    character(len=*) :: fami
    character(len=8) :: typmod(*)
!
! --------------------------------------------------------------------------------------------------
!
! Comportment management
!
! KIT_DDI
!
! --------------------------------------------------------------------------------------------------
!
!       IN      KPG,KSP  NUMERO DU (SOUS)POINT DE GAUSS
!               NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!               TYPMOD  TYPE DE MODELISATION
!               IMAT    ADRESSE DU MATERIAU CODE
!               COMP    COMPORTEMENT DE L ELEMENT
!                       COMP(1) = RELATION DE COMPORTEMENT
!                       COMP(2) = NB DE VARIABLES INTERNES
!                       COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
!               OPT     OPTION DE CALCUL A FAIRE
!                               'RIGI_MECA_TANG'> DSDE(T)
!                               'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
!                               'RAPH_MECA'     > SIG(T+DT)
!               CRIT    CRITERES  LOCAUX
!                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                                 (ITER_INTE_MAXI == ITECREL)
!                       CRIT(2) = TYPE DE JACOBIEN A T+DT
!                                 (TYPE_MATR_COMP == MACOMP)
!                                 0 = EN VITESSE     > SYMETRIQUE
!                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
!                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                                 (RESI_INTE_RELA == RESCREL)
!                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                                 REDECOUPAGE LOCAL DU PAS DES
!                                 (ITER_INTE_PAS == ITEDEC)
!                                 0 = PAS DE REDECOUPAGE
!                                 N = NOMBRE DE PALIERS
!               WKIN  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
!                       FIXEE EN DUR)
!               TIMED   INSTANT T
!               TIMEF   INSTANT T+DT
!               EPSDT   DEFORMATION TOTALE A T
!               DEPST   INCREMENT DE DEFORMATION TOTALE
!               SIGD    CONTRAINTE A T
!               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!       OUT     SIGF    CONTRAINTE A T+DT
!               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!               IRET    CODE RETOUR DE L'INTEGRATION INTEGRATION DU
!                       COUPLAGE FLUAGE/FISSURATION
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ABSENCE DE CONVERGENCE
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: rela_flua, rela_plas
    character(len=16) :: texte(2)
!
! --------------------------------------------------------------------------------------------------
!
    rela_flua = compor(8)
    rela_plas = compor(9)
!
    if (rela_flua(1:10) .eq. 'GRANGER_FP') then
        if (rela_plas      .eq. 'ELAS'           .or.&
            rela_plas(1:9) .eq. 'VMIS_ISOT'      .or.&
            rela_plas      .eq. 'ROUSS_PR'       .or.&
            rela_plas      .eq. 'BETON_DOUBLE_DP') then
            call nmcpla(fami, kpg   , ksp  , ndim  , typmod,&
                        imat, compor, mult_comp, carcri , timed , timef ,&
                        neps, epsdt , depst, nsig  , sigd  ,&
                        vind, option, nwkin, wkin  , sigf  ,&
                        vinf, ndsde , dsde , nwkout, wkout ,&
                        iret)
            if (iret .eq. 1) then
                goto 999
            endif
        else 
            call utmess('F', 'COMPOR3_2', sk=rela_plas)
        endif
    else if (rela_flua.eq.'BETON_UMLV_FP') then
        if (rela_plas      .eq. 'ENDO_ISOT_BETON' .or.&
            rela_plas(1:6) .eq. 'MAZARS') then
            if (rela_plas(1:15) .eq. 'ENDO_ISOT_BETON') then
                if ((typmod(1).eq.'C_PLAN') .and. (.not.lcpdb)) then
                    call utmess('F', 'ALGORITH7_5')
                endif
            else
                if ((typmod(1).eq.'C_PLAN') .and. lcpdb) then
                    call utmess('F', 'ALGORITH7_4')
                endif
            endif
            if (typmod(2) .eq. 'GRADEPSI') then
                call lcumfe(fami, kpg  , ksp   , ndim     , typmod,&
                            imat, timed, timef , epsdt    , depst ,&
                            sigd, vind , option, rela_plas, sigf  ,&
                            vinf, dsde , wkin)
            else if (typmod(2) .eq. 'GRADVARI') then
                texte(1)=typmod(2)
                texte(2)=rela_plas
                call utmess('F', 'COMPOR3_49', nk=2, valk=texte)
            else
                call lcumfp(fami , kpg   , ksp  , ndim  , typmod   ,&
                            imat , compor, timed, timef , epsdt    ,&
                            depst, sigd  , vind , option, rela_plas,&
                            sigf , vinf  , dsde , carcri)
            endif
        else
            call utmess('F', 'COMPOR3_3', sk=rela_plas)
        endif
    else if (rela_flua(1:4).eq.'GLRC') then
        if (rela_plas .eq. 'VMIS_ISOT_TRAC' .or.&
            rela_plas .eq. 'VMIS_ISOT_LINE' .or.&
            rela_plas .eq. 'VMIS_CINE_LINE') then
            call kit_glrc_dm_vmis(imat  , rela_plas, epsdt, depst, vind,&
                                  option, sigd     , sigf , vinf , dsde,&
                                  carcri  , iret)
        else
            call utmess('F', 'COMPOR3_4', sk=rela_plas)
        endif
    else if ((rela_flua(1:15).eq.'FLUA_PORO_BETON').or.&
             (rela_plas(1:15).eq.'FLUA_PORO_BETON')) then
! ----- For "KIT_RGI"
        if ((rela_plas(1:15) .eq. 'ENDO_PORO_BETON') .or.&
            (rela_flua(1:15) .eq. 'ENDO_PORO_BETON')) then
            call lc0065(fami  , kpg , ksp  , ndim  , imat  ,&
                        compor, carcri, timed, timef , epsdt ,&
                        depst , sigd, vind , option, angmas,&
                        sigf  , vinf, wkin , typmod, 1     ,&
                        155   , dsde, iret)
        else
            call utmess('F', 'COMPOR3_7')
        endif
    else
        call utmess('F', 'COMPOR3_6', sk=rela_flua)
    endif
!
999 continue
end subroutine
