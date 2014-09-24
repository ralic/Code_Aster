subroutine nmcoup(fami, kpg, ksp, ndim, typmod,&
                  imat, comp, lcpdb, crit, timed,&
                  timef, neps, epsdt, depst, nsig,&
                  sigd, vind, opt, angmas, nwkin,&
                  wkin, sigf, vinf, ndsde, dsde,&
                  nwkout, wkout, iret)
! aslint: disable=W1504
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-michel.proix at edf.fr
!       ----------------------------------------------------------------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/kit_glrc_dm_vmis.h"
#include "asterfort/lcumfe.h"
#include "asterfort/lcumfp.h"
#include "asterfort/nmcpla.h"
#include "asterfort/utmess.h"
#include "asterfort/lc0065.h"
    integer :: imat, ndim, kpg, ksp, iret
    integer :: neps, nsig, nwkin, nwkout, ndsde
!
    real(kind=8) :: crit(*)
    real(kind=8) :: timed, timef
    real(kind=8) :: wkin(*), wkout(*)
    real(kind=8) :: epsdt(*), depst(*)
    real(kind=8) :: sigd(6), sigf(6)
    real(kind=8) :: vind(*), vinf(*), angmas(*)
!
    real(kind=8) :: dsde(*)
    aster_logical :: lcpdb
!
    character(len=16) :: comp(*), opt
    character(len=*) :: fami
    character(len=8) :: typmod(*)
!
!       ----------------------------------------------------------------
!
!       AIGUILLAGE DES LOIS DE COMPORTEMENT COUPLES
!
!       ================================================================
!       ARGUMENTS
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
    character(len=16) :: option(2), cmp1, cmp2, cmp4
    character(len=16) :: texte(2)
!
    cmp1(1:16) = comp(8)
    cmp2(1:16) = comp(9)
    option(1)(1:16) = opt
!
!
    if (cmp1(1:10) .eq. 'GRANGER_FP') then
        if (cmp2(1:5) .eq. 'ELAS ' .or. cmp2(1:9) .eq. 'VMIS_ISOT' .or. cmp2(1:14) .eq.&
            'VMIS_ISOT_LINE' .or. cmp2(1:8) .eq. 'ROUSS_PR' .or. cmp2(1:15) .eq.&
            'BETON_DOUBLE_DP') then
            call nmcpla(fami, kpg, ksp, ndim, typmod,&
                        imat, comp, crit, timed, timef,&
                        neps, epsdt, depst, nsig, sigd,&
                        vind, opt, nwkin, wkin, sigf,&
                        vinf, ndsde, dsde, nwkout, wkout,&
                        iret)
            if (iret .eq. 1) goto 999
        else 
            call utmess('F', 'COMPOR3_2', nk=1, valk=cmp2)
        endif
!
    else if (cmp1(1:13).eq.'BETON_UMLV_FP') then
!
        if (cmp2(1:15) .eq. 'ENDO_ISOT_BETON' .or. cmp2(1:6) .eq. 'MAZARS') then
!
            cmp4(1:16) = typmod(2)
            option(2)(1:16) = cmp2(1:16)
!
            if (cmp2(1:15) .eq. 'ENDO_ISOT_BETON') then
                if ((typmod(1).eq.'C_PLAN') .and. (.not.lcpdb)) then
                    call utmess('F', 'ALGORITH7_5')
                endif
            else
                if ((typmod(1).eq.'C_PLAN') .and. lcpdb) then
                    call utmess('F', 'ALGORITH7_4')
                endif
            endif
!
            if (typmod(2) .eq. 'GRADEPSI') then
                call lcumfe(fami, kpg, ksp, ndim, typmod,&
                            imat, timed, timef, epsdt, depst,&
                            sigd, vind, option, sigf, vinf,&
                            dsde, wkin)
            else if (typmod(2) .eq. 'GRADVARI') then
                texte(1)=cmp4
                texte(2)=cmp2
                call utmess('F', 'COMPOR3_49', nk=2, valk=texte)
            else
                call lcumfp(fami, kpg, ksp, ndim, typmod,&
                            imat, comp, timed, timef, epsdt,&
                            depst, sigd, vind, option, sigf,&
                            vinf, dsde, crit)
            endif
        else
            call utmess('F', 'COMPOR3_3', nk=1, valk=cmp2)
        endif
!
    else if (cmp1(1:4).eq.'GLRC') then
!
!
        if (cmp2 .eq. 'VMIS_ISOT_TRAC' .or. cmp2 .eq. 'VMIS_ISOT_LINE' .or. cmp2 .eq.&
            'VMIS_CINE_LINE') then
!
            option(2)(1:16) = cmp2(1:16)
!
            call kit_glrc_dm_vmis(imat, cmp2, epsdt, depst, vind,&
                                  opt, sigd, sigf, vinf, dsde,&
                                  crit, iret)
!
        else
            call utmess('F', 'COMPOR3_4', nk=1, valk=cmp2)
        endif
!
    else if ((cmp1(1:15).eq.'FLUA_PORO_BETON').or.&
             (cmp2(1:15).eq.'FLUA_PORO_BETON')) then
        if ((cmp2(1:15) .eq. 'ENDO_PORO_BETON') .or. (cmp1(1:15) .eq. 'ENDO_PORO_BETON')) then
!
            call lc0065(fami, kpg, ksp, ndim, imat,&
                        comp, crit, timed, timef, epsdt,&
                        depst, sigd, vind, opt, angmas,&
                        sigf, vinf, wkin, typmod, 1,&
                        155, dsde, iret)
        else
            call utmess('F', 'COMPOR3_7')
        endif
    else
        call utmess('F', 'COMPOR3_6', nk=1, valk=cmp1)
    endif
!
999 continue
end subroutine
