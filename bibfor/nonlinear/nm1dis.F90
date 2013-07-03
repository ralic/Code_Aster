subroutine nm1dis(fami, kpg, ksp, imate, em,&
                  ep, sigm, deps, vim, option,&
                  compor, materi, sigp, vip, dsde)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!
    implicit none
! ----------------------------------------------------------------------
!          PLASTICITE VON MISES ISOTROPE BILINEAIRE MONODIM
!          ON PEUT AVOIR T0 DIFF TREF
!
!
! IN  T        : TEMPERATURE PLUS
! IN  TM       : TEMPERATURE MOINS
! IN  E        : MODULE D YOUNG
! IN  ET       : PENTE D ECROUISSAGE
! IN  ALPH     : COEF DILAT THERMIQUE
! IN  SY       : LIMITE D ELASTICITE INITIALE
!
! IN  SIGM    : CONTRAINTE AU TEMPS MOINS
!               UTILISE UNIQUEMENT POUR EVALUER DSDEM
! IN  DEPS    : DEFORMATION  TOTALE PLUS - DEFORMATION MOINS
!                    - INCREMENT DEFORMATION THERMIQUE
! IN  EPSPM   : DEFORMATION  PLASTIQUE MOINS
! IN  PM      : DEFORMATION  PLASTIQUE CUMULEE MOINS
!
! OUT SIG     : CONTRAINTES PLUS
! OUT EPSP    : DEFORMATION  PLASTIQUE PLUS
! OUT P       : DEFORMATION  PLASTIQUE CUMULEE PLUS
! OUT DSDE    : DSIG/DEPS
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
#include "asterfort/rctype.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/u2mess.h"
    real(kind=8) :: em, ep, et, sigy
    real(kind=8) :: sigm, deps, pm, vim(*), vip(*), resu
    real(kind=8) :: sigp, dsde, rbid
    character(len=16) :: option, compor(*)
    character(len=*) :: fami, materi
    integer :: kpg, ksp, imate
!     ------------------------------------------------------------------
!     VARIABLES LOCALES
!     ------------------------------------------------------------------
    real(kind=8) :: rprim, rm, sige, valpar, valres(2), airerp, dum
    real(kind=8) :: sieleq, rp, dp, nu, asige
    integer :: jprolm, jvalem, nbvalm, nbvalp, jprolp, jvalep, iret
    integer :: icodre(2)
    character(len=8) :: nompar, nomecl(2), type
    data nomecl /'D_SIGM_E','SY'/
!
!
!
    nompar = 'TEMP'
    pm = vim(1)
!
! --- CARACTERISTIQUES ECROUISSAGE LINEAIRE
!
    if ((compor(1).eq.'VMIS_ISOT_LINE') .or. (compor(1).eq.'GRILLE_ISOT_LINE')) then
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    materi, 'ECRO_LINE', 0, ' ', 0.d0,&
                    1, nomecl, valres, icodre, 1)
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    materi, 'ECRO_LINE', 0, ' ', 0.d0,&
                    1, nomecl(2), valres(2), icodre(2), 0)
        if (icodre(2) .ne. 0) valres(2) = 0.d0
        et = valres(1)
        sigy = valres(2)
        rprim = ep*et/ (ep-et)
        rm = rprim*vim(1) + sigy
!
! --- CARACTERISTIQUES ECROUISSAGE DONNE PAR COURBE DE TRACTION
!
    else if (compor(1).eq.'VMIS_ISOT_TRAC') then
        call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                    ksp, valpar, iret)
        call rctype(imate, 1, nompar, valpar, resu,&
                    type)
        if ((type.eq.'TEMP') .and. (iret.eq.1)) call u2mess('F', 'CALCULEL_31')
        call rctrac(imate, 1, 'SIGM', resu, jprolm,&
                    jvalem, nbvalm, em)
        call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                    ksp, valpar, iret)
        call rctype(imate, 1, nompar, valpar, resu,&
                    type)
        if ((type.eq.'TEMP') .and. (iret.eq.1)) call u2mess('F', 'CALCULEL_31')
        call rctrac(imate, 1, 'SIGM', resu, jprolp,&
                    jvalep, nbvalp, ep)
        call rcfonc('S', 1, jprolp, jvalep, nbvalp,&
                    sigy, dum, dum, dum, dum,&
                    dum, dum, dum, dum)
        call rcfonc('V', 1, jprolp, jvalep, nbvalp,&
                    rbid, rbid, rbid, vim(1), rm,&
                    rprim, airerp, rbid, rbid)
        et=rprim
    endif
!     ------------------------------------------------------------------
!     ESTIMATION ELASTIQUE
!     ------------------------------------------------------------------
    sige = ep* (sigm/em+deps)
    sieleq = abs(sige)
!     ------------------------------------------------------------------
!     CALCUL EPSP, P , SIG
!     ------------------------------------------------------------------
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        if (sieleq .le. rm) then
            dp=0.d0
            sigp = sige
            dsde = ep
            vip(2) = 0.d0
            vip(1) = vim(1)
            sigp = sige
        else
            vip(2) = 1.d0
            if ((compor(1).eq.'VMIS_ISOT_LINE') .or. (compor(1) .eq.'GRILLE_ISOT_LINE')) then
                dp = abs(sige) - rm
                dp = dp/ (rprim+ep)
                rp = sigy + rprim* (pm+dp)
                if (option .eq. 'FULL_MECA_ELAS') then
                    dsde = ep
                else
                    dsde = et
                endif
            else
                nu=0.5d0
                asige=abs(sige)
                call rcfonc('E', 1, jprolp, jvalep, nbvalp,&
                            rbid, ep, nu, vim(1), rp,&
                            rprim, airerp, asige, dp)
                if (option .eq. 'FULL_MECA_ELAS') then
                    dsde = ep
                else
                    dsde = ep*rprim/ (ep+rprim)
                endif
            endif
            vip(1) = vim(1) + dp
            sigp = sige/ (1.d0+ep*dp/rp)
        endif
    endif
    if (option(1:10) .eq. 'RIGI_MECA_') then
        if ((vim(2).lt.0.5d0) .or. (option.eq.'RIGI_MECA_ELAS')) then
            dsde = ep
        else
            dsde = et
        endif
    endif
end subroutine
