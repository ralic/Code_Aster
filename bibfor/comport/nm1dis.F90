subroutine nm1dis(fami, kpg, ksp, imate, em,&
                  ep, sigm, deps, vim, option,&
                  compor, materi, sigp, vip, dsde)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ----------------------------------------------------------------------
!
!          PLASTICITE VON MISES ISOTROPE BILINEAIRE MONODIM
!
! ----------------------------------------------------------------------
!
!   em      : module d'Young à t-
!   ep      : module d'Young à t+
!   sigm    : contrainte à t-
!   vim     : variables internes à t-
!   deps    : déformation totale plus - déformation moins - incrément déformation thermique
!   sigp    : contraintes à t+
!   vip     : variables internes à t+
!   epsp    : deformation  plastique plus
!   dsde    : dsig/deps
!
! ----------------------------------------------------------------------
!
    implicit none
#include "asterfort/assert.h"
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
#include "asterfort/rctype.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
! --------------------------------------------------------------------------------------------------
    integer :: kpg, ksp, imate
    real(kind=8) :: em, ep, et, sigy
    real(kind=8) :: sigm, deps, pm, vim(*), vip(*), para_vale
    real(kind=8) :: sigp, dsde
    character(len=16) :: option, compor(*)
    character(len=*) :: fami, materi
! --------------------------------------------------------------------------------------------------
    integer :: jprolm, jvalem, nbvalm, nbvalp, jprolp, jvalep, iret
    integer :: icodre(2)
    real(kind=8) :: rprim, rm, sige, valpar, valres(2), airerp, sieleq, rp, dp, nu, asige
    character(len=8) :: nompar,para_type
    character(len=16) :: nomecl(2)
    data nomecl /'D_SIGM_EPSI','SY'/
! --------------------------------------------------------------------------------------------------
!
    nompar = 'TEMP'
    pm = vim(1)
!
    et = 0.0d0
!   caractéristiques écrouissage linéaire
    if ((compor(1).eq.'VMIS_ISOT_LINE') .or. (compor(1).eq.'GRILLE_ISOT_LINE')) then
        call rcvalb(fami, kpg, ksp, '+', imate, materi, 'ECRO_LINE', 0, ' ', [0.d0],&
                    1, nomecl, valres, icodre, 1)
        call rcvalb(fami, kpg, ksp, '+', imate, materi, 'ECRO_LINE', 0, ' ', [0.d0],&
                    1, nomecl(2), valres(2), icodre(2), 0)
        if (icodre(2) .ne. 0) valres(2) = 0.d0
        et = valres(1)
        sigy = valres(2)
        rprim = ep*et/ (ep-et)
        rm = rprim*vim(1) + sigy
!
!   caractéristiques écrouissage donné par courbe de traction
    else if (compor(1).eq.'VMIS_ISOT_TRAC') then
        call rcvarc(' ', 'TEMP', '-', fami, kpg, ksp, valpar, iret)
        call rctype(imate, 1, nompar, [valpar], para_vale, para_type, materi=materi)
        if ((para_type.eq.'TEMP') .and. (iret.eq.1)) then
            call utmess('F', 'COMPOR5_5', sk = para_type)
        endif
        call rctrac(imate, 1, 'SIGM', para_vale, jprolm, jvalem, nbvalm, em, materi=materi)
        call rcvarc(' ', 'TEMP', '+', fami, kpg, ksp, valpar, iret)
        call rctype(imate, 1, nompar, [valpar], para_vale, para_type,materi=materi)
        if ((para_type.eq.'TEMP') .and. (iret.eq.1)) then
            call utmess('F', 'COMPOR5_5', sk = para_type)
        endif
        call rctrac(imate, 1, 'SIGM', para_vale, jprolp, jvalep, nbvalp, ep, materi=materi)
        call rcfonc('S', 1, jprolp, jvalep, nbvalp, sigy = sigy)
        call rcfonc('V', 1, jprolp, jvalep, nbvalp, p=vim(1), rp=rm, rprim=rprim, airerp=airerp)
        et=rprim
    else
        ASSERT( .FALSE. )
    endif
!
!   estimation élastique
    sige = ep* (sigm/em+deps)
    sieleq = abs(sige)
!
!   calcul epsp, p , sig
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
                call rcfonc('E', 1, jprolp, jvalep, nbvalp, e=ep, nu=nu, p=vim(1), rp=rp,&
                            rprim=rprim, airerp=airerp, sieleq=asige, dp=dp)
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
