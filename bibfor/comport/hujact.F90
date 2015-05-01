subroutine hujact(mater, vind, vinf, vins, sigd,&
                  sigf, negmul, chgmec, indi)
    implicit none
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
!   ------------------------------------------------------------------
!   DEFINITION DU DOMAINE POTENTIEL DES MECANISMES ACTIFS
!   IN  MATER    :  COEFFICIENTS MATERIAU A T+DT
!       VIND     :  VARIABLES INTERNES  A T
!       VINF     :  VARIABLES INTERNES A T+DT
!       VINS     :  VARIABLES INTERNES A T AVANT CREATION DU DOMAINE
!                   POTENTIEL DE MECANISMES
!       SIGD     :  CHAMPS DE CONTRAINTES A T
!       SIGF     :  CHAMPS DE CONTRAINTES A T+DT
!       NEGMUL() = .TRUE. ---> MULTIPLICATEUR PLASTIQUE NEGATIF
!
!   OUT VIND   :  VARIABLES INTERNES MODIFIEES SI CHGMEC = .TRUE.
!       VINF   :  VARIABLES INTERNES MODIFIEES SI NECESSAIRE
!       CHGMEC   = .TRUE. SI MODIFICATION DU DOMAINE POTENTIEL
!                            DES MECANISMES ACTIFS
!   ------------------------------------------------------------------
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/hujcdc.h"
#include "asterfort/hujcic.h"
#include "asterfort/hujcrd.h"
#include "asterfort/hujcri.h"
#include "asterfort/hujdrc.h"
#include "asterfort/hujmed.h"
#include "asterfort/hujmei.h"
#include "asterfort/hujrmo.h"
#include "asterfort/lceqvn.h"
    integer :: ndt, ndi, i, mono, indi(7)
    real(kind=8) :: tole1, sigd(6), sigf(6)
    real(kind=8) :: vind(*), vinf(*), vins(50), vint(50)
    real(kind=8) :: mater(22, 2), un, zero
    real(kind=8) :: psf
    real(kind=8) :: seuil, rd, rf, psm
    aster_logical :: debug, chgmec, negmul(8), miso
    real(kind=8) :: vinm(50), seuilm, c1td, c2td, cmod, deux
! --------------------------------------------------------------------
    common /tdim/   ndt, ndi
    common /meshuj/ debug
! --------------------------------------------------------------------
    parameter     (tole1= 1.d-7)
    parameter     (deux = 2.d0)
    parameter     (un   = 1.d0)
    parameter     (zero = 0.d0)
!
! ====================================================================
! --- CONSTRUCTION DES SURFACES CYCLIQUES PRECEDENTES -----------
! ====================================================================
!
    call lceqvn(50, vind, vinm)
    do 50 i = 1, 3
        if ((vind(5*i+31).ne.zero) .or. (vind(5*i+32).ne.zero)) then
            vinm(4*i+5) = vind(5*i+31)
            vinm(4*i+6) = vind(5*i+32)
            vinm(4*i+7) = vind(5*i+33)
            vinm(4*i+8) = vind(5*i+34)
            vinm(i+4) = vind(5*i+35)
        endif
 50 end do
!
! ===================================================================
! -------------- DETERMINATION DES CRITERES ACTIFS A T+DT -----------
! ===================================================================
    miso = .false.
    do 20 i = 1, 7
        if (indi(i) .eq. 4) miso = .true.
        if (indi(i) .eq. 8) miso = .true.
 20 continue
!
    do 30 i = 1, 50
        vint(i) = vind(i)
 30 continue
!
    do 40 i = 1, 4
! ====================================================================
! ---------------- MECANISME MONOTONE SUPPOS  ACTIF ------------------
! ====================================================================
        if (vind(23+i) .eq. un) then
!
            if (negmul(i)) then
                chgmec = .true.
                negmul(i) = .false.
                if (i .lt. 4) then
                    if (vind(i) .eq. mater(13,2)) then
                        vind(23+i) = zero
                    else
                        if ((vins(4*i+7).ne.zero) .or. (vins(4*i+8) .ne.zero)) then
                            vind(4+i) = vins(4+i)
                            vind(4*i+5) = vins(4*i+5)
                            vind(4*i+6) = vins(4*i+6)
                            vind(4*i+7) = vins(4*i+7)
                            vind(4*i+8) = vins(4*i+8)
                            vind(23+i) = -un
                        else
                            vind(23+i) = -un
                            call hujmed(i, mater, vind, sigd)
                            vind(i+4) = mater(18,2)
                        endif
                    endif
                else
                    if (chgmec .and. (.not.miso)) goto 40
                    if (vind(i) .eq. mater(14,2)) then
                        vind(23+i) = zero
                    else
                        if (vins(22) .ne. zero) then
                            vind(8) = vins(8)
                            vind(21) = vins(21)
                            vind(22) = vins(22)
                            vind(23+i) = -un
                        else
                            call hujrmo(mater, sigd, vind, rd)
                            call hujrmo(mater, sigf, vinf, rf)
                            if ((rd-rf) .ge. r8prem()) then
                                vind(23+i) = -un
                                call hujmei(vind)
                                vind(8) = mater(19,2)
                            else
                                vind(23+i) = zero
                            endif
                        endif
                    endif
                endif
            endif
            goto 40
!
! ==================================================================
! ---------- MECANISME MONOTONE SUPPOS  ELASTIQUE ------------------
! ==================================================================
        else if (vind(23+i).eq.zero) then
!
! ************************
! --- MECANISME DEVIATOIRE
! ************************
            if (i .lt. 4) then
                call hujcrd(i, mater, sigf, vinf, seuil)
                if (seuil .gt. tole1) then
                    chgmec = .true.
                    vind(23+i) = un
                    vind(4*i+5) = zero
                    vind(4*i+6) = zero
                    vind(4*i+7) = zero
                    vind(4*i+8) = zero
                    vind(4+i) = mater(18,2)
                endif
                goto 40
!
! ******************************
! --- MECANISME DE CONSOLIDATION
! ******************************
            else
                if (chgmec .and. (.not.miso)) goto 40
                call hujcri(mater, sigf, vinf, seuil)
                mono = 0
                if (seuil .gt. tole1) then
                    chgmec = .true.
                    vind(27) = un
                    mono = 1
                endif
                if (mono .ne. 1) then
                    call hujrmo(mater, sigd, vind, rd)
                    call hujrmo(mater, sigf, vinf, rf)
                    if ((rd-rf) .ge. r8prem()) then
                        vind(23+i) = -un
                        call hujmei(vind)
                        vind(8) = mater(19,2)
                        chgmec = .true.
                    endif
                endif
                goto 40
            endif
!
! ====================================================================
! ---------- MECANISME MONOTONE SUPPOS  EN DECHARGE ------------------
! ====================================================================
        else if (vind(23+i).eq.-un) then
!
! ***********************************************
! --- VERIFICATION DES MULTIPLICATEURS PLASTIQUES
! ***********************************************
            if (negmul(i+4)) then
                chgmec = .true.
                negmul(i+4) = .false.
                vind(27+i) = zero
                goto 40
            endif
!
!
! *************************************
! --- VERIFICATION DES SEUILS MONOTONES
! *************************************
            if (i .lt. 4) then
                call hujcrd(i, mater, sigf, vinf, seuil)
            else
                if (chgmec .and. (.not.miso)) goto 40
                call hujcri(mater, sigf, vinf, seuil)
            endif
            if (seuil .gt. tole1) then
                chgmec = .true.
                if (i .lt. 4) then
                    vind(27+i) = zero
                    vind(23+i) = un
                    vind(4*i+5) = zero
                    vind(4*i+6) = zero
                    vind(4*i+7) = zero
                    vind(4*i+8) = zero
                    vind(i+4) = mater(18,2)
                else
                    if ((vind(22).eq.un) .and. (vins(22).eq.-un)) then
                        vind(21) = vins(21)
                        vind(22) = vins(22)
                        vind(31) = un
                        vind(8) = vins(8)
                    else
                        vind(31) = zero
                        vind(27) = un
                        vind(21) = zero
                        vind(22) = zero
                    endif
                endif
                goto 40
            endif
!
! ***********************************************************
! --- EMPECHE L INTERSECTION DES CERCLES CYCLIQUE ET MONOTONE
! ***********************************************************
            if ((i.lt.4) .and. (vinf(27+i).eq.un)) then
                c1td = (vinf(4*i+5)-vinf(4+i)*vinf(4*i+7))
                c2td = (vinf(4*i+6)-vinf(4+i)*vinf(4*i+8))
                cmod = sqrt(c1td**deux+c2td**deux/deux)
                if ((cmod+vinf(i+4)-vinf(i))/vinf(i) .gt. tole1) then
                    chgmec = .true.
                    vind(4*i+7) = c1td/cmod
                    vind(4*i+8) = c2td/cmod
                    vind(4*i+5) = vind(4*i+7)*vind(i)
                    vind(4*i+6) = vind(4*i+8)*vind(i)
                    vind(5*i+31) = zero
                    vind(5*i+32) = zero
                    vind(5*i+33) = zero
                    vind(5*i+34) = zero
                    vind(5*i+35) = mater(18,2)
                    vind(i+4) = vinf(i+4)
                    goto 40
                endif
            endif
!
! ****************************************
! --- MECANISME CYCLIQUE SUPPOSE ELASTIQUE
! ****************************************
            if (vind(27+i) .eq. zero) then
                if (abs(vind(4+i)-un) .lt. tole1) goto 40
!
! ------------------------
! --- MECANISME DEVIATOIRE
! ------------------------
                if (i .lt. 4) then
                    call hujcdc(i, mater, sigf, vinf, seuil)
                    if (seuil .gt. tole1) then
                        chgmec = .true.
                        vind(27+i) = un
                        call hujdrc(i, mater, sigf, vinf, psf)
                        if ((vind(5*i+31).ne.zero) .or. (vind(5*i+32) .ne.zero)) then
                            call hujdrc(i, mater, sigf, vinm, psm)
                            call hujcdc(i, mater, sigf, vinm, seuilm)
                            if ((seuilm.gt.tole1) .and. (psm.lt.zero)) then
! --- REPRISE ANCIENNE SURFACE
                                vind(4*i+5) = vind(5*i+31)
                                vind(4*i+6) = vind(5*i+32)
                                vind(4*i+7) = vind(5*i+33)
                                vind(4*i+8) = vind(5*i+34)
                                vind(i+4) = vind(5*i+35)
! --- MISE A ZERO SURFACE SURFACE ANTERIEURE
                                vind(5*i+31) = zero
                                vind(5*i+32) = zero
                                vind(5*i+33) = zero
                                vind(5*i+34) = zero
                                vind(5*i+35) = mater(18,2)
                                goto 40
                            endif
                        else if (psf.ge.zero) then
! --- ENREGISTREMENT SURFACE ACTUELLE
                            vind(5*i+31) = vind(4*i+5)
                            vind(5*i+32) = vind(4*i+6)
                            vind(5*i+33) = vind(4*i+7)
                            vind(5*i+34) = vind(4*i+8)
                            vind(5*i+35) = vind(4+i)
! --- MODIFICATION SURFACE PAR POINT TANGENT OPPOSE
                            vind(4*i+5) = vind(4*i+5)- deux*vind(i+4)* vind(4*i+7)
                            vind(4*i+6) = vind(4*i+6)- deux*vind(i+4)* vind(4*i+8)
                            vind(4*i+7) =-vind(4*i+7)
                            vind(4*i+8) =-vind(4*i+8)
                            goto 40
                        endif
                    endif
                    if (( (vins(4*i+5).ne.vind(4*i+5)) .and. (vins(4*i+ 5).ne.zero) ) .or.&
                        ( (vins(4*i+6).ne.vind(4*i+6)) .and. (vins(4*i+6).ne.zero) )) then
                        call hujdrc(i, mater, sigf, vinf, psf)
!
                        if (psf .gt. zero) then
                            vinf(4*i+5) = vins(4*i+5)
                            vinf(4*i+6) = vins(4*i+6)
                            vinf(4*i+7) = vins(4*i+7)
                            vinf(4*i+8) = vins(4*i+8)
                            vinf(4+i) = vins(4+i)
                            call hujcdc(i, mater, sigf, vinf, seuil)
                            if (seuil .gt. tole1) then
                                chgmec = .true.
                                vind(4*i+5) = vins(4*i+5)
                                vind(4*i+6) = vins(4*i+6)
                                vind(4*i+7) = vins(4*i+7)
                                vind(4*i+8) = vins(4*i+8)
                                vind(4+i) = vins(4+i)
                                vind(27+i) = un
                            endif
                        endif
                        elseif(((vins(4*i+5).ne.vind(4*i+5)).and. (vins(4*&
                    i+5).eq.zero)).or. ((vins(4*i+6).ne.vind(4*i+6))&
                    .and. (vins(4*i+6).eq.zero)))then
!
                        call hujdrc(i, mater, sigf, vinf, psf)
!
                        if (psf .gt. tole1) then
                            vinf(4*i+5) = zero
                            vinf(4*i+6) = zero
                            vinf(4*i+7) = zero
                            vinf(4*i+8) = zero
                            vinf(4+i) = mater(18,2)
                            vinf(23+i) = un
                            vinf(27+i) = zero
                            chgmec = .false.
                        endif
                    else
                        if (vind(4+i) .ne. mater(18,2)) then
                            call hujmed(i, mater, vinf, sigf)
                            vinf(4+i) = mater(18,2)
                        endif
                    endif
                    goto 40
! ------------------------------
! --- MECANISME DE CONSOLIDATION
! ------------------------------
                else
                    if ((chgmec) .and. (.not.miso)) goto 40
                    call hujcic(mater, sigf, vinf, seuil)
                    call hujrmo(mater, sigd, vind, rd)
                    call hujrmo(mater, sigf, vinf, rf)
!
                    if ((vind(22).eq.un) .and. ((rd-rf).ge.r8prem())) then
                        if (seuil .gt. tole1) then
                            chgmec = .true.
                            vind(31) = un
                        else
                            vinf(21) = vins(21)
                            vinf(22) = vins(22)
                            vinf(8) = vins(8)
!
                            if (vins(22) .eq. zero) vinf(27)=zero
                        endif
                        elseif((vind(22).eq.-un).and.((rd-rf).lt.r8prem())&
                    )then
                        if (seuil .gt. tole1) then
                            chgmec = .true.
                            vind(31) = un
                        else
                            vinf(21) = vins(21)
                            vinf(22) = vins(22)
                            vinf(8) = vins(8)
                        endif
                        elseif((vind(22).eq.un).and.((rd-rf).lt.r8prem()))&
                    then
!
                        if (vins(22) .ne. vinf(22)) then
                            vinf(21) = vins(21)
                            vinf(22) = vins(22)
                            vinf(8) = vins(8)
                            seuil = zero
                            if (vinf(22) .ne. zero) then
                                call hujcic(mater, sigf, vinf, seuil)
                            else
                                vinf(27) = zero
                            endif
                            if (seuil .gt. tole1) then
                                chgmec = .true.
                                vind(21) = vins(21)
                                vind(22) = vins(22)
                                vind(8) = vins(8)
                                vind(31) = un
                            endif
                        else
                            call hujmei(vint)
                            vint(23) = vinf(23)
                            vint(8) = mater(19,2)
                            call hujcic(mater, sigf, vint, seuil)
                            if (seuil .gt. tole1) then
                                chgmec = .true.
                                vind(21) = vint(21)
                                vind(22) = vint(22)
                                vind(8) = vint(8)
                                vind(31) = un
                            endif
                        endif
                        elseif ((vind(22).eq.-un).and. ((rd-rf).gt.r8prem(&
                    ))) then
                        if (vins(22) .ne. vinf(22)) then
                            vinf(21) = vins(21)
                            vinf(22) = vins(22)
                            vinf(8) = vins(8)
                            call hujcic(mater, sigf, vinf, seuil)
                            if (seuil .gt. tole1) then
                                chgmec = .true.
                                vind(21) = vins(21)
                                vind(22) = vins(22)
                                vind(8) = vins(8)
                                vind(31) = un
                            endif
                        else
                            call hujmei(vint)
                            vint(23) = vinf(23)
                            vint(8) = mater(19,2)
                            call hujcic(mater, sigf, vint, seuil)
                            if (seuil .gt. tole1) then
                                chgmec = .true.
                                vind(21) = vint(21)
                                vind(22) = vint(22)
                                vind(8) = vint(8)
                                vind(31) = un
                            endif
                        endif
                    endif
                    goto 40
                endif
            endif
        endif
!
 40 continue
!
end subroutine
