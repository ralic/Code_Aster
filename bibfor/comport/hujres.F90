subroutine hujres(fami, kpg, ksp, mod, crit,&
                  mater, imat, nvi, deps, sigd,&
                  vind, sigf, vinf, iret, etatf)
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
!   INTEGRATION PLASTIQUE DE LA LOI DE HUJEUX
!   IN  MOD    :  MODELISATION
!       CRIT   :  CRITERES DE CONVERGENCE
!       MATER  :  COEFFICIENTS MATERIAU A T+DT
!       NVI    :  NOMBRE DE VARIABLES INTERNES
!       DEPS   :  INCREMENT DE DEFORMATION
!       SIGD   :  CONTRAINTE  A T
!       VIND   :  VARIABLES INTERNES  A T
!   VAR SIGF   :  CONTRAINTE A T+DT  (IN -> ELAS, OUT -> PLASTI)
!   OUT VINF   :  VARIABLES INTERNES A T+DT
!                 (CUMUL DECOUPAGE)
!       NDEC   :  NOMBRE DE DECOUPAGE
!       IRET   :  CODE RETOUR DE  L'INTEGRATION DE LA LOI DE HUJEUX
!                    IRET=0 => PAS DE PROBLEME
!                    IRET=1 => ECHEC
!       ETATF  :  ETAT PLASTIQUE OU ELASTIQUE DU POINT DE GAUSS
!   ------------------------------------------------------------------
#include "asterf_types.h"
#include "asterfort/hujact.h"
#include "asterfort/hujmid.h"
#include "asterfort/hujpot.h"
#include "asterfort/hujpre.h"
#include "asterfort/hujprj.h"
#include "asterfort/infniv.h"
#include "asterfort/lceqve.h"
#include "asterfort/lceqvn.h"
#include "asterfort/utmess.h"
    integer :: ndt, ndi, nvi, ndec, iret, kpg, ksp
    integer :: i, k, ifm, niv, nsubd, maj
    integer :: nvimax, idec, imat, indi(7)
    parameter     (nvimax=50)
    real(kind=8) :: deps(6), vins(nvimax)
    real(kind=8) :: sigd(6), sigf(6), predic(6), ptrac
    real(kind=8) :: sigd0(6), deps0(6), predi0(6)
    real(kind=8) :: vind(*), vinf(*), vind0(nvimax)
    real(kind=8) :: mater(22, 2), crit(*)
    real(kind=8) :: pf, qf, vec(3), zero, pref
    real(kind=8) :: deux
    real(kind=8) :: tol, asig, adsig, tole1, rtrac
    aster_logical :: chgmec, noconv, aredec, stopnc, negmul(8)
    aster_logical :: subd, rdctps, loop, impose
    character(len=8) :: mod
    character(len=7) :: etatf
    character(len=*) :: fami
    aster_logical :: debug, plas, try, nodec, tract
!
    common /tdim/   ndt, ndi
    common /meshuj/ debug
!
    data zero, deux   / 0.d0, 2.d0/
    data tol, tole1 / .5d0, 1.d-7 /
!
    if (nvi .gt. nvimax) then
        call utmess('F', 'COMPOR1_1')
    endif
!
    try = .true.
    loop = .false.
    nodec=.false.
!
    do 70 i = 1, 7
        indi(i) = 0
70  continue
!
    ptrac = mater(21,2)
    pref = mater(8,2)
    rtrac = abs(1.d-6*pref)
!
    call infniv(ifm, niv)
!
! ----  SAUVEGARDE DES GRANDEURS D ENTREE INITIALES
    call lceqve(sigf, predi0)
    call lceqve(sigd, sigd0)
    call lceqve(deps, deps0)
    call lceqvn(nvi, vind, vind0)
!
!  ARRET OU NON EN NON CONVERGENCE INTERNE
!  ---------------------------------------
    if (int(crit(1)) .lt. 0) then
        stopnc = .true.
    else
        stopnc = .false.
    endif
!
!
!  INITIALISATION DES VARIABLES DE REDECOUPAGE
!  -------------------------------------------
!  INT( CRIT(5) ) = 0  1 OU -1 ==> PAS DE REDECOUPAGE DU PAS DE TEMPS
    if ((int(crit(5)) .eq. 0) .or. (int(crit(5)) .eq. -1) .or. (int(crit(5)) .eq. 1)) then
        ndec = 1
        nodec = .true.
        aredec = .true.
        noconv = .false.
!
!
! ---- INT( CRIT(5) ) < -1 ==> REDECOUPAGE DU PAS DE TEMPS
!                            SI NON CONVERGENCE
    else if (int(crit(5)) .lt. -1) then
        ndec = 1
        aredec = .false.
        noconv = .false.
!
!
! ---- INT( CRIT(5) ) > 1 ==> REDECOUPAGE IMPOSE DU PAS DE TEMPS
    else if (int(crit(5)) .gt. 1) then
        ndec = int(crit(5))
        aredec = .true.
        noconv = .false.
    endif
!
!
!  POINT DE RETOUR EN CAS DE DECOUPAGE POUR DR/R > TOLE OU
!  APRES UNE NON CONVERGENCE, POUR  INT(CRIT(5)) < -1
!
    subd = .false.
    rdctps = .false.
!
500  continue
    if (noconv) then
        ndec = 3
        iret = 0
        aredec = .true.
        noconv = .false.
    endif
!
    if (subd) then
        ndec = nsubd
        iret = 0
        aredec = .true.
        try = .false.
    endif
!
    if (rdctps) then
        ndec = 3
        iret = 0
        aredec = .true.
    endif
!
!   RESTAURATION DE SIGD VIND DEPS ET PREDIC ELAS SIGF
!   EN TENANT COMPTE DU DECOUPAGE EVENTUEL
    call lceqve(sigd0, sigd)
    call lceqvn(nvi, vind0, vind)
!
    do 10 i = 1, ndt
        deps(i) = deps0(i)/ndec
10  continue
!
    call hujpre(fami, kpg, ksp,'ELASTIC', mod,&
                crit, imat, mater, deps, sigd,&
                sigf, vind, iret)
!
!KH ON IMPOSE UNE CONDITION SUR LA VARIATION DE DSIGMA
!   < 50% de SIGMA_INIT
    asig = 0.d0
    adsig= 0.d0
!
    do 12 i = 1, ndt
        asig = asig + sigd(i)**2.d0
        adsig= adsig+ (sigf(i)-sigd(i))**2.d0
12  end do
    asig = sqrt(asig)
    adsig= sqrt(adsig)
!
    if ((-asig/pref) .gt. tole1) then
        nsubd = nint(adsig/asig/tol)
        if (nsubd .gt. 1) then
            ndec = min(ndec*nsubd,100)
            do 11 i = 1, ndt
                deps(i) = deps0(i)/ndec
11          continue
        endif
    endif
!
!
    if (debug) then
!
        write(ifm,*)
        write(ifm,'(A)') '================== HUJRES =================='
        write(ifm,*)
        write(ifm,1001) 'NDEC=',ndec
!
    endif
!
!               INIT BOUCLE SUR LES DECOUPAGES
!  =============================================================
    do 400 idec = 1, ndec
!
! --- MISE A JOUR DU COMPTEUR D'ITERATIONS LOCALES
        loop = .false.
        vind(35) = vind0(35) + idec
!
        if (debug) then
!
            write(ifm,*)
            write(ifm,1001) '%% IDEC=',idec
!
        endif
!
        maj = 0
        call lceqve(sigf, predic)
!
        do 20 k = 1, 8
            negmul(k)=.false.
20      continue
!
! ---> SAUVEGARDE DES SURFACES DE CHARGE AVANT MODIFICATION
        call lceqvn(nvi, vind, vins)
! ---> DEFINITION DU DOMAINE POTENTIEL DE MECANISMES ACTIFS
!
        call hujpot(mod, mater, vind, deps, sigd,&
                    sigf, etatf, rdctps, iret, aredec)
        if (iret .eq. 1) goto 9999
!
        if (rdctps .and. (.not. aredec)) then
            goto 500
        else if (rdctps .and. (aredec)) then
            iret = 1
            goto 9999
        endif
!
!
! ---> SI ELASTICITE PASSAGE A L'ITERATION SUIVANTE
        plas = .false.
        if (etatf .eq. 'ELASTIC') then
            do 29 i = 1, 3
                call hujprj(i, sigf, vec, pf, qf)
                if (((pf+deux*rtrac-ptrac)/abs(pref)) .ge. zero) then
                    plas = .true.
                    etatf = 'PLASTIC'
                endif
29          continue
!
            if (.not.plas) then
                do 30 i = 1, nvi
                    vinf(i)=vind(i)
30              continue
                chgmec = .false.
                goto 40
            endif
        endif
!
!
! ---> SINON RESOLUTION VIA L'ALGORITHME DE NEWTON
        chgmec = .false.
!
        call lceqvn(nvi, vind, vinf)
        if (debug) write(6,*)'HUJRES - VINF =',(vinf(i),i=24,31)
!
100      continue
!--->   RESOLUTION EN FONCTION DES MECANISMES ACTIVES
!       MECANISMES ISOTROPE ET DEVIATOIRE
!-----------------------------------------------------
!
        call hujmid(mod, crit, mater, nvi, deps,&
                    sigd, sigf, vind, vinf, noconv,&
                    aredec, stopnc, negmul, iret, subd,&
                    loop, nsubd, indi, tract)
!
! --- ON CONTROLE QUE LES PRESSIONS ISOTROPES PAR PLAN
!     NE PRESENTENT PAS DE TRACTION
        do 48 i = 1, ndi
            call hujprj(i, sigf, vec, pf, qf)
            if (((pf+rtrac-ptrac)/abs(pref)) .gt. zero) then
                noconv=.true.
                if (debug) write(6,'(A)')'HUJRES :: SOL EN TRACTION'
            endif
48      continue
! --- SI TRACTION DETECTEE ET NON CONVERGENCE, ON IMPOSE
! --- ETAT DE CONTRAINTES ISOTROPE
        if ((noconv) .and. (tract)) then
            noconv=.false.
            do 51 i = 1, 3
                sigf(i) = -deux*rtrac
                sigf(3+i) = zero
                vind(23+i) = zero
                vind(27+i) = zero
                vind(5+4*i) = zero
                vind(6+4*i) = zero
                vind(7+4*i) = zero
                vind(8+4*i) = zero
51          continue
            call lceqvn(nvi, vind, vinf)
            iret = 0
            if (debug) write(6,'(A)')'HUJRES :: CORRECTION SIGMA'
        endif
! --- SI PROCHE TRACTION ET NON CONVERGENCE, ON IMPOSE
! --- ETAT DE CONTRAINTES ISOTROPE
        if (noconv) then
            impose = .false.
            do 49 i = 1, ndi
                call hujprj(i, sigf, vec, pf, qf)
                if ((abs(pf-ptrac)/abs(pref)) .lt. 1d-5) then
                    impose = .true.
                    if (debug) write(6,'(A)')'HUJRES :: SOL LIQUEFIE'
                endif
49          continue
            if (impose) then
                noconv = .false.
                do 47 i = 1, 3
                    sigf(i) = -deux*rtrac
                    sigf(3+i) = zero
                    vind(23+i) = zero
                    vind(27+i) = zero
                    vind(5+4*i) = zero
                    vind(6+4*i) = zero
                    vind(7+4*i) = zero
                    vind(8+4*i) = zero
47              continue
                call lceqvn(nvi, vind, vinf)
                iret = 0
            endif
        endif
!
! --- PRISE DE DECISION POUR LA SUITE DE L'ALGORITHME
! --- ECHEC DE L'INTEGRATION
        if (iret .eq. 1) goto 9999
!
! --- REDECOUPAGE LOCAL SI POSSIBLE
        if ((noconv .or. subd) .and. (.not. aredec)) goto 500
!
! --- REDECOUPAGE LOCAL SI ENCORE POSSIBLE
        if (subd .and. try .and. nodec) goto 500
!
! --- NON CONVERGENCE D'OU ECHEC D'INTEGRATION LOCAL
        if (noconv) then
            if (debug) write(6,'(A)')'HUJRES :: PROBLEME AVEC NOCONV '
            iret = 1
            goto 9999
        endif
!
! --- REPRISE A CE NIVEAU SI MECANISME SUPPOSE ELASTIQUE
40      continue
!
! --- VERIFICATION DES MECANISMES SUPPOSES ACTIFS
!                 DES MULTIPLICATEURS PLASTIQUES
!                 DES SEUILS EN DECHARGE
!
        call hujact(mater, vind, vinf, vins, sigd,&
                    sigf, negmul, chgmec, indi)
        if (iret .eq. 1) goto 9999
!
! - SI ON MODIFIE LE DOMAINE POTENTIEL DE MECANISMES ACTIVES : RETOUR
!   AVEC UNE CONDITION DE CONVERGENCE DE LA METHODE DE NEWTON
        if (chgmec) then
            if (.not. aredec) then
! --- REDECOUPAGE ACTIVE S'IL NE L'ETAIT PAS ENCORE
                chgmec = .false.
                noconv = .true.
                goto 500
            else
! --- REPRISE DE L'ITERATION EN TENANT COMPTE DES MODIFICATIONS
                if (debug) write(ifm,'(A)') 'HUJRES :: CHANGEMENT DE MECANISME'
                chgmec = .false.
!
! --- REINITIALISATION DE SIGF A LA PREDICTION ELASTIQUE PREDIC
!            CALL LCEQVE (PREDIC, SIGF)
! --- PREDICTEUR MIS A JOUR TENANT COMPTE DE L'ETAT PRECEDEMMENT OBTENU
                maj = maj + 1
                if (maj .lt. 5) then
                    loop = .true.
                    call lceqvn(nvi, vind, vinf)
                    goto 100
                else
                    if (debug) write(6,'(A)') 'HUJRES :: SOLUTION EXPLICITE'
                endif
            endif
        endif
! End - CHGMEC
! --- S'IL N'Y A PAS DE CHANGEMENT DE MECANISME, ON POURSUIT
!
        maj = 0
        if (idec .lt. ndec) then
            call lceqve(sigf, sigd)
            do 50 i = 1, nvi
                vind(i) = vinf(i)
50          continue
            do 60 i = 1, ndt
                deps(i) = deps0(i)/ndec
60          continue
! --- APPLICATION DE L'INCREMENT DE DÉFORMATIONS, SUPPOSE ELASTIQUE
            call hujpre(fami, kpg, ksp, 'ELASTIC', mod,&
                        crit, imat, mater, deps, sigd,&
                        sigf, vind, iret)
        endif
!
400  continue
! End - Boucle sur les redecoupages
!
!
9999  continue
!
    1001 format(a,i3)
    if (debug) write(6,*)'IRET - HUJRES =',iret
end subroutine
