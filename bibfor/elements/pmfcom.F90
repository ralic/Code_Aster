subroutine pmfcom(kpg, debsp, option, compor, crit,&
                  nf, instam, instap, icdmat, nbvalc,&
                  defam, defap, varim, varimp, contm,&
                  defm, ddefp, epsm, modf, sigf,&
                  varip, codret)
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
! aslint: disable=W1504
! --------------------------------------------------------------------------------------------------
!
!        COMPORTEMENT DES ELEMENTS DE POUTRE MULTIFIBRE
!
! --------------------------------------------------------------------------------------------------
!
!   IN
!       kpg     : numero de point de gauss
!       debsp   : numero de sous-point de la premiere fibre du groupe
!       option  :
!       compor  : nom du comportement
!       crit    : criteres de convergence locaux
!       nf      : nombre de fibres du groupe
!       instam  : instant du calcul precedent
!       instap  : instant du calcul
!       icdmat  : code materiau
!       nbvalc  :
!       defam   : deformations anelastiques a l'instant precedent
!       defap   : deformations anelastiques a l'instant du calcul
!       varim   : variables internes moins
!       varimp  : variables internes iteration precedente (pour deborst)
!       contm   : contraintes moins par fibre
!       defm    : deformation  a l'instant du calcul precedent
!       ddefp   : increment de deformation
!       epsm    : deformation a l'instant precedent
!
!   OUT
!       modf    : module tangent des fibres
!       sigf    : contrainte a l'instant actuel des fibres
!       varip   : variables internes a l'instant actuel
!       codret :
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "asterf_types.h"
#include "asterfort/comp1d.h"
#include "asterfort/mazu1d.h"
#include "asterfort/nm1dci.h"
#include "asterfort/nm1dco.h"
#include "asterfort/nm1dis.h"
#include "asterfort/nm1dpm.h"
#include "asterfort/nm1vil.h"
#include "asterfort/paeldt.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/vmci1d.h"
#include "blas/dcopy.h"
!
    integer :: nf, icdmat, nbvalc, kpg, debsp, codret
    real(kind=8) :: contm(nf), defm(nf), ddefp(nf), modf(nf), sigf(nf)
    real(kind=8) :: varimp(nbvalc*nf), varip(nbvalc*nf), varim(nbvalc*nf)
    real(kind=8) :: instam, instap, epsm
    real(kind=8) :: crit(*), defap(*), defam(*)
!
    character(len=16) :: option
    character(len=24) :: compor(*)
!
! --------------------------------------------------------------------------------------------------
!
    integer , parameter :: nbval=12
    integer :: icodre(nbval)
    real(kind=8) :: valres(nbval)

    integer :: nbvari, codrep, ksp, i, ivari, iret1
    real(kind=8) :: ep, em, depsth, epsth, tref, tempm, tempp, sigx, epsx, depsx
    real(kind=8) :: cstpm(13), angmas(3), depsm, nu
    character(len=4) :: fami
    character(len=8) :: nompim(12), mazars(8), materi
    character(len=16) :: compo, algo, nomres(2)
    character(len=30) :: valkm(3)
    aster_logical :: ltemp
!
    data nompim /'SY', 'EPSI_ULT', 'SIGM_ULT', 'EPSP_HAR', 'R_PM',&
                 'EP_SUR_E', 'A1_PM', 'A2_PM', 'ELAN', 'A6_PM', 'C_PM', 'A_PM'/
    data mazars /'EPSD0', 'K', 'AC', 'BC', 'AT', 'BT', 'SIGM_LIM', 'EPSI_LIM'/
!
! --------------------------------------------------------------------------------------------------
    codret = 0
    codrep = 0
    fami = 'RIGI'
    materi = compor(1)(1:8)
    compo  = compor(2)(1:16)
    algo   = compor(3)(1:16)
!
!   calcul de la température
    call rcvarc(' ', 'TEMP', 'REF', fami, 1,&
                1, tref, iret1)
!
    ltemp=.true.
    if (iret1 .eq. 1) ltemp=.false.
    if (.not.ltemp) then
        nomres(1) = 'E'
        nomres(2) = 'NU'
        call rcvalb(fami, 1, 1, '+', icdmat, materi, 'ELAS', 0, '', [0.d0],&
                    2, nomres, valres, icodre, 1)
        ep = valres(1)
        nu = valres(2)
        em=ep
        depsth=0.d0
    endif
!   angle du MOT_CLEF massif (AFFE_CARA_ELEM)
!   initialise à 0.D0 (on ne s'en sert pas)
    call r8inir(3, 0.d0, angmas, 1)
!
! --------------------------------------------------------------------------------------------------
    if (compo .eq. 'ELAS') then
        nomres(1) = 'E'
        do i = 1, nf
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, 'T', icdmat, materi, em, ep, nu, depsth)
            endif
            modf(i) = ep
            sigf(i) = ep*(contm(i)/em + ddefp(i) - depsth)
        enddo
!
! --------------------------------------------------------------------------------------------------
    else if (compo.eq.'MAZARS_GC') then
        epsth = 0.d0
!       on récupère les paramètres matériau
        call r8inir(nbval, 0.d0, valres, 1)
        call rcvalb(fami, 1, 1, '+', icdmat,&
                    materi, 'MAZARS', 0, ' ', [0.0d0],&
                    8, mazars, valres, icodre, 1)
        if (icodre(7)+icodre(8) .ne. 0) then
            valkm(1)='MAZARS_GC'
            valkm(2)=mazars(7)
            valkm(3)=mazars(8)
            call utmess('F', 'COMPOR1_76', nk=3, valk=valkm)
        endif
!       ajout de NU dans VALRES
        valres(9) = nu
!       boucle comportement sur chaque fibre
        do i = 1, nf
            ivari = nbvalc*(i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, '+', icdmat, materi, em, ep, nu, epsth)
                valres(9) = nu
            endif
            epsm = defm(i) - epsth
            call mazu1d(ep, valres, contm(i), varim(ivari), epsm,&
                        ddefp(i), modf(i), sigf(i), varip(ivari), option)
        enddo
!
! --------------------------------------------------------------------------------------------------
    else if (compo.eq.'VMIS_CINE_GC') then
!       boucle sur chaque fibre
        do i = 1, nf
            ivari = nbvalc*(i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, 'T', icdmat, materi, em, ep, nu, depsth)
            endif
            depsm = ddefp(i)-depsth
            call vmci1d('RIGI', kpg, i, icdmat, em,&
                        ep, contm(i), depsm, varim(ivari), option,&
                        materi, sigf(i), varip(ivari), modf(i))
        enddo
!
! --------------------------------------------------------------------------------------------------
    else if (compo.eq.'PINTO_MENEGOTTO') then
!       on récupère les paramètres matériau
        call r8inir(nbval, 0.d0, valres, 1)
        call rcvalb(fami, 1, 1, '-', icdmat,&
                    materi, 'PINTO_MENEGOTTO', 0, ' ', [0.0d0],&
                    12, nompim, valres, icodre, 0)
        if (icodre(7) .ne. 0) valres(7) = -1.0d0
        cstpm(1) = ep
        do i = 1, 12
            cstpm(i+1) = valres(i)
        enddo
        do i = 1, nf
            ivari = nbvalc*(i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, 'T', icdmat, materi, em, ep, nu, depsth)
                cstpm(1) = ep
            endif
            depsm = ddefp(i)-depsth
            call nm1dpm('RIGI', kpg, i, icdmat, option,&
                        nbvalc, 13, cstpm, contm(i), varim(ivari),&
                        depsm, varip(ivari), sigf(i), modf(i))
        enddo
!
! --------------------------------------------------------------------------------------------------
    else if (compo.eq.'VMIS_CINE_LINE') then
        do i = 1, nf
            ivari = nbvalc* (i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, 'T', icdmat, materi, em, ep, nu, depsth)
            endif
            depsm = ddefp(i)-depsth
            call nm1dci('RIGI', kpg, i, icdmat, em,&
                        ep, contm(i), depsm, varim(ivari), option,&
                        materi, sigf(i), varip(ivari), modf(i))
        enddo
!
! --------------------------------------------------------------------------------------------------
    else if ((compo.eq.'VMIS_ISOT_LINE').or.(compo.eq.'VMIS_ISOT_TRAC')) then
        do i = 1, nf
            ivari = nbvalc* (i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, 'T', icdmat, materi, em, ep, nu, depsth)
            endif
            depsm = ddefp(i)-depsth
            call nm1dis('RIGI', kpg, i, icdmat, em,&
                        ep, contm(i), depsm, varim(ivari), option,&
                        compo, materi, sigf(i), varip(ivari), modf(i))
        enddo
!
! --------------------------------------------------------------------------------------------------
    else if (compo.eq.'CORR_ACIER') then
        do i = 1, nf
            ivari = nbvalc* (i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, 'T', icdmat, materi, em, ep, nu, depsth)
            endif
            call nm1dco('RIGI', kpg, i, option, icdmat,&
                        materi, ep, contm(i), epsm, ddefp(i),&
                        varim(ivari), sigf(i), varip(ivari), modf(i), crit,&
                        codret)
            if (codret .ne. 0) goto 999
        enddo
!
! --------------------------------------------------------------------------------------------------
    else if ((compo.eq.'GRAN_IRRA_LOG').or.(compo.eq.'VISC_IRRA_LOG')) then
        if (algo(1:10) .eq. 'ANALYTIQUE') then
            if (.not. ltemp) then
                call utmess('F', 'COMPOR5_40',sk=compo)
            endif
            do i = 1, nf
                ivari = nbvalc* (i-1) + 1
                if (ltemp) then
                    ksp=debsp-1+i
                    call paeldt(kpg, ksp, fami, 'T', icdmat, materi, em, ep, nu, depsth,&
                                tmoins=tempm, tplus=tempp, trefer=tref)
                endif
                depsm = ddefp(i)-depsth
                call nm1vil('RIGI', kpg, i, icdmat, materi,&
                            crit, instam, instap, tempm, tempp,&
                            tref, depsm, contm(i), varim(ivari), option,&
                            defam(1), defap(1), angmas, sigf(i), varip( ivari),&
                            modf(i), codret, compo, nbvalc)
                if (codret .ne. 0) goto 999
            enddo
        else
            if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9) .eq.'RAPH_MECA')) then
                nbvari = nbvalc*nf
                call dcopy(nbvari, varimp, 1, varip, 1)
            endif
            do i = 1, nf
                ivari = nbvalc* (i-1) + 1
                sigx = contm(i)
                epsx = defm(i)
                depsx = ddefp(i)
!               attention, que pour 1 matériau par élément !!!!!
                call comp1d('RIGI', kpg, i, option, sigx,&
                            epsx, depsx, angmas, varim(ivari), varip(ivari),&
                            sigf(i), modf(i), codrep)
                if (codrep .ne. 0) then
                    codret=codrep
!                   code 3: on continue et on le renvoie à la fin. Autres codes: sortie immédiate
                    if (codrep .ne. 3) goto 999
                endif
            enddo
        endif
!
! --------------------------------------------------------------------------------------------------
    else if (compo.eq.'GRANGER_FP_INDT') then
!       Appel à comp1d pour bénéficier des comportements AXIS: méthode de DEBORST
!           La LDC doit retourner le module tangent
        if ((algo(1:7).ne.'DEBORST') .and. (compo(1:4).ne.'SANS')) then
            valkm(1) = compo
            valkm(2) = 'DEFI_COMPOR/MULTIFIBRE'
            call utmess('F', 'ALGORITH6_81', nk=2, valk=valkm)
        else
            if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9) .eq.'RAPH_MECA')) then
                nbvari = nbvalc*nf
                call dcopy(nbvari, varimp, 1, varip, 1)
            endif
            do i = 1, nf
                ivari = nbvalc* (i-1) + 1
                sigx = contm(i)
                epsx = defm(i)
                depsx = ddefp(i)
!               attention, que pour 1 matériau par élément !!!!!
                call comp1d('RIGI', kpg, i, option, sigx,&
                            epsx, depsx, angmas, varim(ivari), varip(ivari),&
                            sigf(i), modf(i), codrep)
                if (codrep .ne. 0) then
                    codret=codrep
!                   code 3: on continue et on le renvoie à la fin. Autre codes: sortie immédiate
                    if (codrep .ne. 3) goto 999
                endif
            enddo
        endif
    else
        call utmess('F', 'ELEMENTS2_39', sk=compo)
    endif
!
999 continue
end subroutine
