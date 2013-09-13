subroutine pmfcom(kpg, debsp, option, compor, crit,&
                  nf, instam, instap, icdmat, nbvalc,&
                  defam, defap, varim, varimp, contm,&
                  defm, ddefp, epsm, modf, sigf,&
                  varip, isecan, codret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
    implicit none
#include "asterfort/comp1d.h"
#include "asterfort/mazu1d.h"
#include "asterfort/nm1dci.h"
#include "asterfort/nm1dco.h"
#include "asterfort/nm1dis.h"
#include "asterfort/nm1dpm.h"
#include "asterfort/nm1tra.h"
#include "asterfort/nm1vil.h"
#include "asterfort/nmcb1d.h"
#include "asterfort/paeldt.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/verifm.h"
#include "asterfort/verift.h"
#include "asterfort/vmci1d.h"
#include "blas/dcopy.h"
    integer :: nf, icdmat, nbvalc, isecan, kpg, debsp
    real(kind=8) :: contm(nf), defm(nf), ddefp(nf), modf(nf), sigf(nf)
    real(kind=8) :: varimp(nbvalc*nf), varip(nbvalc*nf), varim(nbvalc*nf)
    real(kind=8) :: tempm, tempp, tref, sigx, epsx, depsx, instam, instap
    real(kind=8) :: crit(*), defap(*), defam(*)
!
    character(len=16) :: option
    character(len=24) :: compor(*)
    logical :: ltemp
! --- ------------------------------------------------------------------
!
!        AIGUILLAGE COMPORTEMENT DES ELEMENTS DE POUTRE MULTIFIBRES
!
! --- ------------------------------------------------------------------
!
! IN  KPG   : NUMERO DE POINT DE GAUSS
! IN  DEBSP : NUMERO DE SOUS-POINT DE LA PREMIERE FIBRE DU GROUPE
! IN  COMPO : NOM DU COMPORTEMENT
! IN  CRIT  : CRITERES DE CONVERGENCE LOCAUX
! IN  NF    : NOMBRE DE FIBRES DU GROUPE
! IN  INSTAM: INSTANT DU CALCUL PRECEDENT
! IN  INSTAP: INSTANT DU CALCUL
! IN  E     : MODULE D'YOUNG (ELASTIQUE)
! IN  ICDMAT: CODE MATERIAU
! IN  NV    : NOMBRE DE VARIABLES INTERNES DU MODELE
! IN  VARIM : VARIABLES INTERNES MOINS
! IN  VARIMP: VARIABLES INTERNES ITERATION PRECEDENTE (POUR DEBORST)
! IN  DEFAM : DEFORMATIONS ANELASTIQUES A L'INSTANT PRECEDENT
! IN  DEFAP : DEFORMATIONS ANELASTIQUES A L'INSTANT DU CALCUL
! IN  CONTM : CONTRAINTES MOINS PAR FIBRE
! IN  DEFM  : DEFORMATION  A L'INSTANT DU CALCUL PRECEDENT
! IN  DDEFP : INCREMENT DE DEFORMATION
! IN  EPSM  : DEFORMATION A L'INSTANT PRECEDENT
! OUT MODF  : MODULE TANGENT DES FIBRES
! OUT SIGF  : CONTRAINTE A L'INSTANT ACTUEL DES FIBRES
! OUT VARIP : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT ISECAN: EVALUATION DU MODULE SECANT
!              0 :
!              1 :
!
! --- ------------------------------------------------------------------
    integer :: nbval, nbvari, codrep, ksp
    parameter     (nbval=12)
    integer :: icodre(nbval)
    integer :: i, ivari, codret, iret1, iret2, iret3
    real(kind=8) :: valres(nbval), ep, em, depsth
    real(kind=8) :: cstpm(13), epsm, angmas(3), depsm, nu
    character(len=4) :: fami
    character(len=8) :: noeclb(9), nompim(12), mazars(8), ecroli(4)
    character(len=8) :: materi, nomres(2)
    character(len=16) :: compo, algo
    character(len=30) :: valkm(3)
!
    data noeclb /'Y01','Y02','A1','A2','B1',&
     &             'B2','BETA1','BETA2','SIGF'/
    data nompim /'SY','EPSI_ULT','SIGM_ULT','EPSP_HAR','R_PM',&
     &             'EP_SUR_E','A1_PM','A2_PM','ELAN','A6_PM',&
     &              'C_PM','A_PM'/
    data mazars /'EPSD0','K','AC','BC','AT',&
     &             'BT','SIGM_LIM','EPSI_LIM'/
    data ecroli /'D_SIGM_E','SY','SIGM_LIM','EPSI_LIM'/
!
! --- ------------------------------------------------------------------
    codret = 0
    codrep = 0
    fami = 'RIGI'
    materi = compor(1)(1:8)
    compo = compor(2)(1:16)
    algo = compor(3)(1:16)
!
!     CALCUL DE LA TEMPERATURE
    call rcvarc(' ', 'TEMP', 'REF', fami, 1,&
                1, tref, iret1)
!
    ltemp=.true.
    if (iret1 .eq. 1) ltemp=.false.
    if (.not.ltemp) then
        nomres(1) = 'E'
        nomres(2) = 'NU'
        call rcvalb(fami, 1, 1, '+', icdmat,&
                    materi, 'ELAS', 0, '', 0.d0,&
                    2, nomres, valres, icodre, 1)
        ep = valres(1)
        nu = valres(2)
        em=ep
        depsth=0.d0
    endif
!
!     EVALUATION DU MODULE SECANT
    isecan = 0
! --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! --- INITIALISE A 0.D0 (ON NE S'EN SERT PAS)
    call r8inir(3, 0.d0, angmas, 1)
!
    if (compo .eq. 'ELAS') then
        nomres(1) = 'E'
        do 100 i = 1, nf
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, icdmat, materi,&
                            em, ep, nu, depsth)
            endif
            modf(i) = ep
            sigf(i) = ep*(contm(i)/em + ddefp(i) - depsth)
100      continue
!
    else if (compo.eq.'LABORD_1D') then
        depsth = 0.d0
! ---    ON RECUPERE LES PARAMETRES MATERIAU
        call r8inir(nbval, 0.d0, valres, 1)
        call rcvalb(fami, 1, 1, '+', icdmat,&
                    materi, 'LABORD_1D', 0, ' ', 0.0d0,&
                    9, noeclb, valres, icodre, 1)
! ---    BOUCLE COMPORTEMENT SUR CHAQUE FIBRE
        do 250 i = 1, nf
            ivari = nbvalc*(i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, icdmat, materi,&
                            em, ep, nu, depsth)
            endif
            epsm = defm(i) - depsth
            call nmcb1d(ep, valres, contm(i), varim(ivari), epsm,&
                        ddefp(i), modf(i), sigf(i), varip(ivari), crit,&
                        option)
250      continue
!
    else if (compo.eq.'MAZARS_GC') then
        depsth = 0.d0
! ---    ON RECUPERE LES PARAMETRES MATERIAU
        call r8inir(nbval, 0.d0, valres, 1)
        call rcvalb(fami, 1, 1, '+', icdmat,&
                    materi, 'MAZARS', 0, ' ', 0.0d0,&
                    8, mazars, valres, icodre, 1)
        if (icodre(7)+icodre(8) .ne. 0) then
            valkm(1)='MAZARS_GC'
            valkm(2)=mazars(7)
            valkm(3)=mazars(8)
            call utmess('F', 'COMPOR1_76', nk=3, valk=valkm)
        endif
! ---    AJOUT DE NU DANS VALRES
        valres(9) = nu
! ---    BOUCLE COMPORTEMENT SUR CHAQUE FIBRE
        do 275 i = 1, nf
            ivari = nbvalc*(i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, icdmat, materi,&
                            em, ep, nu, depsth)
                valres(9) = nu
            endif
            epsm = defm(i) - depsth
            call mazu1d(ep, valres, contm(i), varim(ivari), epsm,&
                        ddefp(i), modf(i), sigf(i), varip(ivari), option)
275      continue
!
    else if (compo.eq.'VMIS_CINE_GC') then
! ---    VERIFICATION QUE SIGM_LIM, EPSI_LIM SONT PRESENT
        call r8inir(nbval, 0.d0, valres, 1)
        call rcvalb(fami, 1, 1, '+', icdmat,&
                    materi, 'ECRO_LINE', 0, ' ', 0.d0,&
                    4, ecroli, valres, icodre, 1)
        if (icodre(3)+icodre(4) .ne. 0) then
            valkm(1)='VMIS_CINE_GC'
            valkm(2)=ecroli(3)
            valkm(3)=ecroli(4)
            call utmess('F', 'COMPOR1_76', nk=3, valk=valkm)
        endif
! ---    BOUCLE SUR CHAQUE FIBRE
        do 200 i = 1, nf
            ivari = nbvalc*(i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, icdmat, materi,&
                            em, ep, nu, depsth)
            endif
            depsm = ddefp(i)-depsth
            call vmci1d('RIGI', kpg, i, icdmat, em,&
                        ep, contm(i), depsm, varim(ivari), option,&
                        materi, sigf(i), varip(ivari), modf(i))
200      continue
!
    else if (compo.eq.'PINTO_MENEGOTTO') then
! ---    ON RECUPERE LES PARAMETRES MATERIAU
        call r8inir(nbval, 0.d0, valres, 1)
        call rcvalb(fami, 1, 1, '-', icdmat,&
                    materi, 'PINTO_MENEGOTTO', 0, ' ', 0.0d0,&
                    12, nompim, valres, icodre, 0)
        if (icodre(7) .ne. 0) valres(7) = -1.0d0
        cstpm(1) = ep
        do 300 i = 1, 12
            cstpm(i+1) = valres(i)
300      continue
        do 350 i = 1, nf
            ivari = nbvalc*(i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, icdmat, materi,&
                            em, ep, nu, depsth)
                cstpm(1) = ep
            endif
            depsm = ddefp(i)-depsth
            call nm1dpm('RIGI', kpg, i, icdmat, option,&
                        nbvalc, 13, cstpm, contm(i), varim(ivari),&
                        depsm, varip(ivari), sigf(i), modf(i))
350      continue
!
    else if (compo.eq.'VMIS_CINE_LINE') then
        do 400 i = 1, nf
            ivari = nbvalc* (i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, icdmat, materi,&
                            em, ep, nu, depsth)
            endif
            depsm = ddefp(i)-depsth
            call nm1dci('RIGI', kpg, i, icdmat, em,&
                        ep, contm(i), depsm, varim(ivari), option,&
                        materi, sigf(i), varip(ivari), modf(i))
400      continue
!
    else if (compo.eq.'VMIS_ISOT_LINE') then
        do 450 i = 1, nf
            ivari = nbvalc* (i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, icdmat, materi,&
                            em, ep, nu, depsth)
            endif
            depsm = ddefp(i)-depsth
            call nm1dis('RIGI', kpg, i, icdmat, em,&
                        ep, contm(i), depsm, varim(ivari), option,&
                        compo, materi, sigf(i), varip(ivari), modf(i))
450      continue
!
    else if (compo.eq.'CORR_ACIER') then
        do 500 i = 1, nf
            ivari = nbvalc* (i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, icdmat, materi,&
                            em, ep, nu, depsth)
            endif
            call nm1dco('RIGI', kpg, i, option, icdmat,&
                        materi, ep, contm(i), epsm, ddefp(i),&
                        varim(ivari), sigf(i), varip(ivari), modf(i), crit,&
                        codret)
            if (codret .ne. 0) goto 9999
500      continue
!
    else if (compo.eq.'VMIS_ISOT_TRAC') then
        do 550 i = 1, nf
            ivari = nbvalc* (i-1) + 1
            if (ltemp) then
                ksp=debsp-1+i
                call paeldt(kpg, ksp, fami, icdmat, materi,&
                            em, ep, nu, depsth)
            endif
!           NM1TRA NE FONCTIONNE QUE POUR 1 MATERIAU PAR MAILLE
!           (VERIFIE DANS RCTRAC)
            call nm1tra(icdmat, tempp, defm(i), ddefp(i), varim(ivari),&
                        varim(ivari+1), sigf(i), varip(ivari), varip(ivari+1), modf( i))
550      continue
!
        else if ((compo.eq.'GRAN_IRRA_LOG').or. (compo.eq.'VISC_IRRA_LOG')&
    ) then
        if (algo(1:10) .eq. 'ANALYTIQUE') then
            do 600 i = 1, nf
                ivari = nbvalc* (i-1) + 1
                if (ltemp) then
                    ksp=debsp-1+i
                    call paeldt(kpg, ksp, fami, icdmat, materi,&
                                em, ep, nu, depsth)
                endif
                depsm = ddefp(i)-depsth
                if ((iret1+iret2+iret3) .ge. 1) then
                    call utmess('F', 'CALCULEL_31')
                endif
                call nm1vil('RIGI', kpg, i, icdmat, materi,&
                            crit, instam, instap, tempm, tempp,&
                            tref, depsm, contm(i), varim(ivari), option,&
                            defam(1), defap(1), angmas, sigf(i), varip( ivari),&
                            modf(i), codret, compo, nbvalc)
                if (codret .ne. 0) goto 9999
600          continue
        else
            if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9) .eq.'RAPH_MECA')) then
                nbvari = nbvalc*nf
                call dcopy(nbvari, varimp, 1, varip, 1)
            endif
            do 700 i = 1, nf
                ivari = nbvalc* (i-1) + 1
                sigx = contm(i)
                epsx = defm(i)
                depsx = ddefp(i)
!              ATTENTION, QUE POUR 1 MATERIAU PAR ELEMENT !!!!!
                call comp1d('RIGI', kpg, i, option, sigx,&
                            epsx, depsx, angmas, varim(ivari), varip(ivari),&
                            sigf(i), modf(i), codrep)
                if (codrep .ne. 0) then
                    codret=codrep
!                 CODE 3: ON CONTINUE ET ON LE RENVOIE A LA FIN
!                 AUTRE CODES: SORTIE IMMEDIATE
                    if (codrep .ne. 3) goto 9999
                endif
!              SI MODULE TANGENT PAS CALCULE EXACTEMENT -> EVALUATION
                isecan = 0
700          continue
        endif
!
    else if (compo.eq.'LEMA_SEUIL') then
        call utmess('F', 'ELEMENTS2_39')
    else
!        APPEL A COMP1D POUR BENEFICIER DE TOUS LES COMPORTEMENTS AXIS
!        PAR UNE EXTENSION DE LA METHODE DE DEBORST
        if ((algo(1:7).ne.'DEBORST') .and. (compo(1:4).ne.'SANS')) then
            valkm(1) = compo
            valkm(2) = 'DEFI_COMPOR/MULTIFIBRE'
            call utmess('F', 'ALGORITH6_81', nk=2, valk=valkm)
        else
            if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9) .eq.'RAPH_MECA')) then
                nbvari = nbvalc*nf
                call dcopy(nbvari, varimp, 1, varip, 1)
            endif
            do 750 i = 1, nf
                ivari = nbvalc* (i-1) + 1
                sigx = contm(i)
                epsx = defm(i)
                depsx = ddefp(i)
!              ATTENTION, QUE POUR 1 MATERIAU PAR ELEMENT !!!!!
                call comp1d('RIGI', kpg, i, option, sigx,&
                            epsx, depsx, angmas, varim(ivari), varip(ivari),&
                            sigf(i), modf(i), codrep)
                if (codrep .ne. 0) then
                    codret=codrep
!                 CODE 3: ON CONTINUE ET ON LE RENVOIE A LA FIN
!                 AUTRE CODES: SORTIE IMMEDIATE
                    if (codrep .ne. 3) goto 9999
                endif
!              SI MODULE TANGENT PAS CALCULE EXACTEMENT -> EVALUATION
                isecan = 1
750          continue
        endif
    endif
!
9999  continue
end subroutine
