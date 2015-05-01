subroutine lcpima(fami, kpg, ksp, poum, mate,&
                  compor, instam, instap, crit, sigm,&
                  vim)
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
! ***************************************************************
! *       INTEGRATION DE LA LOI SIMO_MIEHE ECROUISSAGE ISOTROPE *
! *        LECTURE DES CARACTERISTIQUES DU MATERIAU             *
! ***************************************************************
! IN  MATE    : ADRESSE DU MATERIAU
! IN  COMPOR  : COMPORTEMENT
! IN  TEMP    : TEMPERATURE A L'INSTANT DU CALCUL
! IN  INSTAM  : INTANT T-
! IN  INSTAP  : INSTANT T+
! IN COMMON   : PM DOIT DEJA ETRE AFFECTE (PLASTICITE CUMULEE EN T-)
! ----------------------------------------------------------------------
!
    implicit none
!
! DECLARATION GLOBALE
!
#include "asterc/r8miem.h"
#include "asterfort/ecpuis.h"
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
#include "asterfort/rctype.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/rupmat.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
    integer :: mate, kpg, ksp
    character(len=16) :: compor
    character(len=*) :: fami
    character(len=1) :: poum
    real(kind=8) :: temp, instam, instap, crit(*), sigm(*), vim(*)
!
!  COMMON MATERIAU POUR VON MISES
!
    integer :: jprol, jvale, nbval
    real(kind=8) :: pm, young, nu, mu, unk, troisk, cother, val(1)
    real(kind=8) :: sigm0, epsi0, dt, coefm, rpm, pente
!
    common /lcpim/&
     &          pm,young,nu,mu,unk,troisk,cother,&
     &          sigm0,epsi0,dt,coefm,rpm,pente,&
     &          apui,npui,sigy,jprol,jvale,nbval
! ----------------------------------------------------------------------
!
! DECLARATION LOCALE
!
    integer :: icodre(3), lgpg
    character(len=16) :: nomres(3)
    character(len=8) :: kbid, para_type
    real(kind=8) :: epsthe, sigy, aire, dsde, valrm(2)
    real(kind=8) :: r8bid, valres(3), para_vale, apui, npui, rprim
    integer :: iret1, iret2
! ----------------------------------------------------------------------
!
    r8bid  = 0.d0
!
! 1 - A L INSTANT COURANT YOUNG, MU ET UNK
!
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'ELAS', 0, kbid, [r8bid],&
                1, 'NU', val, icodre(1), 2)
    nu=val(1)            
!
    call verift(fami, kpg, ksp, poum, mate,&
                iret = iret1, epsth=epsthe)
    call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                ksp, temp, iret2)
    if (compor(6:14) .eq. 'ISOT_TRAC') then
        call rctype(mate, 1, 'TEMP', [temp], para_vale,&
                    para_type)
        if ((para_type.eq.'TEMP') .and. (iret1.eq.1)) then
            call utmess('F', 'COMPOR5_5', sk = para_type)
        endif
        call rctrac(mate, 1, 'SIGM', para_vale, jprol,&
                    jvale, nbval, young)
    else
        call rcvalb(fami, kpg, ksp, poum, mate,&
                    ' ', 'ELAS', 0, kbid, [r8bid],&
                    1, 'E', val, icodre(1), 2)
        young=val(1)            
    endif
!
!     CRIT_RUPT
    if ((crit(13).gt.0.d0) .and. (vim(14).gt.0.d0)) then
        lgpg = 14
        call rupmat(fami, kpg, ksp, mate, vim,&
                    lgpg, young, sigm)
    endif
    mu = young/(2.d0*(1.d0+nu))
    troisk = young/(1.d0-2.d0*nu)
    unk = troisk/3.d0
!
! 2 - COEFFICIENT DE DILATATION THERMIQUE ALPHA
!     => CONTRAINTE THERMIQUE COTHER
!
    cother = troisk*epsthe
!
! 3 - SIGY, PENTE ET ECROUISSAGE A TEMP ET EN P-
!
    if (compor(10:14) .eq. '_TRAC') then
        call rcfonc('S', 1, jprol, jvale, nbval,&
                    sigy = sigy)
!
        call rcfonc('V', 1, jprol, jvale, nbval,&
                    p = pm, rp = rpm, rprim = pente, airerp = aire)
    endif
!
    if (compor(10:14) .eq. '_LINE') then
        nomres(1)='D_SIGM_EPSI'
        nomres(2)='SY'
        call rcvalb(fami, kpg, ksp, poum, mate,&
                    ' ', 'ECRO_LINE', 0, kbid, [r8bid],&
                    2, nomres, valres, icodre(1), 2)
        dsde=valres(1)
        sigy=valres(2)
        if ((young-dsde) .lt. r8miem()) then
            valrm(1)=dsde
            valrm(2)=young
            call utmess('F', 'COMPOR1_54', nr=2, valr=valrm)
        else
            pente=dsde*young/(young-dsde)
        endif
        rpm=pente*pm+sigy
    endif
!
    if (compor(10:14) .eq. '_PUIS') then
        nomres(1)='SY'
        nomres(2)='A_PUIS'
        nomres(3)='N_PUIS'
        call rcvalb(fami, kpg, ksp, poum, mate,&
                    ' ', 'ECRO_PUIS', 0, kbid, [r8bid],&
                    3, nomres, valres, icodre, 2)
        sigy = valres(1)
        apui = valres(2)
        npui = valres(3)
        call ecpuis(young, sigy, apui, 1.d0/npui, pm,&
                    0.d0, rpm, rprim)
    endif
!
! 4 - PARAMETRES DE CARACTERISTIQUES VISQUEUSES SI BESOIN
!
    dt = instap - instam
    if (compor(1:4) .eq. 'VISC') then
        nomres(1)= 'SIGM_0'
        nomres(2)= 'EPSI_0'
        nomres(3)= 'M'
        call rcvalb(fami, kpg, ksp, poum, mate,&
                    ' ', 'VISC_SINH', 0, kbid, [r8bid],&
                    3, nomres, valres, icodre(1), 2)
        sigm0=valres(1)
        epsi0=valres(2)
        coefm= valres(3)
    endif
!
end subroutine
