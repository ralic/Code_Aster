subroutine lcroma(fami, kpg, ksp, poum, mate)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    include 'asterfort/rcfonc.h'
    include 'asterfort/rctrac.h'
    include 'asterfort/rctype.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/u2mess.h'
    integer :: kpg, ksp, mate
    character(len=*) :: fami, poum
!
! ******************************************************
! *       INTEGRATION DE LA LOI DE ROUSSELIER LOCAL    *
! *        LECTURE DES CARACTERISTIQUES DU MATERIAU    *
! ******************************************************
!
! IN  MATE    : ADRESSE DU MATERIAU
! IN  TEMP    : TEMPERATURE A L'INSTANT DU CALCUL
! IN COMMON   : PM DOIT DEJA ETRE AFFECTE (PLASTICITE CUMULEE EN T-)
! ----------------------------------------------------------------------
!  COMMON LOI DE COMPORTEMENT ROUSSELIER
!
    integer :: itemax, jprolp, jvalep, nbvalp, iret
    real(kind=8) :: prec, young, nu, sigy, sig1, rousd, f0, fcr, acce
    real(kind=8) :: pm, rpm, fonc, fcd, dfcddj, dpmaxi
    common /lcrou/ prec,young,nu,sigy,sig1,rousd,f0,fcr,acce,&
     &               pm,rpm,fonc,fcd,dfcddj,dpmaxi,&
     &               itemax, jprolp, jvalep, nbvalp
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: icodre(6)
    character(len=8) :: nomres(6), type
    real(kind=8) :: r8bid, valres(6), pente, aire, temp, resu
! ----------------------------------------------------------------------
!
!
!
! 1 - CARACTERISTIQUE ELASTIQUE E ET NU => CALCUL DE MU - K
!
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, 'NU', nu, icodre(1), 2)
    call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                ksp, temp, iret)
    call rctype(mate, 1, 'TEMP', temp, resu,&
                type)
    if ((type.eq.'TEMP') .and. (iret.eq.1)) call u2mess('F', 'CALCULEL_31')
    call rctrac(mate, 1, 'SIGM', resu, jprolp,&
                jvalep, nbvalp, young)
!
!
! 2 - SIGY ET ECROUISSAGE EN P-
!
    call rcfonc('S', 1, jprolp, jvalep, nbvalp,&
                sigy, r8bid, r8bid, r8bid, r8bid,&
                r8bid, r8bid, r8bid, r8bid)
!
    call rcfonc('V', 1, jprolp, jvalep, nbvalp,&
                r8bid, r8bid, r8bid, pm, rpm,&
                pente, aire, r8bid, r8bid)
!
!
! 3 - PARAMETRES DE CROISSANCE DE CAVITES ET CONTROLE INCR. PLASTIQUE
!
    nomres(1) = 'D'
    nomres(2) = 'SIGM_1'
    nomres(3) = 'PORO_INIT'
    nomres(4) = 'PORO_CRIT'
    nomres(5) = 'PORO_ACCE'
    nomres(6) = 'DP_MAXI'
!
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'ROUSSELIER', 0, ' ', 0.d0,&
                6, nomres, valres, icodre, 2)
    rousd = valres(1)
    sig1 = valres(2)
    f0 = valres(3)
    fcr = valres(4)
    acce = valres(5)
    dpmaxi= valres(6)
!
end subroutine
