subroutine nmzmat(fami, kpg, ksp, ndim, typmod,&
                  compor, instam, instap, epsm, deps,&
                  sigm, vim, option, angmas, sigp,&
                  vip, dsidep, codret)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
! ----------------------------------------------------------------------
!     INTEGRATION DU COMPORTEMENT EN UTILISANT LA BIBLIOTHEQUE ZMAT
! ----------------------------------------------------------------------
!     ROUTINE INTERFACE : Zaster.c (EN C++ DANS libzAster.so)
!
! ARGUMENTS DE ZASTER (SIMPLEMENT DEDUITS DE CEUX DE NMZMAT) :
! IN :
!  FAMI    (K) : LISTE DE NOMS DE FAMILLES DE POINTS DE GAUSS
!  KPG     (I) : NUMERO DE POINT DE GAUSS
!  KSP     (I) : NUMERO DE SOUS-POINT
!  NDIM    (I) : DIMENSION DE L'ESPACE (2 OU 3)
!  TYPMOD  (K) : TYPE DE MODELE (1:3D, 2:AXIS, 3:D_PLAN, 4:C_PLAN)
!  COMPOR  : (1) = TYPE DE RELATION COMPORTEMENT (INUTILE ICI)
!            (2) = NB VARIABLES INTERNES / PG (UTILISE ICI)
!            (3) = HYPOTHESE SUR LES DEFORMATIONS (UTILISE)
!            (6) NUNIT  : NUMERO DE L'UNITE LOGIQUE QUI CONTIENT
!                LES DONNEES POUR LE COMPORTEMENT ZMAT
!  INSTAM (R8) : INSTANT DU CALCUL PRECEDENT
!  INSTAP (R8) : INSTANT DU CALCUL
!  EPSM  (R8*) : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
!  DEPS  (R8*) : INCREMENT DE DEFORMATION TOTALE :
!                   DEPS(T) = DEPS(MECANIQUE(T)) + DEPS(DILATATION(T))
!  SIGM  (R8*) : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
!  VIM   (R8*) : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
!  OPTION  (I) : OPTION (1:RIGI_MECA_TANG, 2:FULL_MECA , 3:RAPH_MECA)
!  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
!
! IN/OUT :
!  VIP   (R8*) : VARIABLES INTERNES
!                IN  : ESTIMATION (ITERATION PRECEDENTE OU LAG. AUGM.)
!                OUT : EN T+
!
! OUT :
!  SIGP   (R8*) : CONTRAINTES A L'INSTANT DU CALCUL
!  DSIDEP (R8*) : MATRICE CARREE
! ----------------------------------------------------------------------
!
    implicit none
    include 'jeveux.h'
    include 'asterc/zaswrp.h'
    include 'asterfort/naueul.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/tecael.h'
    integer :: ndim, codret, i, nunit, kpg, ksp, nvarcm, iret2, idbg, numa
    parameter (nvarcm=5)
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), option, nomvar(nvarcm)
    character(len=*) :: fami
    real(kind=8) :: instam, instap
    real(kind=8) :: epsm(*), deps(*), dsidep(*)
    real(kind=8) :: sigm(*), vim(*), sigp(*), vip(*)
    real(kind=8) :: angmas(7), angeul(3)
    real(kind=8) :: varplu(nvarcm), varmoi(nvarcm)
    real(kind=8) :: varref(nvarcm)
!
!
!
!
!
    integer :: izi, izk
    integer :: modele, nvar, ndef, nopt
!
!     MODELE=1 EN 3D,2 EN AXIS, 3 EN D_PLAN, 4 EN C_PLAN
    if (typmod(1) .eq. '3D') then
        modele=1
    else if (typmod(1).eq.'AXIS') then
        modele=2
    else if (typmod(1).eq.'D_PLAN') then
        modele=3
    else
        modele=4
    endif
    read (compor(7),'(I16)') nunit
    read (compor(2),'(I16)') nvar
    if (compor(3) .eq. 'GDEF_HYPO_ELAS') then
        ndef=2
    else
        ndef=1
    endif
!
    if (option .eq. 'RIGI_MECA_TANG') then
        nopt=1
    else if (option.eq.'FULL_MECA') then
        nopt=2
    else
        nopt=3
    endif
!
!     VARIABLES DE COMMANDE
    nomvar(1)='temperature'
    nomvar(2)='fluence'
    nomvar(3)='corrosion'
    nomvar(4)='hydratation'
    nomvar(5)='sechage'
!
!     TEMPERATURE
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, varmoi(1), iret2)
    if (iret2 .gt. 0) varmoi(1)=0.d0
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, varplu(1), iret2)
    if (iret2 .gt. 0) varplu(1)=0.d0
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, varref(1), iret2)
    if (iret2 .gt. 0) varref(1)=0.d0
!
!     IRRADIATION AU POINT CONSIDERE
!     FLUX NEUTRONIQUE
    call rcvarc(' ', 'IRRA', '-', fami, kpg,&
                ksp, varmoi(2), iret2)
    if (iret2 .gt. 0) varmoi(2)=0.d0
    call rcvarc(' ', 'IRRA', '+', fami, kpg,&
                ksp, varplu(2), iret2)
    if (iret2 .gt. 0) varplu(2)=0.d0
    varref(2)=0.d0
!
!     CORROSION
    call rcvarc(' ', 'CORR', '-', fami, kpg,&
                ksp, varmoi(3), iret2)
    if (iret2 .gt. 0) varmoi(3)=0.d0
    call rcvarc(' ', 'CORR', '+', fami, kpg,&
                ksp, varplu(3), iret2)
    if (iret2 .gt. 0) varplu(3)=0.d0
    varref(3)=0.d0
!
!     hydratation
    call rcvarc(' ', 'HYDR', '-', fami, kpg,&
                ksp, varmoi(4), iret2)
    if (iret2 .gt. 0) varmoi(4)=0.d0
    call rcvarc(' ', 'CORR', '+', fami, kpg,&
                ksp, varplu(4), iret2)
    if (iret2 .gt. 0) varplu(4)=0.d0
    varref(4)=0.d0
!
!     sechage
    call rcvarc(' ', 'SECH', '-', fami, kpg,&
                ksp, varmoi(5), iret2)
    if (iret2 .gt. 0) varmoi(5)=0.d0
    call rcvarc(' ', 'SECH', '+', fami, kpg,&
                ksp, varplu(5), iret2)
    if (iret2 .gt. 0) varplu(5)=0.d0
    call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                ksp, varref(5), iret2)
    if (iret2 .ne. 0) varref(5)=0.d0
!
    idbg=0
    if (idbg .eq. 1) then
        call tecael(izi, izk)
        numa=zi(izi)
    else
        numa=0
    endif
    if (ndim .eq. 3) then
        if (angmas(4) .eq. 1.d0) then
            call naueul(angmas, angeul)
        else
            do 1 i = 1, 3
                angeul(i)=angmas(4+i)
 1          continue
        endif
    else
        angeul(1)=angmas(5)
        angeul(2)=0.d0
        angeul(3)=0.d0
    endif
!
    call zaswrp(numa, modele, nvar, ndef, nunit,&
                instam, instap, nvarcm, nomvar, varplu,&
                varmoi, varref, epsm, deps, sigm,&
                vim, nopt, angeul, sigp, vip,&
                dsidep, codret)
!
end subroutine
