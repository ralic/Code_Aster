subroutine cfflm2(resoco, resigr, ndim, neq, nesmax,&
                  nbliac, nbliai, glitol, glimin, glimax)
!
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
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/calapr.h'
    include 'asterfort/cfcglc.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/r8inir.h'
    character(len=24) :: resoco
    real(kind=8) :: resigr
    real(kind=8) :: glimin, glimax, glitol
    integer :: neq, nbliac, nesmax, ndim, nbliai
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! CONSTRUCTION DE LA MATRICE TANGENTE DE FROTTEMENT - TERME NEGATIF
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  RESIGR : RESI_GLOB_RELA
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! IN  NBLIAI : NOMBRE DE LIAISONS
! IN  GLIMIN : GLISSEMENT MINIMUM
! IN  GLIMAX : GLISSEMENT MAXIMUM
! IN  GLITOL : TOELRANCE POUR DETECTER UN GLISSEMENT NUL
!
!
!
!
    integer :: ndlmax
    parameter   (ndlmax = 30)
    integer :: jdecal, nbddl
    real(kind=8) :: glis
    real(kind=8) :: coefpt, coefff, coefte, beta, alpha
    real(kind=8) :: lambdc, lambdf
    integer :: iliac, iliai, iliac2
    character(len=19) :: mu, liac, afmu
    integer :: jmu, jliac, jafmu
    character(len=24) :: apddl, appoin, apcofr
    integer :: japddl, japptr, japcof
    character(len=24) :: tacfin
    integer :: jtacf
    integer :: ztacf
    character(len=19) :: fro2
    integer :: jfro2
    character(len=19) :: deplc
    integer :: jdepc
    logical :: liaact
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcofr = resoco(1:14)//'.APCOFR'
    apddl = resoco(1:14)//'.APDDL'
    liac = resoco(1:14)//'.LIAC'
    tacfin = resoco(1:14)//'.TACFIN'
    afmu = resoco(1:14)//'.AFMU'
    mu = resoco(1:14)//'.MU'
    fro2 = resoco(1:14)//'.FRO2'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcofr, 'L', japcof)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(tacfin, 'L', jtacf)
    call jeveuo(afmu, 'L', jafmu)
    call jeveuo(mu, 'E', jmu)
    ztacf = cfmmvd('ZTACF')
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DEPLC : INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT
! ---         DU PAS DE TEMPS AVEC CORRECTION DU CONTACT
!
    deplc = resoco(1:14)//'.DEPC'
    call jeveuo(deplc (1:19)//'.VALE', 'L', jdepc)
!
! --- CALCUL DE LA MATRICE E_T*AaT
!
    do 100 iliai = 1, nbliai
!
! ----- INITIALISATION DE LA COLONNE
!
        call jeveuo(jexnum(fro2, iliai), 'E', jfro2)
        call r8inir(ndlmax, 0.d0, zr(jfro2), 1)
!
! ----- LA LIAISON EST-ELLE ACTIVE ?
!
        liaact = .false.
        do 200 iliac = 1, nbliac
            if (zi(jliac-1+iliac) .eq. iliai) then
                liaact = .true.
                iliac2 = iliac
            endif
200      continue
!
! ----- CALCUL
!
        if (liaact) then
!
! ------- REPERAGE DE LA LIAISON
!
            jdecal = zi(japptr+iliai-1)
            nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
!
! ------- CALCUL DU JEU TANGENT CORRIGE
!
            call cfcglc(ndim, neq, nesmax, resoco, iliai,&
                        jdecal, nbddl, jdepc, japddl, japcof,&
                        glis)
!
! ------- PARAMETRES
!
            coefff = zr(jtacf+ztacf*(iliai-1)+1-1)
            coefpt = zr(jtacf+ztacf*(iliai-1)+3-1)
            coefte = zr(jtacf+ztacf*(iliai-1)+4-1)
!
! ------- LAMBDA DE CONTACT ET DE FROTTEMENT
!
            lambdc = zr(jmu+iliac2-1)
            lambdf = coefff*lambdc
!
! ------- DETERMINATION DE BETA
!
            if (lambdf .eq. 0.d0) then
                beta = 0.d0
            else
                if (glis .le. (glimax*glitol)) glis = glimin
                alpha = sqrt(lambdf/glis)
                beta = sqrt(1.d0/(lambdf*glis))
                if (alpha .gt. sqrt(coefpt)) beta = 0.d0
            endif
!
! ------- ON MULTIPIE BETA PAR COEF_MATR_FROT QUI VAUT 1
! ------- SI LE RESIDU EST PLUS PETIT QUE 1E-3
!
            if (resigr .ge. 1.d-03) then
                beta = sqrt(coefte) * beta
            endif
            call calapr(nbddl, beta, zr(jafmu), zi(japddl+jdecal), zr( jfro2))
        endif
        call jelibe(jexnum(fro2, iliai))
100  end do
!
    call jedema()
!
end subroutine
