subroutine lcumsf(sigi, sigf, nstrs, vari, nvari,&
                  cmat, nmat, isph, tdt, hini,&
                  hfin, varf)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
! ROUTINE APPELE DANS FLU
! LCUMSF     SOURCE    BENBOU   01/03/26
!
!_______________________________________________________________________
!
! ROUTINE QUI CALCULE LA DEFORMATION DE FLUAGE DE DESSICCATION
! ET DE FLUAGE PROPRE A LA FIN DU PAS DE TEMPS
! ELLES SONT SAUVEGARDES DANS VARF(NVARI)
!
! IN  SIGI     : CONTRAINTES INITIALES
! IN  SIGF     : CONTRAINTES FINALES
! IN  NSTRS    : DIMENSION DES VECTEURS CONTRAINTE ET DEFORMATION
! IN  VARI     : VARIABLES INTERNES INITIALES
! IN  NVARI    : DIMENSION DES VECTEURS VARIABLES INTERNES
! IN  CMAT     : VECTEUR DE PARAMETRES (MATERIAU ET AUTRE)
! IN  NMAT     : DIMENSION DE CMAT
! IN  ISPH     : MODE DE CALCUL DE LA PARTIE SPHERIQUE
! IN  TDT      : PAS DE TEMPS
! IN  HINI     : HUMIDITE INITIALE
! IN  HFIN     : HUMIDITE FINALE
!
! OUT VARF     : VARIABLES INTERNES FINALES
!_______________________________________________________________________
!
    implicit none
    include 'asterc/r8prem.h'
    include 'asterfort/lcumfb.h'
    include 'asterfort/lcumfd.h'
    include 'asterfort/lcumfs.h'
    integer :: i, icou, ifou, ifpo, ides, isph, j, nmat, nstrs, nvari
    real(kind=8) :: ersp, eisp
    real(kind=8) :: varf(20), cmat(15), sigf(nstrs), sigi(nstrs)
! MODIFI DU 6 JANVIER 2003 - YLP SUPPRESSION DES DECLARATIONS
! IMPLICITES DES TABLEAUX
!      REAL*8 AFD(NSTRS),BFD(NSTRS,NSTRS),CFD(NSTRS,NSTRS)
!      REAL*8 AFPD(NSTRS),SIGFI(NSTRS),VARI(20)
    real(kind=8) :: afd(6), bfd(6, 6), cfd(6, 6)
    real(kind=8) :: afpd(6), sigfi(6), vari(20)
    real(kind=8) :: efde(6), epsdvr(6), epsdvi(6), sigidv(6), sigfdv(6)
    real(kind=8) :: afps, bfps, bfpd, cfpd, cfps, deisp, hfin, hini
    real(kind=8) :: sigfsp, sigisp, tdt, visp
!
! RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
!
!
    ifou = nint(cmat(12))
    ifpo = nint(cmat(13))
    ides = nint(cmat(14))
    icou = nint(cmat(15))
!
! --- VISCOSITE SPHERIQUE IRREVERSIBLE
    visp = cmat(6)
!
! RECUPERATION DES VARIABLES INTERNES INITIALES
!
!   FLUAGE PROPRE
!
!     - SPHERIQUE  : ERSP EISP
!     - DEVIATOIRE : EPSDVR EPSDVI
!
!   FLUAGE DE DESSICCATION : EFDE
!
    ersp = vari(1)
    eisp = vari(2)
!
    epsdvr(1) = vari(3)
    epsdvi(1) = vari(4)
    epsdvr(2) = vari(5)
    epsdvi(2) = vari(6)
    epsdvr(3) = vari(7)
    epsdvi(3) = vari(8)
    epsdvr(4) = vari(12)
    epsdvi(4) = vari(13)
    epsdvr(5) = vari(14)
    epsdvi(5) = vari(15)
    epsdvr(6) = vari(16)
    epsdvi(6) = vari(17)
!
    efde(1) = vari(9)
    efde(2) = vari(10)
    efde(3) = vari(11)
    efde(4) = vari(18)
    efde(5) = vari(19)
    efde(6) = vari(20)
!
! INITIALISATION DES VARIABLES
!
! MOFI DU 6 JANVIER 2003 - YLP NSTRS --> 6
!      DO 11 I=1,NSTRS
!        AFPD(I)  = 0.D0
!        AFD(I)   = 0.D0
!        SIGFI(I) = 0.D0
!        DO 12 J=1,NSTRS
!          BFD(I,J)  = 0D0
!          CFD(I,J)  = 0D0
!12      CONTINUE
!11    CONTINUE
    do 11 i = 1, 6
        afpd(i) = 0.d0
        afd(i) = 0.d0
        sigfi(i) = 0.d0
        do 12 j = 1, 6
            bfd(i,j) = 0d0
            cfd(i,j) = 0d0
12      continue
11  end do
    do 13 i = 1, 6
        sigidv(i) = 0.d0
        sigfdv(i) = 0.d0
13  end do
!
! TEST DE COUPLAGE AVEC ICOU
!
    if (icou .eq. 0) then
        do 10 i = 1, nstrs
            sigfi(i) = sigi(i)
10      continue
    else
        do 20 i = 1, nstrs
            sigfi(i) = sigf(i)
20      continue
    endif
!
! CALCUL DES CONTRAINTES SPHERIQUES INI ET FIN
!
!  => EQUATION (3.11-1)
!
    sigisp = (sigi(1) + sigi(2)) / 3.d0
    sigfsp = (sigfi(1) + sigfi(2)) / 3.d0
!
    if (ifou .ne. -2) then
        sigisp = sigi(3) / 3.d0 + sigisp
        sigfsp = sigfi(3) / 3.d0 + sigfsp
    endif
!
! CALCUL DES CONTRAINTES DEVIATOIRES INI ET FIN
!
!  => EQUATION (3.11-2)
!
    sigidv(1) = sigi(1) - sigisp
    sigidv(2) = sigi(2) - sigisp
!
    sigfdv(1) = sigfi(1) - sigfsp
    sigfdv(2) = sigfi(2) - sigfsp
!
    if (ifou .ne. -2) then
        sigidv(3) = sigi(3) - sigisp
        sigidv(4) = sigi(4)
!
        sigfdv(3) = sigfi(3) - sigfsp
        sigfdv(4) = sigfi(4)
!
        if (ifou .eq. 2) then
            sigidv(5) = sigi(5)
            sigidv(6) = sigi(6)
!
            sigfdv(5) = sigfi(5)
            sigfdv(6) = sigfi(6)
        endif
    else
        sigidv(3) = sigi(3)
!
        sigfdv(3) = sigfi(3)
    endif
!_______________________________________________________________________
!
!  FLUAGE DE DESSICCATION : CALCUL DES MATRICES
!
! --> DEBRANCHE le 11 octobre 2002 - ylp.
! --> REBRANCHE le 25 aout 2004 - ylp.
!_______________________________________________________________________
!
    if ((ides.eq.1) .or. (ides.eq.2)) then
        call lcumfb(sigi, nstrs, vari, nvari, cmat,&
                    nmat, tdt, hini, hfin, afd,&
                    bfd, cfd)
    endif
!  CALCUL DES DEFORMATIONS FINALES FLUAGE DE DESSICCATION
!
!  => EQUATION (3.11-3)
!
    do 30 i = 1, nstrs
        efde(i) = afd(i) + efde(i) + bfd(i,i) * sigi(i) + cfd(i,i) * sigfi(i)
30  continue
!
!  FLUAGE PROPRE
!
    if (ifpo .ne. 0) then
!
!   FLUAGE PROPRE SPHERIQUE REVERSIBLE ET IRREVERSIBLE
!
!  => EQUATION (3.11-4)
!
        call lcumfs(vari, nvari, cmat, nmat, 1,&
                    isph, tdt, hini, hfin, afps,&
                    bfps, cfps)
        ersp = ersp + afps + bfps*sigisp + cfps*sigfsp
!
        call lcumfs(vari, nvari, cmat, nmat, 2,&
                    isph, tdt, hini, hfin, afps,&
                    bfps, cfps)
        deisp = afps + bfps*sigisp + cfps*sigfsp
!
!     TEST SUR LA CROISSANCE DE LA DEFORMATION DE FLUAGE
!          PROPRE SPHERIQUE
!
        if (isph .ne. 0) then
            if (deisp .gt. 0.d0) then
                isph = 2
                goto 60
            endif
            if ((sigfsp/visp.gt.-r8prem()) .and. (sigisp/visp.gt.- r8prem())) then
                isph = 2
                goto 60
            endif
        endif
!
        eisp = eisp + deisp
!
!   FLUAGE PROPRE DEVIATORIQUE REVERSIBLE ET IRREVERSIBLE
!
!  => EQUATION (3.11-5)
!
        call lcumfd(vari, nvari, nstrs, cmat, nmat,&
                    1, tdt, hini, hfin, afpd,&
                    bfpd, cfpd)
        do 40 i = 1, nstrs
            epsdvr(i) = epsdvr(i) + afpd(i)+ bfpd*sigidv(i) + cfpd* sigfdv(i)
40      continue
        call lcumfd(vari, nvari, nstrs, cmat, nmat,&
                    2, tdt, hini, hfin, afpd,&
                    bfpd, cfpd)
        do 50 i = 1, nstrs
            epsdvi(i) = epsdvi(i) + afpd(i) + bfpd*sigidv(i) + cfpd* sigfdv(i)
50      continue
    endif
!
!  SAUVEGARDE DES DEFORMATIONS DE FLUAGE
!
!    PROPRE
!
!      - SPHERIQUE
!
    varf(1) = ersp
    varf(2) = eisp
!
!      - DEVIATORIQUE
!
    varf(3) = epsdvr(1)
    varf(4) = epsdvi(1)
    varf(5) = epsdvr(2)
    varf(6) = epsdvi(2)
    varf(7) = epsdvr(3)
    varf(8) = epsdvi(3)
    varf(12) = epsdvr(4)
    varf(13) = epsdvi(4)
    varf(14) = epsdvr(5)
    varf(15) = epsdvi(5)
    varf(16) = epsdvr(6)
    varf(17) = epsdvi(6)
!
!    DESSICCATION
!
    varf(9) = efde(1)
    varf(10) = efde(2)
    varf(11) = efde(3)
    varf(18) = efde(4)
    varf(19) = efde(5)
    varf(20) = efde(6)
!
60  continue
!
end subroutine
