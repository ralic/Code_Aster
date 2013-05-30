subroutine lcumfd(vari, nvari, nstrs, cmat, nmat,&
                  iflu, tdt, hini, hfin, afpd,&
                  bfpd, cfpd)
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ROUTINE APPELE DANS LCUMSD
! LCUMFD     SOURCE    BENBOU   01/03/26
!_______________________________________________________________________
!
! ROUTINE QUI CALCUL LES MATRICES DE DEFORMATION
!   DE FLUAGE PROPRE DEVIATORIQUE
!   D APRES LE MODELE UMLV
!
!     IFLU = O => SORTIE DES VECTEURS TOTAUX : EQUATION (3.4-8)
!     IFLU = 1 => SORTIE DES VECTEURS REVERSIBLES : EQUATION (3.4-9)
!     IFLU = 2 => SORTIE DES VECTEURS IRREVERSIBLES : EQUATION (3.4-10)
!
!
!   DFPRODEV(N+1) = AFPD + BFPD * SIGMA(N) + CFPD * SIGMA(N+1)
!
!        => EQUATION (3.4-1)
!
! IN  VARI     : VARIABLES INTERNES INITIALES
! IN  NVARI    : DIMENSION DES VECTEURS VARIABLES INTERNES
! IN  NSTRS    : DIMENSION DES VECTEURS CONTRAINTE ET DEFORMATION
! IN  CMAT     : VECTEUR DE PARAMETRES (MATERIAU ET AUTRE)
! IN  NMAT     : DIMENSION DE CMAT
! IN  IFLU     : CF COMMENTAIRES CI-DESSUS
! IN  TDT      : PAS DE TEMPS
! IN  HINI     : HUMIDITE INITIALE
! IN  HFIN     : HUMIDITE FINALE
! OUT AFPD     : CF. EQUATION CI-DESSUS
! OUT BFPD     : IDEM
! OUT CFPD     : IDEM
!_______________________________________________________________________
!
!      IMPLICIT REAL*8(A-H,O-Z)
    implicit none
    integer :: i, iflu, ifpo, nstrs, nvari, nmat
    real(kind=8) :: vari(nvari), cmat(nmat)
    real(kind=8) :: afpd(6)
    real(kind=8) :: epsdvr(6), epsdvi(6)
    real(kind=8) :: adi, adr, bdi, bdr, bfpd, cdi, cdr, cfpd
    real(kind=8) :: hfin, hini, hvfin, hvini, rrdv, tdev
    real(kind=8) :: tdexp, tdt, vidv, vrdv
!
! RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
!
    rrdv = cmat(7)
    vrdv = cmat(8)
    vidv = cmat(9)
    ifpo = nint(cmat(13))
!
    hvini = hini
    hvfin = hfin
!
! TEST SI L HUMIDITE RELATIVE DOIT ETRE PRISE EN COMPTE OU NON
!
    if (ifpo .eq. 1) then
        hini = 1.d0
        hfin = 1.d0
    endif
!
! TEST SI TDT = 0
!
    if (tdt .eq. 0.d0) then
        do 11 i = 1, nstrs
            afpd(i) = 0.d0
11      continue
        bfpd = 0.d0
        cfpd = 0.d0
        goto 20
    endif
!
! RECUPERATION DES VARIABLES INTERNES INITIALES
!
!    REMPLISSAGE DES VECTEURS DE DEFORMATION DE FLUAGE DEVIATOIRE
!
!         REVERSIBLE ET IRREVERSIBLE
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
! CONSTRUCTION DE LA MATRICE DEVIATOIRE REVERSIBLE
!
!  MODELE => EQUATION (3.4-2)
!  DISCRETISATION => EQUATION (3.4-3)
!
!  VALEUR DES PARAMETRES => EQUATION (3.4-4)
!
    tdev = vrdv / rrdv
    tdexp = exp(-tdt/tdev)
    adr = tdexp - 1.d0
    bdr = 1.d0/rrdv*(&
          tdexp*(&
          -hini*(2*tdev/tdt+1.d0) + hfin*tdev/tdt) + hini*(2.d0*(tdev-tdt)/tdt+1.d0) - hfin*(tdev&
          &-tdt&
          )/tdt&
          )
    cdr = hini/(tdt*rrdv)*( tdev*tdexp - (tdev - tdt) )
!
! CONSTRUCTION DE LA MATRICE DEVIATOIRE IRREVERSIBLE
!
!  MODELE => EQUATION (3.4-5)
!  DISCRETISATION => EQUATION (3.4-6)
!
!  VALEUR DES PARAMETRES => EQUATION (3.4-7)
!
    adi = 0.d0
    bdi = hfin*tdt/(2.d0*vidv)
    cdi = hini*tdt/(2.d0*vidv)
!
! SAUVEGARDE DES VALEURS SUIVANT LA VALEUR DE IFLU
!
!
!        CONSTRUTION DE LA MATRICE TOTALE DEVIATOIRE
!
!
!  CONSTRUCTION DE LA MATRICE TOTAL :
!
!  * PARTIE REVERSIBLE   : ADR  BDR  CDR  (IFLU = 0)
!          => EQUATION (3.4-8)
!             +
!  * PARTIE IRREVERSIBLE : ADI  BDI  CDI  (IFLU = 1)
!          => EQUATION (3.4-9)
!             =
!  * TOTAL               : AFPD BFPD CFPD (IFLU = 2)
!          => EQUATION (3.4-10)
!
    do 10 i = 1, 6
        if (iflu .eq. 0) then
!
            afpd(i) = adr * epsdvr(i) + adi * epsdvi(i)
            bfpd = bdr + bdi
            cfpd = cdr + cdi
!
        else if (iflu.eq.1) then
!
            afpd(i) = adr * epsdvr(i)
            bfpd = bdr
            cfpd = cdr
!
        else if (iflu.eq.2) then
!
            afpd(i) = adi * epsdvi(i)
            bfpd = bdi
            cfpd = cdi
        endif
!
10  end do
!
20  continue
!
    hini = hvini
    hfin = hvfin
!
end subroutine
