subroutine lcumfs(vari, nvari, cmat, nmat, iflu,&
                  isph, tdt, hini, hfin, afps,&
                  bfps, cfps)
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
!
! ROUTINE APPELE DANS LCUMSD
! LCUMFS     SOURCE    BENBOU   01/03/26
!
!_______________________________________________________________________
!
! ROUTINE QUI CALCUL LES MATRICES DE DEFORMATION
!   DE FLUAGE PROPRE SPHERIQUE
!   D APRES LE MODELE UMLV
!
!     IFLU = O => SORTIE DES VALEURS TOTALES
!     IFLU = 1 => SORTIE DES VALEURS REVERSIBLES
!     IFLU = 2 => SORTIE DES VALEURS IRREVERSIBLES
!
!
!
!   DFPROSPH(N+1) = AFPS + BFPS * SIGMA(N) + CFPS * SIGMA(N+1)
!
! IN  VARI     : VARIABLES INTERNES INITIALES
! IN  NVARI    : DIMENSION DES VECTEURS VARIABLES INTERNES
! IN  CMAT     : VECTEUR DE PARAMETRES (MATERIAU ET AUTRE)
! IN  NMAT     : DIMENSION DE CMAT
! IN  IFLU     : CF COMMENTAIRES CI-DESSUS
! IN  ISPH     : MODE DE CALCUL DE LA PARTIE SPHERIQUE
! IN  TDT      : PAS DE TEMPS
! IN  HINI     : HUMIDITE INITIALE
! IN  HFIN     : HUMIDITE FINALE
! OUT AFPS     : CF. EQUATION CI-DESSUS
! OUT BFPS     : IDEM
! OUT CFPS     : IDEM
!_______________________________________________________________________
!
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/lcump1.h'
    integer :: nvari, nmat
    integer :: iflu, ifpo, isph
    real(kind=8) :: afps, bfps, cfps
    real(kind=8) :: vari(nvari), cmat(nmat)
    real(kind=8) :: ambda1, ambda2
    real(kind=8) :: asi, asr, bsi, bsr, csi, csr, deno, dh, eisp, ersp
    real(kind=8) :: hfin, hini, hvfin, hvini
    real(kind=8) :: risp, rrsp, tdt, tsexp, tsph, var1
    real(kind=8) :: var2, vrsp, x1, x2
!
! RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    rrsp = cmat(3)
    vrsp = cmat(4)
    risp = cmat(5)
!     VISP = CMAT(6)
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
    dh = hfin - hini
!
! TEST SI TDT = 0
!
    afps = 0.d0
    bfps = 0.d0
    cfps = 0.d0
    if (tdt .eq. 0.d0) then
        afps = 0.d0
        bfps = 0.d0
        cfps = 0.d0
        goto 10
    endif
!
! RECUPERATION DES VARIABLES INTERNES INITIALES
!
    ersp = vari(1)
    eisp = vari(2)
!
    if (isph .eq. 0) then
!
! LA DEFORMATION DE FLUAGE PROPRE IRREVERSIBLE N EST PAS PRIS EN COMPTE
!
!
!    CONSTRUCTION DE LA MATRICE SPHERIQUE REVERSIBLE
!
!          => EQUATION (3.6-4) ET (3.6-5)
!
        tsph = vrsp / rrsp
        tsexp = exp(-tdt/tsph)
        asr = (tsexp - 1.d0) * ersp
        bsr = 1.d0/rrsp*(&
              tsexp*(&
              -hini*(2*tsph/tdt+1.d0) + hfin*tsph/ tdt) + hini*(2.d0*(tsph-tdt)/tdt+1.d0) - hfin*&
              &(tsph-tdt&
              )/tdt&
              )
        csr = hini/(tdt*rrsp)*( tsph*tsexp - (tsph - tdt) )
!
!    CONSTRUCTION DE LA MATRICE SPHERIQUE IRREVERSIBLE
!
!          => EQUATION (3.6-6)
!
        asi = 0.d0
        bsi = 0.d0
        csi = 0.d0
!
    else
!
! LA DEFORMATION DE FLUAGE PROPRE IRREVERSIBLE EST PRISE EN COMPTE
!
!    CONSTRUCTION DE LA MATRICE SPHERIQUE REVERSIBLE
!
!          => EQUATION (3.6-8) ET (3.6-9)
!
!      CALCUL DE PARAMETRES MATERIAUX
!
        call lcump1(cmat, nmat, ambda1, ambda2, x1,&
                    x2)
        deno = x1 * x2 - 1.d0
!
        call assert(deno.ne.0.d0)
!
!
        var1 = (x1*x2*exp(ambda1*tdt) - exp(ambda2*tdt))/deno - 1.d0
        var2 = (x1*( exp(ambda1*tdt) - exp(ambda2*tdt) ))/deno
!
        asr = var1 * ersp - var2 * eisp
        bsr = dh* ( -var1/rrsp + var2/risp )
        csr = hini* ( -var1/rrsp + var2/risp )
!
!    CONSTRUCTION DE LA MATRICE SPHERIQUE IRREVERSIBLE
!
!          => EQUATION (3.6-8) ET (3.6-10)
!
!
        var1 = x2 * ( exp(ambda1*tdt) - exp(ambda2*tdt) )/deno
        var2 = ( exp(ambda1*tdt) - x1*x2*exp(ambda2*tdt))/deno + 1.d0
!
        asi = var1 * ersp - var2 * eisp
        bsi = dh* ( -var1/rrsp + var2/risp )
        csi = hini* ( -var1/rrsp + var2/risp )
    endif
!
!  CONSTRUCTION DE LA MATRICE TOTAL :
!
!  * PARTIE REVERSIBLE   : ASR  BSR  CSR  (IFLU = 0)
!        => EQUATION (3.6-11)
!             +
!  * PARTIE IRREVERSIBLE : ASI  BSI  CSI  (IFLU = 1)
!        => EQUATION (3.6-12)
!             =
!  * TOTAL               : AFPS BFPS CFPS (IFLU = 2)
!        => EQUATION (3.6-13)
!
    if (iflu .eq. 0) then
        afps = asr + asi
        bfps = bsr + bsi
        cfps = csr + csi
    else if (iflu.eq.1) then
        afps = asr
        bfps = bsr
        cfps = csr
    else if (iflu.eq.2) then
        afps = asi
        bfps = bsi
        cfps = csi
    endif
!
10  continue
!
    hini = hvini
    hfin = hvfin
!
end subroutine
