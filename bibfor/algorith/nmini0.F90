subroutine nmini0(zpmet, zpcri, zconv, zpcon, znmeth,&
                  fonact, parmet, parcri, conv, parcon,&
                  method, eta, numins, matass, zmeelm,&
                  zmeass, zveelm, zveass, zsolal, zvalin,&
                  sdimpr)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
! TOLE CRP_21
!
    implicit none
    include 'jeveux.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/nmchai.h'
    include 'asterfort/obcrea.h'
    integer :: zpmet, zpcri, zconv
    integer :: zpcon, znmeth
    integer :: fonact(*)
    real(kind=8) :: parmet(zpmet), parcri(zpcri), conv(zconv)
    real(kind=8) :: parcon(zpcon)
    character(len=16) :: method(znmeth)
    character(len=19) :: matass
    character(len=24) :: sdimpr
    integer :: numins
    real(kind=8) :: eta
    integer :: zmeelm, zmeass, zveelm, zveass, zsolal, zvalin
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATIONS)
!
! PREMIERES INITIALISATIONS DE MECA_NON_LINE: MISES A ZERO
!
! ----------------------------------------------------------------------
!
! IN  SDIMPR : SD AFFICHAGE
!
!
!
!
    real(kind=8) :: zero
    parameter    (zero=0.d0)
    integer :: i, long
!
! ----------------------------------------------------------------------
!
!
! --- FONCTIONNALITES ACTIVEES               (NMFONC/ISFONC)
!
    do 2 i = 1, 100
        fonact(i) = 0
 2  end do
!
! --- PARAMETRES DES METHODES DE RESOLUTION  (NMDOMT)
!
    do 3 i = 1, zpmet
        parmet(i) = zero
 3  end do
!
! --- PARAMETRES DES CRITERES DE CONVERGENCE (NMLECT)
!
    do 4 i = 1, zpcri
        parcri(i) = zero
 4  end do
!
! --- INFORMATIONS SUR LA CONVERGENCE DU CALCUL
!
    do 5 i = 1, zconv
        conv (i) = r8vide()
 5  end do
    conv(1) = -1
!
! --- PARAMETRES DU CRITERE DE CONVERGENCE EN CONTRAINTE (NMLECT)
!
    do 7 i = 1, zpcon
        parcon(i) = zero
 7  end do
!
! --- METHODES DE RESOLUTION
!
    do 8 i = 1, znmeth
        method(i) = ' '
 8  end do
!
! --- INITIALISATION BOUCLE EN TEMPS
!
    numins = 0
    eta = 0.d0
    matass = '&&OP0070.MATASS'
!
! --- VERIF. LONGUEURS VARIABLES CHAPEAUX (SYNCHRO OP0070/NMCHAI)
!
    call nmchai('MEELEM', 'LONMAX', long)
    call assert(long.eq.zmeelm)
    call nmchai('MEASSE', 'LONMAX', long)
    call assert(long.eq.zmeass)
    call nmchai('VEELEM', 'LONMAX', long)
    call assert(long.eq.zveelm)
    call nmchai('VEASSE', 'LONMAX', long)
    call assert(long.eq.zveass)
    call nmchai('SOLALG', 'LONMAX', long)
    call assert(long.eq.zsolal)
    call nmchai('VALINC', 'LONMAX', long)
    call assert(long.eq.zvalin)
!
! --- CREATION SD AFFICHAGE
!
    call obcrea('AFFICHAGE', sdimpr)
!
end subroutine
