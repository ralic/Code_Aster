subroutine d1cro2(zimat, nmnbn, nmplas, nmdpla, nmddpl,&
                  nmprox, cnbn, cplas, rpara, cief,&
                  cdeps, cdtg, cier, cdepsp, dc,&
                  bend)
    implicit  none
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DU MULTIPLICATEUR PLASTIQUE
!     ET DE L INCREMENT DE COURBURE PLASTIQUE
!     DANS LE CAS OU 1 CRITERE PLASTIQUE EST ACTIVE
!     METHODE EXPLICITE AVEC UNE CONDITION DE SECOND ORDRE
!
! IN  ZIMAT : ADRESSE DE LA LISTE DE MATERIAU CODE
! IN  NMNBN : FORCE - BACKFORCE
! IN  NMPLAS : MOMENTS LIMITES DE PLASTICITE
! IN  NMDPLA : DERIVEES DES MOMENTS LIMITES DE PLASTICITE
! IN  NMDDPL : DERIVEES SECONDES DES MOMENTS LIMITES DE PLASTICITE
! IN  NMPROX : NMPROX > 0 : NBN DANS ZONE DE CRITIQUE
! IN  CDTG : MATRICE TANGENTE
! IN  DC : MATRICE ELASTIQUE + CONSTANTES DE PRAGER
! IN  BEND : FLEXION POSITIVE (1) OU NEGATIVE (-1)
!
! IN/OUT RPARA : LISTES DE PARAMETRES DE TYPE ENTIER
!
! OUT CNBN : NOUVELLE FORCE - BACKFORCE
! OUT CPLAS : NOUVEAUX MOMENTS LIMITES DE PLASTICITE
! OUT CIEF : NOUVEAU CIEF > 0 : NBN HORS DE LA ZONE DE DEFINITION DE MP
! OUT CDEPS : NOUVEL INCREMENT DE DEFORMATION DANS LE REPERE ORTHO
! OUT CIER : NOUVEAU CODE ERREUR
! OUT CDEPSP : NOUVEL INCREMENT DE DEF PLASTIQUE DANS LE REPERE ORTHO
!
    include 'asterfort/dfplgl.h'
    include 'asterfort/dfuuss.h'
    include 'asterfort/draac2.h'
    include 'asterfort/fplass.h'
    include 'asterfort/hplass.h'
    include 'asterfort/matmul.h'
    include 'asterfort/nmnet1.h'
    include 'asterfort/r8inir.h'
    integer :: bend, nbxx, i, j
    integer :: nmprox(2), cief, cier, zimat
!
    real(kind=8) :: nmnbn(6), nmplas(2, 3), nmdpla(2, 2), nmddpl(2, 2)
    real(kind=8) :: cnbn(6), cplas(2, 3), czef, czeg
    real(kind=8) :: cdeps(6), cdtg(6, 6), cdepsp(6)
    real(kind=8) :: dc(6, 6), normm, cp1(1, 6), cp0(1), tdcu(1, 6), h(6, 6)
    real(kind=8) :: df(6)
    real(kind=8) :: lambda, u(6), a0(1), a1(1), a2(1), xx(2), dcu(6), hdcu(6)
    real(kind=8) :: tdf(1, 6), ddeps(6), tddeps(1, 6), rpara(3)
!
    czef = rpara(1)
    czeg = rpara(2)
    normm = rpara(3)
!
!     CALCUL LA MATRICE HESSIENNE DU CRITERE DE PLASTICITE
    call hplass(nmnbn, nmplas, nmdpla, nmddpl, bend,&
                h)
!     CALUL DES DIRECTIONS DE L ECOULEMENT DES DEFORMATIONS PLASTIQUES
    call dfuuss(nmnbn, nmplas, nmdpla, nmprox, bend,&
                u)
!     CALCUL LE GRADIENT DU CRITERE DE PLASICITE
    call dfplgl(nmnbn, nmplas, nmdpla, bend, df)
!
    do 10, j = 1,6
    tdf(1,j) = df(j)
    10 end do
!
    call matmul(cdtg, cdeps, 6, 6, 1,&
                ddeps)
!
    do 20, j = 1,6
    tddeps(1,j) = ddeps(j)
    20 end do
!
    call matmul(dc, u, 6, 6, 1,&
                dcu)
!
    do 30, j = 1,6
    tdcu(1,j) = dcu(j)
    30 end do
!
    call matmul(h, dcu, 6, 6, 1,&
                hdcu)
    call matmul(tddeps, h, 1, 6, 6,&
                cp1)
!
    do 40, j = 1,6
    cp1(1,j) = tdf(1,j) + 0.5d0*cp1(1,j)
    40 end do
!
    call matmul(cp1, ddeps, 1, 6, 1,&
                cp0)
!
    a0(1) = fplass(nmnbn,nmplas,bend) + cp0(1)
!
    call matmul(tdf, dcu, 1, 6, 1,&
                a1)
    call matmul(tddeps, hdcu, 1, 6, 1,&
                cp0)
    a1(1) = -a1(1) - cp0(1)
    call matmul(tdcu, hdcu, 1, 6, 1,&
                cp0)
    a2(1) = 0.5d0 * cp0(1)
!
!     RESOLUTION DE L EQUATION DU SECOND DEGRE
    call draac2(a2, a1, a0, xx(1), xx(2),&
                nbxx)
!
    do 54, i = 1, nbxx
    if (xx(i) .ge. 0.d0) then
        lambda=xx(i)
!
        do 50, j = 1,6
        cdepsp(j) = lambda * u(j)
50      continue
!
!     CALCUL DE CNBN ET CDEPSP QUAND UN CRITERE PLASTIQUE EST ACTIVE
        call nmnet1(zimat, nmnbn, cnbn, cplas, czef,&
                    czeg, cief, cdeps, cdtg, cier,&
                    cdepsp, dc, normm)
!
        if (cier .eq. 0) goto 60
    endif
    54 end do
!
    cier=3
!
    call r8inir(6, 0.0d0, cdepsp, 1)
!
60  continue
!
    rpara(1) = czef
    rpara(2) = czeg
    rpara(3) = normm
end subroutine
