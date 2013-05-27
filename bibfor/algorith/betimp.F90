subroutine betimp(nmat, mater, sig, vind, vinf,&
                  elgeom, nseui1, nseui2, nseui3, nseui4,&
                  sige, sigd)
    implicit none
!       ================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       BETON_DOUBLE_DP: CONVEXE ELASTO PLASTIQUE POUR (MATER,SIG,P1,P2)
!            AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
!            SEUILC = FCOMP      = (SIGEQ   + A  SIGH)/B - FC
!            SEUILT = FTRAC      = (SIGEQ   + C  SIGH)/D - FT
!                AVEC SIGEQ      = SQRT(3/2(D) (D)) (CONTR EQUIVALENTE)
!                     D          = SIG - 1/3 TR(SIG) I
!                     SIGH       = 1/3 TR(SIG)    (CONTR HYDROSTATIQUE)
!       IMPRESSION DE VALEURS EN CAS DE NON CONVERGENCE
!       ----------------------------------------------------------------
!       NSEUIL = 1  --> CRITERE  EN COMPRESSION ACTIVE
!       NSEUIL = 2  --> CRITERE  EN TRACTION ACTIVE
!       NSEUIL = 3  --> CRITERES EN COMPRESSION ET EN TRACTION ACTIVE
!       NSEUIL = 11 --> PROJECTION AU SOMMET DU CONE DE COMPRESSION
!       NSEUIL = 22 --> PROJECTION AU SOMMET DU CONE DE TRACTION
!       NSEUIL = 33 --> PROJECTION AU SOMMET DES CONES DE COMPRESSION
!                       ET TRACTION
!       ----------------------------------------------------------------
!       IN  SIG    :  CONTRAINTE
!       IN  VIND   :  VARIABLES INTERNES = ( PC PT THETA ) A T
!       IN  VINF   :  VARIABLES INTERNES = ( PC PT THETA ) A T+DT
!       IN  NMAT   :  DIMENSION MATER
!       IN  MATER  :  COEFFICIENTS MATERIAU
!       IN  ELGEOM :  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES AUX
!                     LOIS DE COMPORTEMENT
!       IN  NSEUI1 :  CRITERE ACTIVE LORS DE LA PREMIERE RESOLUTION
!       IN  NSEUI2 :  CRITERE ACTIVE LORS DE LA DEUXIEME RESOLUTION
!       IN  NSEUI3 :  CRITERE ACTIVE LORS DE LA TROISIEME RESOLUTION
!       IN  NSEUI4 :  CRITERE ACTIVE LORS DE LA QUATRIEME RESOLUTION
!       IN  SIGE   :  CONTRAINTE ELASTIQUE
!       IN  SIGD   :  CONTRAINTE A L'INSTANT PRECEDENT
!       ----------------------------------------------------------------
    include 'jeveux.h'
    include 'asterfort/betfpp.h'
    include 'asterfort/infniv.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lchydr.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/tecael.h'
    integer :: nmat, nseui4, ifm, niv
    integer :: nseui1, nseui2, nseui3
    real(kind=8) :: pc, pt, sig(6), sige(6), sigd(6), dev(6), lc
    real(kind=8) :: mater(nmat, 2), elgeom(*), vind(*), vinf(*)
    real(kind=8) :: fc, ft, beta
    real(kind=8) :: rac2, zero, un, deux, trois
    real(kind=8) :: ke, fcomp, ftrac
    real(kind=8) :: a, b, c, d
    real(kind=8) :: sigeq, sigh, p, dfcdlc, dftdlt, kuc, kut
    real(kind=8) :: d13, dlambc, dlambt
    integer :: iadzi, iazk24
    character(len=8) :: nomail
!       ---------------------------------------------------------------
    integer :: ndt, ndi
    common /tdim/   ndt , ndi
!       ----------------------------------------------------------------
!
    data   d13      /.33333333333333D0 /
    data   zero     / 0.d0 /
    data   un       / 1.d0 /
    data   deux     / 2.d0 /
    data   trois    / 3.d0 /
    rac2 = sqrt (deux)
!
    call infniv(ifm, niv)
    if (niv .eq. 2) then
    endif
!
    call tecael(iadzi, iazk24)
    nomail = zk24(iazk24-1+3)(1:8)
!
! ---   IMPRESSION GENERALE
!
!
! ---   CARACTERISTIQUES MATERIAU
!
    beta = mater(3,2)
!
    a = rac2 * (beta - un) / (deux * beta - un)
    b = rac2 / trois * beta / (deux * beta - un)
    c = rac2
    d = deux * rac2 / trois
!
!
! --- LONGUEUR CARACTERISTIQUE POUR LOI BETON LC
!
    if (mater(9,2) .lt. zero) then
        lc = elgeom(1)
    else
        lc = mater(9,2)
    endif
!
    1000  format(a43)
    1001  format(a43,a8)
    1002  format(a43,1pd12.5)
    1003  format(4(a9,i3))
!
! ---   TRAITEMENT DE LA CONTRAINTE ELASTIQUE
!
    call lcdevi(sige, dev)
    call lcprsc(dev, dev, p)
    sigeq = sqrt (1.5d0 * p)
!
    call lchydr(sige, sigh)
!
    pc = vind(1)
    pt = vind(2)
    call betfpp(mater, nmat, elgeom, pc, pt,&
                3, fc, ft, dfcdlc, dftdlt,&
                kuc, kut, ke)
!
    fcomp = (rac2 * d13 * sigeq + a * sigh) / b - fc
    ftrac = (rac2 * d13 * sigeq + c * sigh) / d - ft
!
    write(ifm,1000) ' -----------------------------------------  '
    write(ifm,1000) ' NON CONVERGENCE DE LA LOI BETON_DOUBLE_DP '
    write(ifm,1000) ' -----------------------------------------  '
    write(ifm,1001) 'MAILLE :                                   ',&
     &                   nomail
    write(ifm,1002) 'LONGUEUR CARACTERISTIQUE :                 ',&
     &                   lc
    write(ifm,1002) 'CONTRAINTE ELASTIQUE EQUIVALENTE :         ',&
     &                   sigeq
    write(ifm,1002) 'CONTRAINTE ELASTIQUE HYDROSTATIQUE :       ',&
     &                   sigh
    write(ifm,1002) 'ECROUISSAGE EN COMPRESSION :               ',&
     &                   pc
    write(ifm,1002) 'ECROUISSAGE EN TRACTION :                  ',&
     &                   pt
    write(ifm,1002) 'RESISTANCE COMPRESSION AVANT ECROUISSAGE : ',&
     &                   fc
    write(ifm,1002) 'RESISTANCE TRACTION AVANT ECROUISSAGE :    ',&
     &                   ft
    write(ifm,1002) 'ECROUISSAGE ULTIME EN COMPRESSION :        ',&
     &                   kuc
    write(ifm,1002) 'ECROUISSAGE AU PIC EN COMPRESSION :        ',&
     &                   ke
    write(ifm,1002) 'ECROUISSAGE ULTIME EN TRACTION :           ',&
     &                   kut
    write(ifm,1002) 'VALEUR DU CRITERE DE COMPRESSION :         ',&
     &                   fcomp
    write(ifm,1002) 'VALEUR DU CRITERE DE TRACTION :            ',&
     &                   ftrac
!
! ---   TRAITEMENT DE LA CONTRAINTE (NON ELASTIQUE) A L'INSTANT MOINS
!
    call lcdevi(sigd, dev)
    call lcprsc(dev, dev, p)
    sigeq = sqrt (1.5d0 * p)
!
    call lchydr(sigd, sigh)
!
    write(ifm,1002) 'CONTRAINTE EQUIVALENTE A L INSTANT MOINS : ',&
     &                   sigeq
    write(ifm,1002) 'CONTRAINTE HYDROSTATIQUE A L INSTANT MOINS:',&
     &                   sigh
!
! ---   TRAITEMENT DE LA CONTRAINTE ELASTO PLASTIQUE
!
    call lcdevi(sig, dev)
    call lcprsc(dev, dev, p)
    sigeq = sqrt (1.5d0 * p)
!
    call lchydr(sig, sigh)
!
    pc = vinf(1)
    pt = vinf(2)
    call betfpp(mater, nmat, elgeom, pc, pt,&
                3, fc, ft, dfcdlc, dftdlt,&
                kuc, kut, ke)
!
    fcomp = (rac2 * d13 * sigeq + a * sigh) / b - fc
    ftrac = (rac2 * d13 * sigeq + c * sigh) / d - ft
    dlambc = vinf(1) - vind(1)
    dlambt = vinf(2) - vind(2)
!
    write(ifm,1003) 'NSEUI1 : ',nseui1,' NSEUI2 : ',nseui2,&
     &                 ' NSEUI3 : ',nseui3,' NSEUI4 : ',nseui4
    write(ifm,1002) 'CONTRAINTE EQUIVALENTE A L INSTANT PLUS :  ',&
     &                   sigeq
    write(ifm,1002) 'CONTRAINTE HYDROSTATIQUE A L INSTANT PLUS :',&
     &                   sigh
    write(ifm,1002) 'ECROUISSAGE EN COMPRESSION A L INST. PLUS :',&
     &                   pc
    write(ifm,1002) 'ECROUISSAGE EN TRACTION A L INSTANT PLUS:  ',&
     &                   pt
    write(ifm,1002) 'RESISTANCE COMPRESSION APRES ECROUISSAGE : ',&
     &                   fc
    write(ifm,1002) 'RESISTANCE TRACTION APRES ECROUISSAGE :    ',&
     &                   ft
    write(ifm,1002) 'VALEUR DU CRITERE DE COMPRESSION :         ',&
     &                   fcomp
    write(ifm,1002) 'VALEUR DU CRITERE DE TRACTION :            ',&
     &                   ftrac
    write(ifm,1002) 'INCREMENT DE DEFO. PLAS. EN COMPRESSION :  ',&
     &                   dlambc
    write(ifm,1002) 'INCREMENT DE DEFO. PLAS. EN TRACTION :     ',&
     &                   dlambt
    write(ifm,1000) ' -----------------------------------------  '
!
!
end subroutine
