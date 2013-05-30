subroutine betfpp(materf, nmat, elgeom, pc, pt,&
                  nseuil, fc, ft, dfcdlc, dftdlt,&
                  kuc, kut, ke)
    implicit none
!       ================================================================
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
!                   AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
!       CALCUL DES VALEURS DES COURBES D'ADOUCISSEMENT ET DES DERIVES
!       PAR RAPPORT AUX INCREMENTS DE MULTIPLICATEURS PLASTIQUES
!       IN  MATERF :  COEFFICIENTS MATERIAU
!           NMAT   :  DIMENSION MATERF
!           ELGEOM :  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                     AUX LOIS DE COMPORTEMENT
!           PC     :  MULTIPLICATEUR PLASTIQUE EN COMPRESSION
!           PT     :  MULTIPLICATEUR PLASTIQUE EN TRACTION
!           NSEUIL :  SEUIL D'ELASTICITE ACTIVE
!                     NSEUIL = 1  -->  SEUIL COMPRESSION ACTIF
!                     NSEUIL = 2  -->  SEUIL TRACTION ACTIF
!                     NSEUIL = 3  -->  SEUIL COMPRESSION ET TRACTION
!                                                             ACTIFS
!       OUT FC     :  ECCROUISSAGE EN COMPRESSION
!           FT     :  ECCROUISSAGE EN TRACTION
!           DFCDLC :  DERIVE DE LA COURBE D'ADOUCISSEMENT EN COMPRESSION
!           DFTDLT :  DERIVE DE LA COURBE D'ADOUCISSEMENT EN TRACTION
!           KUC    :  ECROUISSAGE ULTIME EN COMPRESSION
!           KUT    :  ECROUISSAGE ULTIME EN TRACTION
!       ----------------------------------------------------------------
    include 'jeveux.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: un, zero, d13, deux
    parameter       ( un    = 1.d0   )
    parameter       ( deux  = 2.d0   )
    parameter       ( zero  = 0.d0   )
    parameter       ( d13   =  .33333333333333D0 )
!
    integer :: nmat, nseuil
    real(kind=8) :: materf(nmat, 2), elgeom(*)
    real(kind=8) :: pc, pt, dfcdlc, dftdlt, kuc, kut
!
    real(kind=8) :: lc, fcp, ftp, fc, ft
    real(kind=8) :: gc, gt, celas
    real(kind=8) :: e, ku, ke
    real(kind=8) :: lc0, epsi
    integer :: typcom, typtra, iadzi, iazk24
    character(len=8) :: nomail
!     ------------------------------------------------------------------
    integer :: n, nd
    common /tdim/   n , nd
    data   epsi     /1.d-6/
!     ------------------------------------------------------------------
!
! --- INITIALISATION
!
    e = materf(1,1)
    fcp = materf(1,2)
    ftp = materf(2,2)
    gc = materf(4,2)
    gt = materf(5,2)
    celas = materf(6,2)
    typcom = int( materf(7,2) + 0.5d0 )
    typtra = int( materf(8,2) + 0.5d0 )
!
    kuc = zero
    ke = zero
    kut = zero
    fc = zero
    ft = zero
    dfcdlc = zero
    dftdlt = zero
!
! --- LONGUEUR CARACTERISTIQUE POUR LOI BETON LC
!
    if (materf(9,2) .lt. zero) then
        lc = elgeom(1)
    else
        lc = materf(9,2)
    endif
!
!
! --- VALEUR ET DERIVEE DE LA COURBE D'ADOUCISSEMENT EN COMPRESSION
!
    if (nseuil .eq. 1 .or. nseuil .eq. 3 .or. nseuil .eq. 11 .or. nseuil .eq. 33) then
!
! -      COURBE POST PIC EN COMPRESSION LINEAIRE
!
        if (typcom .eq. 0) then
            ke = deux * (un - celas) * fcp / e
            ku = deux * gc / (lc * fcp) - (un + deux * celas) * ke * d13
            lc0 = (6.d0 * e * gc) / (fcp * fcp * (11.d0 - 4.d0 * celas * (un + celas)))
            if (lc .gt. lc0) then
                if (materf(9,2) .lt. zero) then
                    call u2mess('A', 'ALGORITH_44')
                else
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3)(1:8)
                    call u2mesk('A', 'ALGORITH_45', 1, nomail)
                endif
            endif
            if (pc .lt. ke) then
                fc = fcp * ( celas + deux*(un - celas)*pc/ke + (celas - un)*pc*pc/(ke*ke) )
                dfcdlc = fcp * deux * (un - celas) * (un - pc/ke) / ke
            else
                if (pc .lt. ku) then
                    fc = fcp * (pc - ku) / (ke - ku)
                    dfcdlc = fcp / (ke - ku)
                else
                    fc = fcp * epsi
!               FC = ZERO
                    dfcdlc = zero
                endif
            endif
!
! -      COURBE POST PIC EN COMPRESSION NON LINEAIRE
!
        else
            ke = deux * (un - celas) * fcp / e
            ku = 1.5d0 * gc / (lc * fcp) - 0.5d0 * celas * ke
            lc0 = (1.5d0 * e * gc) / (fcp * fcp * (4.d0 - celas * (un + celas)))
            if (lc .gt. lc0) then
                if (materf(9,2) .lt. zero) then
                    call u2mess('A', 'ALGORITH_44')
                else
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3)(1:8)
                    call u2mesk('A', 'ALGORITH_45', 1, nomail)
                endif
            endif
            if (pc .lt. ke) then
!               FC = FCP / TROIS
!     1            * (UN +  4.D0*PC/KE - DEUX*PC*PC/(KE*KE))
!               DFCDLC =  (4.D0* FCP)/(TROIS*KE) * (UN - PC/KE)
                fc = fcp * ( celas + deux*(un - celas)*pc/ke + (celas - un)*pc*pc/(ke*ke) )
                dfcdlc = fcp * deux * (un - celas) * (un - pc/ke) / ke
            else if (pc.lt.ku) then
                fc = fcp * (un - (pc - ku)* (pc - ku) /((ke - ku)*(ke - ku)))
                dfcdlc = - deux*fcp*(pc - ku) / ((ke - ku)*(ke - ku))
            else
                fc = fcp * epsi
!               FC = ZERO
                dfcdlc = zero
            endif
        endif
        kuc = ku
    endif
!
! --- VALEUR ET DERIVEE DE LA COURBE D'ADOUCISSEMENT EN TRACTION
!
    if (nseuil .eq. 2 .or. nseuil .eq. 3 .or. nseuil .eq. 22 .or. nseuil .eq. 33) then
!
! -      COURBE POST PIC EN TRACTION LINEAIRE
!
        if (typtra .eq. 0) then
            ku = deux * gt / (lc * ftp)
            lc0 = (deux * e * gt) / (ftp * ftp)
            if (lc .gt. lc0) then
                if (materf(9,2) .lt. zero) then
                    call u2mess('A', 'ALGORITH_46')
                else
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3)(1:8)
                    call u2mesk('A', 'ALGORITH_47', 1, nomail)
                endif
            endif
            if (pt .lt. ku) then
                ft = ftp * (un - pt / ku)
                dftdlt = - ftp / ku
            else
                ft = ftp * epsi
!               FT = ZERO
                dftdlt = zero
            endif
!
! -      COURBE POST PIC EN TRACTION NON LINEAIRE
!
        else
            ku = 1.d06
            lc0 = (e * gt) / (ftp * ftp)
            if (lc .gt. lc0) then
                if (materf(9,2) .lt. zero) then
                    call u2mess('A', 'ALGORITH_46')
                else
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3)(1:8)
                    call u2mesk('A', 'ALGORITH_47', 1, nomail)
                endif
            endif
            ft = ftp * exp( - lc * ftp * pt / gt)
            dftdlt = - ftp * ftp * lc / gt * exp( - lc * ftp * pt / gt)
        endif
        kut = ku
!
    endif
!
end subroutine
