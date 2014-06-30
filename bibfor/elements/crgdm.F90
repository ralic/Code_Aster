subroutine crgdm(imate, compor, lambda, deuxmu, lamf, deumuf, gt, gc, gf, seuil,&
                 alpha, alfmc, ep, lrgm, ipg, ther, tref, dtmoy, dtgra, tmoym, tgram, alph)
! person_in_charge: sebastien.fayolle at edf.fr
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
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
    logical(kind=1) :: lrgm, ther
    integer :: imate, ipg
    real(kind=8) :: lambda, deuxmu, deumuf, lamf
    real(kind=8) :: gt, gc, gf, seuil, alpha, alfmc
    real(kind=8) :: tref, dtmoy, dtgra, tmoyp, tmoym, tgrap, tgram, alph
    character(len=16) :: compor, phenom
! ----------------------------------------------------------------------
!
! BUT : LECTURE DES PARAMETRES MATERIAU POUR LE MODELE GLRC_DM
!
!
! IN:
!       IMATE   : ADRESSE DU MATERIAU
!       COMPOR  : COMPORTMENT
!       EP      : EPAISSEUR DE LA PLAQUE
! OUT:
!       LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
!       DEUXMU  : PARAMETRE D ELASTICITE - MEMBRANE
!       LAMF    : PARAMETRE D ELASTICITE - FLEXION
!       DEUMUF  : PARAMETRE D ELASTICITE - FLEXION
!       GT      : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION
!       GC      : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
!       GF      : PARAMETRE GAMMA POUR LA FLEXION
!       SEUIL   : INITIAL MEMBRANE
!       ALPHA   : PARAMETRE DE SEUIL FLEXION
!       ALFMC   : PARAMETRE DE DECOUPLAGE SEUILS TRACTION-COMPRESSION
! ----------------------------------------------------------------------
!
    integer :: icodre(7), iret, iret1, iretm
    real(kind=8) :: valres(7), e, nu, ep, ef, nycmax, rmesg(2)
    real(kind=8) :: nyt, nyc, myf, nuf, delas(6, 6), t1p, t2p, t3p
    real(kind=8) :: t1m, t2m, t3m
    character(len=4) :: fami
    character(len=8) :: nomres(7)
!
    ther=.false.
    if ((.not.( compor(1:7) .eq. 'GLRC_DM'))) then
        call utmess('F', 'ELEMENTS4_65', sk=compor)
    endif
!
    call r8inir(6*6, 0.0d0, delas, 1)
    call r8inir(7, 0.0d0, valres, 1)
    phenom = 'GLRC_DM         '
    if (phenom .ne. 'GLRC_DM         ') then
        call utmess('F', 'ELEMENTS2_32', sk=phenom)
    endif
!
!     EPAISSEUR
!      CALL JEVECH('PCACOQU','L',JCOQU)
!      EPAIS = ZR(JCOQU)
!      NOMRES(1)  = 'EPAIS'
!      CALL RCVALA(IMATE,' ',PHENOM,0,' ',R8B,1,NOMRES,
!     &              VALRES,CODRES,'FM')
!      IF(VALRES(1) .NE. EPAIS) THEN
!        VALRES(2) = EPAIS
!        CALL U2MESG('F','ELEMENTS5_42',0,' ',0,0,2,VALRES)
!      ENDIF
!
    fami='RIGI'
!
    call rcvarc(' ', 'TEMP_INF', '+', fami, ipg, 1, t2p, iret)
    call rcvarc(' ', 'TEMP_SUP', '+', fami, ipg, 1, t3p, iret)
    if (iret .eq. 0) then
        call rcvarc(' ', 'TEMP', '+', fami, ipg, 1, t1p, iretm)
! SI TEMP N'EST PAS FOURNI ON MET LA MOYENNE DE TSUP ET TINF
        if (iretm .ne. 0) then
            t1p=(t2p+t3p)/2.d0
        endif
        tmoyp=(4.d0*t1p+t2p+t3p)/6.d0
        tgrap=(t3p-t2p)/ep
        call rcvarc(' ', 'TEMP', 'REF', fami, 1, 1, tref, iret1)
        if (iret1 .eq. 0) then
            ther=.true.
        endif
    endif
    if (ther) then
        call rcvarc(' ', 'TEMP_INF', '-', fami, ipg, 1, t2m, iret)
        call rcvarc(' ', 'TEMP_SUP', '-', fami, ipg, 1, t3m, iret)
        if (iret .eq. 0) then
            call rcvarc(' ', 'TEMP', '-', fami, ipg, 1, t1m, iretm)
! SI TEMP N'EST PAS FOURNI ON MET LA MOYENNE DE TSUP ET TINF
            if (iretm .ne. 0) then
                t1m=(t2m+t3m)/2.d0
            endif
            tmoym=(4.d0*t1m+t2m+t3m)/6.d0
            tgram=(t3m-t2m)/ep
            dtmoy=tmoyp-tmoym
            dtgra=tgrap-tgram
        else
            call utmess('F', 'ALGORITH17_30')
        endif
    endif
!
!    LECTURE DES CARACTERISTIQUES DU MATERIAU
    nomres(1) = 'E_M'
    nomres(2) = 'NU_M'
!
    if (iret .eq. 0) then
        nomres(3) = 'ALPHA'
        call rcvala(imate, ' ', 'ELAS_GLRC', 1, 'TEMP', [tmoyp], 3, nomres, valres, icodre, 1)
        alph = valres(3)
!
    else
        call rcvala(imate, ' ', 'ELAS_GLRC', 0, ' ', [0.d0], 2, nomres, valres, icodre, 1)
!
    endif
    e = valres(1)
    nu = valres(2)
    lambda = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)*ep
    deuxmu = e/(1.d0+nu)*ep
!
    nomres(1) = 'E_F'
    nomres(2) = 'NU_F'
!
    call rcvala(imate, ' ', 'ELAS_GLRC', 0, ' ', [0.d0], 2, nomres, valres, icodre, 0)
!
    if (icodre(1) .eq. 0) then
        ef = valres(1)
    else
        ef = e
    endif
!
    if (icodre(2) .eq. 0) then
        nuf = valres(2)
    else
        nuf = nu
    endif
!
    lamf = ef*nuf/(1.d0-nuf*nuf) *ep**3/12.0d0
    deumuf = ef/(1.d0+nuf) *ep**3/12.0d0
!
!    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
    nomres(1) = 'GAMMA_T'
    nomres(2) = 'GAMMA_C'
    nomres(3) = 'GAMMA_F'
    nomres(4) = 'NYT'
    nomres(5) = 'NYC'
    nomres(6) = 'MYF'
    nomres(7) = 'ALPHA_C'
    call rcvala(imate, ' ', 'GLRC_DM', 0, ' ', [0.d0], 7, nomres, valres, icodre, 0)
!
    gt = valres(1)
    gf = valres(3)
    nyt = valres(4)
    myf = valres(6)
    alfmc = valres(7)
!
    if (icodre(2) .eq. 0 .and. icodre(5) .eq. 0) then
! - ON EST DANS LE CAS DE DEFI_GLRC
        gc = valres(2)
        nyc = valres(5)
    else if (icodre(2).eq.0) then
! - ON EST DANS LE CAS DE DEFI_MATERIAU
        gc = valres(2)
!
        if (gc .eq. 1.d0 .and. gt .eq. 1.d0) then
            call utmess('F', 'ALGORITH6_1')
        endif
!
        nyc = (1.d0-nu)*(1.d0+2.d0*nu)*(1.d0-gt)+nu**2*(1.d0-gc)
        nyc = nyc/((1.d0-nu)*(1.d0+2.d0*nu)*(1.d0-gc)+nu**2*(1.d0-gt))
        nyc = -sqrt(nyc*nyt**2)
    else if (icodre(5).eq.0) then
! - ON EST DANS LE CAS DE DEFI_MATERIAU
        nyc = valres(5)
        nycmax = nyt*sqrt((1.d0-nu)*(1.d0+2.d0*nu))/nu
!
        if (valres(5) .gt. nycmax) then
            rmesg(1) = nyc
            rmesg(2) = nycmax
            call utmess('F', 'ALGORITH6_2', nr=2, valr=rmesg)
        endif
!
        gc = (1.d0-gt)*(nyt**2*(1.d0-nu)*(1.d0+2.d0*nu)-nyc**2*nu**2)
        gc = gc/(nyc**2*(1.d0-nu)*(1.d0+2.d0*nu)-nyt**2*nu**2)
        gc = 1.d0 - gc
    else
        gc = 1.d0
    endif
!
    if (gc .eq. 1.d0 .and. gt .eq. 1.d0) then
        call utmess('F', 'ALGORITH6_1')
    endif
!
    if (icodre(7) .eq. 0 .and. gc .ne. 1.d0) then
        alfmc = valres(7)
    else
        if (gc .eq. 1.d0) then
            alfmc = 1.d0
        else
            alfmc=(1.d0-gc)*(nyc**2*(1.d0-nu)*(1.d0+2.d0*nu)/nyt**2-nu**2)&
                /((1.d0-gt)*((1.d0-nu)*(1.d0+2.d0*nu)-(nu*nyc/nyt)**2))
        endif
    endif
!
!    CALCUL DU SEUIL (k0 DANS R7.01.32) ET DE ALPHA
    if (lrgm) then
        alpha = 1.d0
        alfmc = 1.d0
        seuil = 0.d0
    else
        seuil = lambda*(1.0d0 - gt)*(1.0d0-2.0d0*nu)**2&
              + deuxmu*( 1.0d0 - gt + (1.0d0 - gc)*nu**2/alfmc)
!
        seuil = seuil/(2.0d0*(lambda*(1.0d0-2.0d0*nu) + deuxmu))**2
        seuil = seuil*nyt**2
!
        if (seuil .ne. 0.d0) then
            alpha = lamf*(1.0d0-nuf)**2 + deumuf
            alpha = alpha/(2.0d0*(lamf*(1.0d0-nuf) + deumuf)**2)
            alpha = alpha*(1.0d0 - gf)*myf**2/seuil
        else
            call utmess('F', 'ALGORITH6_3')
        endif
    endif
!
end subroutine
