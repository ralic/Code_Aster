subroutine rcfonc(quest, ktrac, jprol, jvale, nbvale,&
                  sigy, e, nu, p, rp,&
                  rprim, airerp, sieleq, dp)
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
    implicit none
#include "jeveux.h"
#include "asterfort/utmess.h"
    character(len=1) :: quest
    integer :: jprol, jvale, nbvale
    real(kind=8) :: e, nu, sigy, p, sieleq, rp, rprim, airerp, dp
    integer :: ktrac
! ----------------------------------------------------------------------
!     INTERPOLATION SUR UNE FONCTION DE TYPE R(P)
!
! IN  QUEST   : 'S' -> CALCUL DE LA LIMITE ELASTIQUE
!                 OUT: SIGY
!               'V' -> CALCUL DE RP, RPRIM ET AIRERP
!                 IN: P  OUT: RP,RPRIM,AIRERP
!               'E' -> CALCUL DE L'INCREMENT DP, RACINE DE L'EQUATION
!                 IN : E(T+),NU(T+),P-,SIELEQ+
!                 OUT: RP(P+),RPRIM(P+),AIRERP(P+),DP
! IN  KTRAC  : 1 -> 'TRACTION'
!              2 -> 'META_TRACTION'
!              3 -> 'META_TRAC_ZIRC'
! IN  JPROL   : ADRESSE DE L'OBJET .PROL DE LA S.D. FONCTION R(P)
! IN  JVALE   : ADRESSE DE L'OBJET .VALE DE LA S.D. FONCTION R(P)
! IN  NBVALE  : NOMBRE DE VALEURS DE LA FONCTION R(P)
! OUT SIGY    : LIMITE D'ELASTICITE
! IN  E       : MODULE D'YOUNG
! IN  NU      : COEFFICIENT DE POISSON
! IN  P       : VARIABLE INTERNE
! OUT RP      : VALEUR DE R(P).
! OUT RPRIM   : VALEUR DE LA DERIVEE DE R(P) EN P
! OUT AIRERP  : AIRE SOUS LA COURBE R(P)
! IN  SIELEQ  : CONTRAINTE ELASTIQUE EQUIVALENTE
! OUT DP      : INCREMENT DE DEFORMATION PLASTIQUE CUMULEE.
!
!
!
!
    logical :: tessup
    character(len=1) :: pro
    character(len=24) :: nom
    integer :: jp, jr, i, i0
    real(kind=8) :: troimu, p0, rp0, pp, equ
!
!
! - DESCRIPTIF DE LA COURBE R(P)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    jp = jvale
    jr = jvale + nbvale
    pro = zk24(jprol+4)(2:2)
    nom = zk24(jprol+5)
!
!
! - LIMITE D'ELASTICITE
    if (quest .eq. 'S') then
        if (ktrac .eq. 1) then
            sigy = zr(jr)
            goto 9999
        else
            call utmess('F', 'MODELISA6_62')
        endif
    endif
!
! - INITIALISATION
    if (p .lt. 0) then
        call utmess('F', 'MODELISA6_59')
    endif
!
! DISTINCTION ENTRE TRACTION ET META_TRACTION CAR
! POUR TRACTION:C EST LA COURBE SIGMA(P)
! LE PREMIER POINT CORRESPOND A LA LIMITE D ELASTICITE
! POUR META_TRACTION:C EST LA COURBE R(P)
! DONC IL Y A UN COUPLE DE MOINS QUE DANS TRACTION
!
    if (ktrac .eq. 1) then
        airerp = 0.d0
        tessup = .false.
!
! - PARCOURS JUSQU'A P
        do 10 i = 1, nbvale-1
            if (p .lt. zr(jp+i)) then
                i0 = i-1
                goto 20
            endif
            airerp = airerp+(zr(jr+i)+zr(jr+i-1))* (zr(jp+i)-zr(jp+i- 1))
10      continue
        tessup = .true.
        if (pro .eq. 'E') then
            call utmess('F', 'MODELISA6_60', sk=nom, sr=zr(jp-1+nbvale))
        endif
        i0=nbvale-1
20      continue
!
! - CALCUL DES VALEURS DE R(P), R'(P) ET AIRE(P)
!
        if (quest .eq. 'V') then
            if (tessup) then
                if (pro .eq. 'L') then
                    rprim = (zr(jr+i0)-zr(jr+i0-1))/ (zr(jp+i0)-zr(jp+ i0-1))
                else
                    rprim = 0.d0
                endif
            else
                rprim = (zr(jr+i0+1)-zr(jr+i0))/ (zr(jp+i0+1)-zr(jp+ i0))
            endif
            p0 = zr(jp+i0)
            rp0 = zr(jr+i0)
            rp = rp0 + rprim*(p-p0)
            airerp = (airerp+(rp0+rp)*(p-p0))*0.5d0
            goto 9999
        endif
    endif
!
    if (ktrac .eq. 2) then
        airerp = 0.d0
        tessup = .false.
!
! - PARCOURS JUSQU'A P
        do 11 i = 1, nbvale
            if (p .lt. zr(jp+i-1)) then
                i0 = i-1
                goto 21
            endif
11      continue
        tessup = .true.
        if (pro .eq. 'E') then
            call utmess('F', 'MODELISA6_60', sk=nom, sr=zr(jp-1+nbvale))
        endif
        i0=nbvale-1
21      continue
!
! - CALCUL DES VALEURS DE R(P) ET R'(P)
!
        if (quest .eq. 'V') then
            if (tessup) then
                if (pro .eq. 'L') then
                    rprim = (zr(jr+i0)-zr(jr+i0-1))/ (zr(jp+i0)-zr(jp+ i0-1))
                else
                    rprim = 0.d0
                endif
                rp = zr(jr+i0)+ rprim*(p-zr(jp+i0))
            else
                if (i0 .eq. 0) then
                    rprim = zr(jr)/zr(jp)
                    p0 = 0.d0
                    rp0 = 0.d0
                else
                    rprim = (zr(jr+i0)-zr(jr+i0-1))/ (zr(jp+i0)-zr(jp+ i0-1))
                    p0 = zr(jp+i0-1)
                    rp0 = zr(jr+i0-1)
                endif
                rp = rp0 + rprim*(p-p0)
            endif
            goto 9999
        endif
    endif
!
! - RESOLUTION DE L'EQUATION R(P+DP) + 3 MU DP = SIELEQ
!
    troimu = 1.5d0*e/(1+nu)
    do 30 i = i0+1, nbvale-1
        equ = zr(jr+i) + troimu*(zr(jp+i)-p) - sieleq
        if (equ .gt. 0) then
            i0 = i-1
            goto 40
        endif
        airerp = airerp+(zr(jr+i)+zr(jr+i-1))* (zr(jp+i)-zr(jp+i-1))
30  end do
    tessup = .true.
    if (pro .eq. 'E') then
        call utmess('F', 'MODELISA6_60', sk=nom, sr=zr(jp-1+nbvale))
    endif
    i0 = nbvale-1
40  continue
!
! - CALCUL DES VALEURS DE DP, R(P+DP), R'(P+DP) ET AIRE(P+DP)
!
    if (tessup) then
        if (pro .eq. 'L') then
            rprim = (zr(jr+i0)-zr(jr+i0-1))/ (zr(jp+i0)-zr(jp+i0-1))
        else
            rprim = 0.d0
        endif
    else
        rprim = (zr(jr+i0+1)-zr(jr+i0))/ (zr(jp+i0+1)-zr(jp+i0))
    endif
    p0 = zr(jp+i0)
    rp0 = zr(jr+i0)
    dp = (sieleq-rp0-rprim*(p-p0))/(troimu+rprim)
    pp = p+dp
    rp = rp0 + rprim*(pp-p0)
    airerp = (airerp+(rp0+rp)*(pp-p0))*0.5d0
!
9999  continue
end subroutine
