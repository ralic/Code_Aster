subroutine rcfon2(quest, jprol, jvale, nbvale, sigy,&
                  e, nu, p, rp, rprim,&
                  c, sieleq, dp)
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
    include 'jeveux.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    character(len=1) :: quest
    integer :: jprol, jvale, nbvale
    real(kind=8) :: e, nu, sigy, p, sieleq, rp, rprim, c, dp
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
! IN  JPROL   : ADRESSE DE L'OBJET .PROL DE LA S.D. FONCTION R(P)
! IN  JVALE   : ADRESSE DE L'OBJET .VALE DE LA S.D. FONCTION R(P)
! IN  NBVALE  : NOMBRE DE VALEURS DE LA FONCTION R(P)
! OUT SIGY    : LIMITE D'ELASTICITE
! IN  E       : MODULE D'YOUNG
! IN  NU      : COEFFICIENT DE POISSON
! IN  P       : VARIABLE INTERNE
! OUT RP      : VALEUR DE R(P).
! OUT RPRIM   : VALEUR DE LA DERIVEE DE R(P) EN P
! IN  C       : CONSTANTE DE PRAGER
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
    real(kind=8) :: p0, rp0, pp, equ, deuxmu, rpm
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
! - LIMITE D'ELASTICITE
    if (quest .eq. 'S') then
        sigy = zr(jr)
        goto 9999
    endif
!
!
! - INITIALISATION
    if (p .lt. 0) call u2mess('F', 'MODELISA6_59')
    tessup = .false.
!
! - PARCOURS JUSQU'A P
    do 10 i = 1, nbvale-1
        if (p .lt. zr(jp+i)) then
            i0 = i-1
            goto 20
        endif
10  end do
    tessup = .true.
    if (pro .eq. 'E') then
        call u2mesg('F', 'MODELISA6_60', 1, nom, 0,&
                    0, 1, p)
    endif
    i0=nbvale-1
20  continue
!
! - CALCUL DES VALEURS DE R(P), R'(P) ET AIRE(P)
!
    if (quest .eq. 'V') then
        if (tessup) then
            if (pro .eq. 'L') then
                rprim = (zr(jr+i0)-zr(jr+i0-1))/ (zr(jp+i0)-zr(jp+i0- 1))
            else
                rprim = 0.d0
            endif
        else
            rprim = (zr(jr+i0+1)-zr(jr+i0))/ (zr(jp+i0+1)-zr(jp+i0))
        endif
!
        p0 = zr(jp+i0)
        rp0 = zr(jr+i0)
        rp = rp0 + rprim*(p-p0) - 1.5d0*c*p
        rprim = rprim - 1.5d0*c
        goto 9999
    endif
!
! - RESOLUTION DE L'EQUATION R(P+DP) + 3/2*(2MU+C) DP = SIELEQ
!
    deuxmu = e/(1+nu)
    do 30 i = i0+1, nbvale-1
        equ = zr(jr+i) + 1.5d0*(deuxmu+c)*(zr(jp+i)-p) - sieleq
        if (equ .gt. 0) then
            i0 = i-1
            goto 40
        endif
30  end do
    tessup = .true.
    if (pro .eq. 'E') then
        call u2mesg('F', 'MODELISA6_60', 1, nom, 0,&
                    0, 1, p)
    endif
    i0 = nbvale-1
40  continue
!
! - CALCUL DES VALEURS DE DP, R(P+DP), R'(P+DP)
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
!
    p0 = zr(jp+i0)
    rp0 = zr(jr+i0)
    rpm = rp0+rprim*(p-p0) -1.5d0*c*p
    dp = (sieleq-rpm)/(1.5d0*deuxmu+rprim)
    pp = p+dp
    rp = rp0 + rprim*(pp-p0)-1.5d0*c*pp
    rprim = rprim - 1.5d0*c
!
9999  continue
end subroutine
