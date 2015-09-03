subroutine posipr(nomte, efge, sipo)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --------------------------------------------------------------------------------------------------
!
!     CALCUL DU VECTEUR ELEMENTAIRE CONTRAINTE REEL ('SIPO_ELNO')
!     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    real(kind=8) :: sipo(*), efge(12)
    character(len=*) :: nomte
!
#include "jeveux.h"
#include "asterfort/tecach.h"
#include "asterfort/poutre_modloc.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: itsec, lrcou, iret
    real(kind=8) :: a, a2, alfay, alfay2, alfaz, alfaz2, aredy
    real(kind=8) :: aredy2, aredz, aredz2, deux, hy1, hy2, hz1
    real(kind=8) :: hz2, r1, r2, rt, rt2, ry, ry2
    real(kind=8) :: rz, rz2, xfly, xflz, xiy, xiy2
    real(kind=8) :: xiz, xiz2, xjx, xjx2, xsiy, xsiz
    real(kind=8) :: xxy, xxz, zero
    aster_logical :: lcoude
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cara = 18
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1','JX1','RY1','RZ1','RT1',&
                    'A2','IY2','IZ2','AY2','AZ2','JX2','RY2','RZ2','RT2'/
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cara1 = 7
    real(kind=8) :: vale_cara1(nb_cara1)
    character(len=8) :: noms_cara1(nb_cara1)
    data noms_cara1 /'HY1','HZ1','HY2','HZ2','R1','R2','TSEC'/
!
! --------------------------------------------------------------------------------------------------
!
    zero = 0.d0
    deux = 2.d0
    lcoude = .false.
!
!   recuperation des caracteristiques generales des sections
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
!   SECTION INITIALE
    a      = vale_cara(1)
    xiy    = vale_cara(2)
    xiz    = vale_cara(3)
    alfay  = vale_cara(4)
    alfaz  = vale_cara(5)
    xjx    = vale_cara(6)
    ry     = vale_cara(7)
    rz     = vale_cara(8)
    rt     = vale_cara(9)
!   SECTION FINALE
    a2     = vale_cara(10)
    xiy2   = vale_cara(11)
    xiz2   = vale_cara(12)
    alfay2 = vale_cara(13)
    alfaz2 = vale_cara(14)
    xjx2   = vale_cara(15)
    ry2    = vale_cara(16)
    rz2    = vale_cara(17)
    rt2    = vale_cara(18)
!
    if (nomte .eq. 'MECA_POU_D_TG') then
        a2 = a
    endif
!
    xfly=1.0d0; xflz=1.0d0; xsiy=1.0d0; xsiz=1.0d0
    if (nomte .eq. 'MECA_POU_D_E') then
        alfay  = zero
        alfaz  = zero
        alfay2 = zero
        alfaz2 = zero
    else if (nomte .eq. 'MECA_POU_D_T') then
        call tecach('ONN', 'PCAARPO', 'L', iret, iad=lrcou)
        if ( iret .eq. 0 ) then
            lcoude = .true.
            xfly = zr(lrcou)
            xsiy = zr(lrcou+1)
            xflz = zr(lrcou+2)
            xsiz = zr(lrcou+3)
!           prise en compte de l'indice de flexibilité
            xiy  = xiy/xfly
            xiz  = xiz/xflz
            xiy2 = xiy2/xfly
            xiz2 = xiz2/xflz
!           prise en compte de l'indice de contraintes
            xxy  = xsiy/xfly
            xxz  = xsiz/xflz
            xiy  = xiy/xxy
            xiz  = xiz/xxz
            xiy2 = xiy2/xxy
            xiz2 = xiz2/xxz
        endif
    endif
!
!   caracteristiques des sections cercle et rectangle
    call poutre_modloc('CAGEPO', noms_cara1, nb_cara1, lvaleur=vale_cara1)
    itsec = nint(vale_cara1(7))
!
!   caracteristiques des sections d extremite de l element
    aredy  = a
    aredz  = a
    aredy2 = a2
    aredz2 = a2
    if (alfay  .ne. zero) aredy  = a/alfay
    if (alfaz  .ne. zero) aredz  = a/alfaz
    if (alfay2 .ne. zero) aredy2 = a2/alfay2
    if (alfaz2 .ne. zero) aredz2 = a2/alfaz2
!
!     1 = SN   = SXX CALCULE A PARTIR DE L'EFFORT NORMAL
!     2 = SVY  = SXY DU A L'EFFORT TRANCHANT VY
!     3 = SVZ  = SXZ DU A L'EFFORT TRANCHANT VZ
!     4 = SMT  = SXY ET SXZ  DUS AU MOMENT DE TORSION MT
!     5 = SMFY = SXX CALCULE A PARTIR DU MOMENT DE FLEXION MFY
!     6 = SMFZ = SXX CALCULE A PARTIR DU MOMENT DE FLEXION MFZ
!
    sipo(1)  = -efge(1)/a
    sipo(2)  = -efge(2)/aredy
    sipo(3)  = -efge(3)/aredz
    sipo(4)  = -efge(4)/xjx*rt
!
    sipo(7)  =  efge(7)/a2
    sipo(8)  =  efge(8)/aredy2
    sipo(9)  =  efge(9)/aredz2
    sipo(10) =  efge(10)/xjx2*rt2
!
!   contraintes dues aux moments de flexion changement de signe entre les noeuds 1 et 2
!   pour ne pas changer l'orientation du repere local
!
!   section rectangulaire : on donne les valeurs aux points (hy/2,0) et (0,hz/2)
    if (itsec .eq. 1) then
        hy1 = vale_cara1(1)
        hz1 = vale_cara1(2)
        hy2 = vale_cara1(3)
        hz2 = vale_cara1(4)
        sipo(5)  = -efge(5)/xiy*hz1/deux
        sipo(6)  =  efge(6)/xiz*hy1/deux
        sipo(11) =  efge(11)/xiy2*hz2/deux
        sipo(12) = -efge(12)/xiz2*hy2/deux
!
!   section circulaire : on donne les valeurs aux points (hy/2,0) et (0,hz/2)
    else if (itsec.eq.2) then
        r1 = vale_cara1(5)
        r2 = vale_cara1(6)
        sipo(5)  = -efge(5)/xiy*r1
        sipo(6)  =  efge(6)/xiz*r1
        sipo(11) =  efge(11)/xiy2*r2
        sipo(12) = -efge(12)/xiz2*r2
!
!   section generale : on donne sxx au point (ry,rz)
    else if (itsec.eq.0) then
        sipo(5)  = -efge(5)/xiy*rz
        sipo(6)  =  efge(6)/xiz*ry
        sipo(11) =  efge(11)/xiy2*rz2
        sipo(12) = -efge(12)/xiz2*ry2
    endif
!
    if (lcoude) then
        xxy = xsiy/xfly
        xxz = xsiz/xflz
        sipo(5)  = sipo(5)*xxy
        sipo(6)  = sipo(6)*xxz
        sipo(11) = sipo(11)*xxy
        sipo(12) = sipo(12)*xxz
    endif
!
end subroutine
