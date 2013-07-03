subroutine lcummd(vari, nvari, cmat, nmat, sigm,&
                  nstrs, isph, tdt, hini, hfin,&
                  an, bn, cn, cfps, cfpd)
!
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
! ROUTINE APPELE DANS NCUMLVFP
! LCUMMD     SOURCE    BENBOU   01/03/26
!-----------------------------------------------------------------------
!_______________________________________________________________________
!
! ROUTINE QUI CALCUL LA MATRICE DE DEFORMATION
!   DE FLUAGE DE DESSICCATION ET DE FLUAGE PROPRE
!
! LA VARIATION DE DEFORMATION DE FLUAGE EST CALCULEE
! AVEC LA RELATION SUIVANTE
!      EPS_N+1 - EPS_N = AN + BN:SIG_N+1 + CN:SIG_N
!
!     EQUATION (3.9-1)
!
! IN  VARI     : VARIABLES INTERNES INITIALES
! IN  NVARI    : DIMENSION DES VECTEURS VARIABLES INTERNES
! IN  CMAT     : VECTEUR DE PARAMETRES (MATERIAU ET AUTRE)
! IN  NMAT     : DIMENSION DE CMAT
! IN  NSTRS    : DIMENSION DES VECTEURS CONTRAINTE ET DEFORMATION
! IN  ISPH     : MODE DE CALCUL DE LA PARTIE SPHERIQUE
! IN  TDT      : PAS DE TEMPS
! IN  HINI     : HUMIDITE INITIALE
! IN  HFIN     : HUMIDITE FINALE
! OUT AN       : CF EQUATION CI-DESSUS
! OUT BN       : CF EQUATION CI-DESSUS
! OUT CN       : CF EQUATION CI-DESSUS
! OUT CFPS     : COEFFICICIENT DE CN SPHERIQUE    (MATRICE TANGENTE)
! OUT CFPD     : COEFFICICIENT DE CN DEVIATORIQUE (MATRICE TANGENTE)
!_______________________________________________________________________
!
    implicit none
#include "asterfort/lcumfb.h"
#include "asterfort/lcumsd.h"
    integer :: nvari, nmat, nstrs, ifpo, isph, ides, i, j
! MODIFI DU 6 JANVIER 2003 - YLP SUPPRESSION DES DECLARATIONS
! IMPLICITES DES TABLEAUX
!      REAL*8 VARI(NVARI),CMAT(NMAT)
    real(kind=8) :: vari(nvari), cmat(nmat)
! MODIFI DU 6 JANVIER 2003 - YLP SUPPRESSION DES DECLARATIONS
! IMPLICITES DES TABLEAUX
!      REAL*8 AN(NSTRS),BN(NSTRS,NSTRS),CN(NSTRS,NSTRS)
!      REAL*8 AFD(NSTRS),BFD(NSTRS,NSTRS),CFD(NSTRS,NSTRS)
!      REAL*8 AFP(NSTRS),BFP(NSTRS,NSTRS),CFP(NSTRS,NSTRS)
    real(kind=8) :: sigm(6)
    real(kind=8) :: an(6), bn(6, 6), cn(6, 6)
    real(kind=8) :: afd(6), bfd(6, 6), cfd(6, 6)
    real(kind=8) :: afp(6), bfp(6, 6), cfp(6, 6)
    real(kind=8) :: cfpd, cfps, hfin, hini, tdt
!
! RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
!
    ifpo = nint(cmat(13))
    ides = nint(cmat(14))
!
! INITIALISATION DES VARIABLES
!
! MODIFI DU 6 JANVIER 2003 6 YLP NSTRS --> 6
!      DO 11 I=1,NSTRS
    do 11 i = 1, 6
        afd(i) = 0.d0
        afp(i) = 0.d0
!        DO 12 J=1,NSTRS
        do 12 j = 1, 6
            bfp(i,j) = 0d0
            bfd(i,j) = 0d0
            cfp(i,j) = 0d0
            cfd(i,j) = 0d0
12      continue
11  end do
!
!_______________________________________________________________________
!
!  FLUAGE DE DESSICCATION
!
! --> DEBRANCHE le 11 octobre 2002 - ylp.
!
!      IF ((IDES.EQ.1).OR.(IDES.EQ.2)) THEN
!        CALL FLUDES(SIGI,NSTRS,VARI,NVARI,CMAT,NMAT,TDT,HINI,HFIN,
!     $              AFD,BFD,CFD)
!      ENDIF
! --> REBRANCHE le 25 aout 2004 - ylp.
    if ((ides.eq.1) .or. (ides.eq.2)) then
        call lcumfb(sigm, nstrs, vari, nvari, cmat,&
                    nmat, tdt, hini, hfin, afd,&
                    bfd, cfd)
    endif
!
!  FLUAGE PROPRE
!
    if (ifpo .ne. 0) then
        call lcumsd(vari, nvari, cmat, nmat, nstrs,&
                    isph, tdt, hini, hfin, afp,&
                    bfp, cfp, cfps, cfpd)
    endif
!
!  CONSTRUCTION DE LA MATRICE DE FLUAGE TOTAL : EQUATION (3.9-2)
!
    do 10 i = 1, nstrs
        an(i) = afd(i) + afp(i)
        do 10 j = 1, nstrs
            bn(i,j) = bfd(i,j) + bfp(i,j)
            cn(i,j) = cfd(i,j) + cfp(i,j)
10      continue
!
end subroutine
