subroutine lcplnf(rela_comp, vind, nbcomm, nmat, cpmono,&
                  materd, materf, iter, nvi, itmax,&
                  toler, pgl, nfs, nsg, toutms,&
                  hsr, dt, dy, yd, yf,&
                  vinf, tampon, sigd, sigf,&
                  deps, nr, mod, timef,&
                  indi, vins, codret)
!
! aslint: disable=W1504
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------
!   POST-TRAITEMENTS SPECIFIQUES AUX LOIS
!
!   CORRESPONDANCE ENTRE LES VARIABLES INTERNES ET LES EQUATIONS
!          DU SYSTEME DIFFERENTIEL APRES INTEGRATION
!
!   CAS GENERAL :
!      COPIE DES YF DANS VINF
!      LA DERNIERE C'EST TOUJOURS L'INDICATEUR PLASTIQUE
!
!   CAS PARTICULIER DU  MONOCRISTAL  :
!       ON GARDE 1 VARIABLE INTERNE PAR SYSTEME DE GLISSEMENT SUR 3
!       DEFORMATION PLASTIQUE EQUIVALENTE CUMULEE MACROSCOPIQUE
! ----------------------------------------------------------------
!  IN
!     LOI    :  NOM DE LA LOI
!     VIND   :  VARIABLE INTERNES A T
!     MATERD :  COEF MATERIAU A T
!     MATERF :  COEF MATERIAU A T+DT
!     NBCOMM :  INCIDES DES COEF MATERIAU
!     NMAT   :  DIMENSION MATER ET DE NBCOMM
!     NVI    :  NOMBRE DE VARIABLES INTERNES
!     DT     : INCREMENT DE TEMPS
!     NR     : DIMENSION VECTEUR INCONNUES (YF/DY)
!     YF     : EQUATIONS DU COMPORTEMENT INTEGRES A T+DT
!     DY     : INCREMENT DES VARIABLES INTERNES
!     TIMED  : INSTANT T
!     TIMEF  : INSTANT T+DT
!     INDI   : INDICATEUR MECANIQMES POT. ACTIFS (HUJEUX)
!     VINS   : VARIABLES INTERNES A T (ORIGINAL - HUJEUX)
!  OUT
!     VINF   :  VARIABLES INTERNES A T+DT
! ----------------------------------------------------------------
#include "asterfort/burlnf.h"
#include "asterfort/hujlnf.h"
#include "asterfort/irrlnf.h"
#include "asterfort/lcdpec.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lkilnf.h"
    integer :: ndt, nvi, nmat, ndi, nbcomm(nmat, 3), iter, itmax, nr, codret
    integer :: nfs, nsg, indi(7), i
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), vins(nvi), timef
    real(kind=8) :: pkc, m13, dtot, hookf(6, 6)
    real(kind=8) :: yd(*), vind(*), toler, pgl(3, 3), dt, tampon(*)
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg), dy(*), yf(*), vinf(*)
    character(len=16) :: rela_comp
    character(len=24) :: cpmono(5*nmat+1)
    character(len=8) :: mod
    real(kind=8) :: sigf(6), deps(*), sigd(6)
!
    common /tdim/   ndt  , ndi
! --- -------------------------------------------------------------
!
!     MISE A JOUR DE SIGF , VINF
    call lceqvn(ndt, yf(1), sigf)
!
    if (rela_comp(1:8) .eq. 'MONOCRIS') then
! ---    DEFORMATION PLASTIQUE EQUIVALENTE CUMULEE MACROSCOPIQUE
        call lcdpec(vind, nbcomm, nmat, ndt, cpmono,&
                    materf, iter, nvi, itmax, toler,&
                    pgl, nfs, nsg, toutms, hsr,&
                    dt, dy, yd, vinf, tampon,&
                    sigf, deps, nr, mod,&
                    codret)
!
    else if (rela_comp(1:7).eq.'IRRAD3M') then
        call irrlnf(nmat, materf, yf(ndt+1), 1.0d0, vinf)
    else if (rela_comp(1:15) .eq. 'BETON_BURGER_FP') then
        call burlnf(nvi, vind, nmat, materd, materf,&
                    dt, nr, yd, yf, vinf,&
                    sigf)
    else if (rela_comp(1:4) .eq. 'LETK') then
        call lkilnf(nvi, vind, nmat, materf, dt,&
                    sigd, nr, yd, yf, deps,&
                    vinf)
    else if (rela_comp .eq. 'HAYHURST') then
!        DEFORMATION PLASTIQUE CUMULEE
        vinf(7) = yf(ndt+1)
!        H1
        vinf(8) = yf(ndt+2)
!        H2
        vinf(9) = yf(ndt+3)
!        PHI
        pkc=materf(11,2)
        m13=-1.d0/3.d0
        vinf(10)=1.d0-(1.d0+pkc*timef)**m13
!        DEFORMATION PLASTIQUE
!        D
        vinf(11) = yf(ndt+4)
        dtot=(1.d0-vinf(11))
        call lcopli('ISOTROPE', mod, materf(1, 1), hookf)
        call lcprmv(hookf, yf, sigf)
        call lcprsv(dtot, sigf, sigf)
        do i = 1, ndt
            vinf(i) = yf(i)
        end do
        vinf(nvi) = iter
    else if (rela_comp(1:6) .eq. 'HUJEUX') then
        call hujlnf(toler, nmat, materf, nvi, vind,&
                    vinf, vins, nr, yd, yf,&
                    sigd, sigf, indi, codret)
    else
!        CAS GENERAL :
        call lceqvn(nvi-1, yf(ndt+1), vinf)
        vinf(nvi) = iter
    endif
!
end subroutine
