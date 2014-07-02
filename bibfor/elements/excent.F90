subroutine excent(sens, excen, nbpoin, nbcmp, lreel,&
                  reffin, reffou, ceffin, ceffou)
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
    character(len=*) :: sens
    integer :: nbpoin, nbcmp
    real(kind=8) :: excen, reffin(*), reffou(*)
    complex(kind=8) :: ceffin(*), ceffou(*)
    aster_logical :: lreel
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
!  BUT : TENIR COMPTE DE L'EXCENTREMENT D'UNE COQUE POUR CHANGER
!        LE "PLAN" DE CALCUL DES EFFORTS
!  ARGUMENTS :
!     IN   K*   SENS :
!           'MOY'  : MAIL -> MOY
!           'MAIL' : MOY  -> MAIL
!     IN   R(*) REFFIN : EFFORTS (REELS) "IN"
!     OUT  R(*) REFFOU : EFFORTS (REELS) "OUT"
!     IN   C(*) CEFFIN : EFFORTS (COMPLEXES) "IN"
!     OUT  C(*) CEFFOU : EFFORTS (COMPLEXES) "OUT"
!     IN   I    NBPOIN : NOMBRE DE POINTS DE GAUSS OU DE NOEUDS
!     IN   I    NBCMP  : NOMBRE DE CMPS DES EFFORTS (PAR POINT)
!     IN   L    LREEL  : .TRUE.  : LES EFFORTS SONT REELS
!                        .FALSE. : LES EFFORTS SONT COMPLEXES
!  REMARQUE :
!     ON PEUT APPELER CETTE ROUTINE AVEC LE MEME TABLEAU POUR EFFOU ET
!     EFFIN (IL N'Y A PAS D'EFFET DE BORD)
!     ------------------------------------------------------------------
    integer :: k, i
    real(kind=8) :: rsign
!     ------------------------------------------------------------------
!
    if (sens .eq. 'MOY') then
        rsign=-1.d0
    else if (sens.eq.'MAIL') then
        rsign=+1.d0
    else
        ASSERT(.false.)
    endif
!
!
    if (lreel) then
        do 10 k = 1, nbpoin*nbcmp
            reffou(k)=reffin(k)
 10     continue
!
        do 20 i = 1, nbpoin
            reffou((i-1)*nbcmp+4)=reffou((i-1)*nbcmp+4)+ rsign*excen*&
            reffou((i-1)*nbcmp+1)
            reffou((i-1)*nbcmp+5)=reffou((i-1)*nbcmp+5)+ rsign*excen*&
            reffou((i-1)*nbcmp+2)
            reffou((i-1)*nbcmp+6)=reffou((i-1)*nbcmp+6)+ rsign*excen*&
            reffou((i-1)*nbcmp+3)
 20     continue
!
!
    else
        do 30 k = 1, nbpoin*nbcmp
            ceffou(k)=ceffin(k)
 30     continue
!
        do 40 i = 1, nbpoin
            ceffou((i-1)*nbcmp+4)=ceffou((i-1)*nbcmp+4)+ rsign*excen*&
            ceffou((i-1)*nbcmp+1)
            ceffou((i-1)*nbcmp+5)=ceffou((i-1)*nbcmp+5)+ rsign*excen*&
            ceffou((i-1)*nbcmp+2)
            ceffou((i-1)*nbcmp+6)=ceffou((i-1)*nbcmp+6)+ rsign*excen*&
            ceffou((i-1)*nbcmp+3)
 40     continue
!
    endif
end subroutine
