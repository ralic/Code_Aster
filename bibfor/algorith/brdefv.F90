subroutine brdefv(e1i, e2i, a, t, b,&
                  k0, k1, eta1, k2, eta2,&
                  pw, e0f, e1f, e2f, e2pc,&
                  e2pt, sige2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!        REPONSE EN RELAXATION
!     A E(T)=A*T+B AVEC E1(0)=E1I ET E2(0)=E2I
!     GESTION DES DEFORMATIONS PLASTIQUES ETAGE 2 (AS SEPTEMBRE 2004)
!     E2P(C OU T)=DEFORMATION PLASTIQUE ETAGE 2 EN +(T) OU -(C)
!
    implicit none
#include "asterf_types.h"
#include "asterfort/bre1bl.h"
#include "asterfort/bre1ec.h"
#include "asterfort/bre2ec.h"
    aster_logical :: h1, h2
!
!     EFLIM=3.0E-2
!-----------------------------------------------------------------------
    real(kind=8) :: k0, k1, k2
    real(kind=8) :: a, b, e0f, e1f, e1i, e2f, e2i
    real(kind=8) :: e2p, e2pc, e2pt, eta1, eta2, pw, sige1
    real(kind=8) :: sige2, t
!-----------------------------------------------------------------------
    h1=.false.
!     ECOULEMENT NIV 2 COMPRESSION
    e2p=e2pt
    call bre2ec(k0, k1, k2, eta1, eta2,&
                e1i, e2i, a, t, b,&
                e2p, pw, e2f)
    if ((e2f-e2p) .lt. 0.d0) then
!      ON EST EN COMP
        if (e2f .le. e2pc) then
!       PRINT *,'HYPOTHESE ECOULEMENT COMP OK'
            h1=.true.
            sige2=k2*(e2f-e2p)
            e2pc=e2f
            call bre1ec(k0, k1, k2, eta1, eta2,&
                        e1i, e2i, a, t, b,&
                        e2p, pw, e1f)
        endif
    endif
!
    if (h1) goto 10
!
    h2=.false.
!     HYPOTHESE  ECOULEMENT NIV 2 TRACTION
    e2p=e2pc
    call bre2ec(k0, k1, k2, eta1, eta2,&
                e1i, e2i, a, t, b,&
                e2p, pw, e2f)
    if ((e2f-e2p) .gt. 0.d0) then
!      ON EST EN TRACTION
        if (e2f .ge. e2pt) then
!       HYPOTHESE ECOULEMENT OK
            h2=.true.
            e2pt=e2f
            sige2=k2*(e2f-e2p)
            call bre1ec(k0, k1, k2, eta1, eta2,&
                        e1i, e2i, a, t, b,&
                        e2p, pw, e1f)
        endif
    endif
!
    if (h2) goto 10
!
!      PRINT*,'HYPOTHESES ECOULEMENT FAUSSES : BLOCAGE NIVEAU 2'
    e2f=e2i
    call bre1bl(k0, k1, k2, eta1, eta2,&
                e1i, e2i, a, t, b,&
                pw, e1f)
    sige1=k1*e1f
    sige2=sige1
!
!     CALCUL DE E0(T)
!
 10 continue
    e0f=a*t+b-e1f-e2f
end subroutine
