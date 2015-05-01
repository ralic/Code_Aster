subroutine pacouf(x, fvect, vecr1, vecr2, typflu,&
                  vecr3, amor, masg, vecr4, vecr5,&
                  veci1, vgap, indic, nbm, nmode)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/coefmo.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=8) :: typflu
    real(kind=8) :: xsi0
    real(kind=8) :: amor(*), vgap, fvect(2), x(2), masg(*), mi
    real(kind=8) :: vecr1(*), vecr2(*), vecr3(*), vecr4(*), vecr5(*)
    integer :: veci1(*)
!
    complex(kind=8) :: fonct, z, xkf
    aster_logical :: zrigi
    real(kind=8) :: depi
!
! ----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: indic, nbm, nmode
    real(kind=8) :: pulsc, xcf, xcs, xks, xmf, xms
!-----------------------------------------------------------------------
    call jemarq()
    depi = r8depi()
!
    pulsc = sqrt(x(1)*x(1)+x(2)*x(2))
    mi = masg(nmode)
    xsi0 = amor(nmode) / (2.0d0*depi*mi*amor(nbm+nmode))
    xcs = amor(nmode)
    xks = depi*depi*amor(nbm+nmode)*amor(nbm+nmode)*mi
    xms = masg(nmode)
    zrigi = .true.
!     ZRIGI=TRUE SIGNIFIE QU'ON CALCULE LES TERMES DE RAIDEUR ET
!     D'AMORTISSEMENT
    xmf = 0.d0
    xkf = dcmplx(0.d0,0.d0)
    xcf = 0.d0
!
! --- CALCUL DES COEFFICIENTS DE MASSE, RAIDEUR ET AMORTISSEMENT ---
! --- AJOUTE                                                     ---
!
    call coefmo(typflu, zrigi, nbm, nmode, indic,&
                x, pulsc, vgap, xsi0, veci1,&
                vecr1, vecr2, vecr3, vecr4, vecr5,&
                xmf, xkf, xcf)
!
! --- CALCUL DE LA FONCTION ---
!
    z = dcmplx(x(1),x(2))
    fonct = (xms+xmf)*z*z + (xcs+xcf)*z + (xks+xkf)
    fvect(1) = dble(fonct)
    fvect(2) = dimag(fonct)
!
    call jedema()
end subroutine
