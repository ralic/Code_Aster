subroutine te0235(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/masstg.h"
#include "asterfort/matrot.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfitx.h"
#include "asterfort/poriro.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utpslg.h"
!
    character(len=*) :: option, nomte
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
! ======================================================================
!     CALCULE LA MATRICE DE RAIDEUR CENTRIFUGE DES ELEMENTS DE POUTRE
!     AVEC GAUCHISSEMENT (MULTIFIBRE ON NON)
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!       'RIGI_MECA_RO'      : CALCUL DE LA MATRICE DE RAIDEUR CENTRIFUGE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES SECTION CONSTANTE
! aslint: disable=W0104
!
    integer :: nbres
    parameter (nbres=6)
    real(kind=8) :: valres(nbres), r8b
    integer :: codres(nbres)
    character(len=8) :: fami, poum
    character(len=16) :: nomres(nbres)
    integer :: i, lmater, j
    integer :: lorien, lmat
    integer :: nno, nc, kpg, spt
    integer :: itype, lx, irota
    integer :: inbfib, nbfib, jacf
    real(kind=8) :: omega(3), omegl(3), s
    real(kind=8) :: xl
    real(kind=8) :: zero, un, deux
    real(kind=8) :: e, g, xnu, rho
    real(kind=8) :: a, xiy, xiz, alfay, alfaz
    real(kind=8) :: pgl(3, 3), mlv(105), matp1(78)
    real(kind=8) :: carsec(6), rbid, casrho(6), casece(6)
!     ------------------------------------------------------------------
    data nomres/'E','NU','RHO','PROF_RHO_F_INT','PROF_RHO_F_EXT','COEF_MASS_AJOU'/
!     ------------------------------------------------------------------
    integer, parameter :: nb_cara = 5
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1'/
!-----------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    deux = 2.d0
    r8b = 0.d0
!     ------------------------------------------------------------------
!
!     --- CARACTERISTIQUES DES ELEMENTS
!
    nno = 2
    nc = 7
    itype = 0
!
!     --- CARACTERISTIQUES GENERALES DES SECTIONS ---
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
    a     = vale_cara(1)
    xiy   = vale_cara(2)
    xiz   = vale_cara(3)
    alfay = vale_cara(4)
    alfaz = vale_cara(5)
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', lmater)
!
    if (nomte .eq. 'MECA_POU_D_TG') then
        do i = 1, nbres
            valres(i) = zero
        enddo
!
!
        fami='FPG1'
        kpg=1
        spt=1
        poum='+'
        call rcvalb(fami, kpg, spt, poum, zi(lmater),&
                    ' ', 'ELAS', 0, ' ', [r8b],&
                    3, nomres, valres, codres, 1)
        e = valres(1)
        xnu = valres(2)
        rho = valres(3)
        g = e / ( deux * ( un + xnu ) )
!
    else if (nomte.eq.'MECA_POU_D_TGM') then
!       CALCUL DE E ET G
        call pmfitx(zi(lmater), 1, casece, g)
!
!       CALCUL DE RHO MOYEN
        call pmfitx(zi(lmater), 2, casrho, rbid)
!
!
        call jevech('PNBSP_I', 'L', inbfib)
        nbfib = zi(inbfib)
        call jevech('PFIBRES', 'L', jacf)
        call pmfitg(nbfib, 3, zr(jacf), carsec)
        a = carsec(1)
        xiy = carsec(5)
        xiz = carsec(4)
!
        rho = casrho(1)/a
        e = casece(1)/a
!
    endif
!     --- COORDONNEES DES NOEUDS ---
    call lonele(3, lx, xl)
!
!     --- RECUPERATION DES ORIENTATIONS ---
    call jevech('PCAORIE', 'L', lorien)
!     --- RECUPERATION DU VECTEUR ROTATION ---
    call jevech('PROTATR', 'L', irota)
    omega(1) = zr(irota+1)*zr(irota)
    omega(2) = zr(irota+2)*zr(irota)
    omega(3) = zr(irota+3)*zr(irota)
    call matrot(zr(lorien), pgl)
    do  i = 1, 3
        s=0.d0
        do  j = 1, 3
            s=s+pgl(i,j)*omega(j)
        end do
        omegl(i)=s
    end do
!
!CC     --- CALCUL DE LA MATRICE DE MASSE LOCALE ---
    do 20 i = 1, 78
        matp1(i) = 0.0d0
20  continue
!        --- POUTRE DROITE SECTION CONSTANTE OU VARIABLE (1 OU 2)
    call poriro(itype, matp1, rho, omegl, e,&
                a, a, xl, xiy, xiy,&
                xiz, xiz, g, alfay, alfay,&
                alfaz, alfaz)
    call masstg(matp1, mlv)
!
!
    call jevech('PMATUUR', 'E', lmat)
!
    call utpslg(nno, nc, pgl, mlv, zr(lmat))
!
end subroutine
