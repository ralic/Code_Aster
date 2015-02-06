subroutine te0235(option, nomte)
!
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
!
! --------------------------------------------------------------------------------------------------
!
!     CALCULE LA MATRICE DE RAIDEUR CENTRIFUGE DES ELEMENTS DE POUTRE
!     AVEC GAUCHISSEMENT (MULTIFIBRE ON NON)
!
! --------------------------------------------------------------------------------------------------
!
!   IN
!       OPTION  : NOM DE L'OPTION A CALCULER
!           'RIGI_MECA_RO'      : CALCUL DE LA MATRICE DE RAIDEUR CENTRIFUGE
!       NOMTE   : NOM DU TYPE ELEMENT
!           'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!           'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                               MULTI-FIBRES SECTION CONSTANTE
! --------------------------------------------------------------------------------------------------
! aslint: disable=W0104
!
    implicit none
    character(len=*) :: option, nomte
!
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/masstg.h"
#include "asterfort/matrot.h"
#include "asterfort/pmfinfo.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfitx.h"
#include "asterfort/poriro.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utpslg.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, lmater, j, lorien, lmat, nno, nc, kpg, spt
    integer :: itype, irota, jacf
    real(kind=8) :: omega(3), omegl(3), s, xl
    real(kind=8) :: e, g, xnu, rho, a, xiy, xiz, alfay, alfaz
    real(kind=8) :: pgl(3, 3), mlv(105), matp1(78)
    real(kind=8) :: carsec(6), rbid, casrho(6), casece(6)
    character(len=8) :: fami, poum
!
    integer :: nbres
    parameter (nbres=6)
    integer :: codres(nbres)
    real(kind=8) :: valres(nbres)
    character(len=16) :: nomres(nbres)
    data nomres/'E','NU','RHO','PROF_RHO_F_INT','PROF_RHO_F_EXT','COEF_MASS_AJOU'/
!
    integer :: nbfibr, nbgrfi, tygrfi, nbcarm, nug(10)
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: nb_cara = 5
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1'/
! --------------------------------------------------------------------------------------------------
!
!   Caracteristiques des elements
    nno = 2
    nc = 7
    itype = 0
!   Caracteristiques generales des sections
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
    a     = vale_cara(1)
    xiy   = vale_cara(2)
    xiz   = vale_cara(3)
    alfay = vale_cara(4)
    alfaz = vale_cara(5)
!
!   Recuperation des caracteristiques materiaux
    call jevech('PMATERC', 'L', lmater)
!
    if (nomte .eq. 'MECA_POU_D_TG') then
        valres(:) = 0.0d0
        fami='FPG1'
        kpg=1
        spt=1
        poum='+'
        call rcvalb(fami, kpg, spt, poum, zi(lmater), ' ', 'ELAS', 0, ' ', [0.0d0],&
                    3, nomres, valres, codres, 1)
        e   = valres(1)
        xnu = valres(2)
        rho = valres(3)
        g   = e /(2.0d0*(1.0d0 + xnu))
    else if (nomte.eq.'MECA_POU_D_TGM') then
!       calcul de E et G
        call pmfitx(zi(lmater), 1, casece, g)
!       calcul de RHO MOYEN
        call pmfitx(zi(lmater), 2, casrho, rbid)
!       Récupération des caractéristiques des fibres
        call pmfinfo(nbfibr,nbgrfi,tygrfi,nbcarm,nug)
        call jevech('PFIBRES', 'L', jacf)
!
        call pmfitg(tygrfi, nbfibr, nbcarm, zr(jacf), carsec)
        a   = carsec(1)
        xiy = carsec(5)
        xiz = carsec(4)
        rho = casrho(1)/a
        e   = casece(1)/a
    endif
!   Coordonnees des noeuds
    xl =  lonele()
!   Récupération des orientations
    call jevech('PCAORIE', 'L', lorien)
!   Récupération du vecteur rotation
    call jevech('PROTATR', 'L', irota)
    omega(1) = zr(irota+1)*zr(irota)
    omega(2) = zr(irota+2)*zr(irota)
    omega(3) = zr(irota+3)*zr(irota)
    call matrot(zr(lorien), pgl)
    do  i = 1, 3
        s=0.d0
        do  j = 1, 3
            s=s+pgl(i,j)*omega(j)
        enddo
        omegl(i)=s
    enddo
!   Calcul de la matrice de masse locale
    matp1(:) = 0.0d0
!   Poutre droite section constante ou variable (1 ou 2)
    call poriro(itype, matp1, rho, omegl, e, a, a, xl, xiy, xiy,&
                xiz, xiz, g, alfay, alfay, alfaz, alfaz)
    call masstg(matp1, mlv)
    call jevech('PMATUUR', 'E', lmat)
    call utpslg(nno, nc, pgl, mlv, zr(lmat))
!
end subroutine
