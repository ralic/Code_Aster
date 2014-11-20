subroutine te0342(option, nomte)
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
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/jsd1ff.h"
#include "asterfort/lonele.h"
#include "asterfort/matrot.h"
#include "asterfort/moytem.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utpvgl.h"
!
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
!     CALCUL
!       - DU VECTEUR ELEMENTAIRE EFFORT GENERALISE,
!     POUR LES ELEMENTS DE POUTRE DE TIMOSHENKO AVEC GAUCHISSEMENT.
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'SIEF_ELGA'
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_POU_D_TG': POUTRE DROITE DE TIMOSHENKO AVEC GAUCHISSEMENT
!
!
!-----------------------------------------------------------------------
    integer :: i, igau, j, jdepl, jeffo, k, lmater
    integer :: lorien, lx, nbpar, nbres, nc, nno
    integer :: npg
    real(kind=8) :: a, alfay, alfaz, deux, douze, e, g
    real(kind=8) :: phiy, phiz, un, valpar, xiy, xiz, xjg
    real(kind=8) :: xjx, xl, xl2, zero
!-----------------------------------------------------------------------
    parameter   (       nbres=2)
    integer :: iret
    real(kind=8) :: valres(nbres)
    integer :: codres(nbres)
    character(len=8) :: nompar
    character(len=16) :: nomres(nbres)
    real(kind=8) :: nu
    real(kind=8) :: b(7, 14)
    real(kind=8) :: pgl(14, 14), depl(14), depglo(14)
    real(kind=8) :: epsgen(7), siggen(3, 7)
!     ------------------------------------------------------------------
    data nomres / 'E' , 'NU' /
!     ------------------------------------------------------------------
    integer, parameter :: nb_cara = 7
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1','JX1','JG1'/
!-----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
    douze = 12.0d0
!
    call r8inir(7*14, zero, b, 1)
    call r8inir(7*3, zero, siggen, 1)
!
    nbpar = 0
    nompar = '  '
    valpar = 0.d0
!
    valres(:) = zero
!
! --- RECUPERATION DE LA TEMPERATURE :
!     -----------------------------------------------
    npg = 3
    call moytem('RIGI', npg, 1, '+', valpar,&
                iret)
!
    nbpar = 1
    nompar = 'TEMP'
!
! --- RECUPERATION ET INTERPOLATION DES CARACTERISTIQUES MATERIAUX :
!     ------------------------------------------------------------
    call jevech('PMATERC', 'L', lmater)
!
    call rcvalb('RIGI', npg, 1, '+', zi(lmater),&
                ' ', 'ELAS', nbpar, nompar, [valpar],&
                nbres, nomres, valres, codres, 1)
!
    e = valres(1)
    nu = valres(2)
    g = e / ( deux * ( un + nu ) )
!
! --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS :
!     --------------------------------------------------------
    
!
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
    a      = vale_cara(1)
    xiy    = vale_cara(2)
    xiz    = vale_cara(3)
    alfay  = vale_cara(4)
    alfaz  = vale_cara(5)
    xjx    = vale_cara(6)
    xjg    = vale_cara(7)
!
    nno = 2
    nc = 7
!
! --- RECUPERATION DES COORDONNEES DES NOEUDS
! --- ET CALCUL DE LA LONGUEUR DE LA POUTRE :
!     -------------------------------------
    call lonele(3, lx, xl)
!
    xl2 = xl*xl
!
! --- CALCUL DES COEFFICIENTS D'INFLUENCE DU CISAILLEMENT TRANSVERSE :
!     --------------------------------------------------------------
    phiy = e*xiz*douze*alfay/ (xl2*g*a)
    phiz = e*xiy*douze*alfaz/ (xl2*g*a)
!
! --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA  :
!     ----------------------------------------------
    call jevech('PCAORIE', 'L', lorien)
!
! --- CONSTRUCTION DE LA MATRICE DE PASSAGE PGL DU REPERE GLOBAL
! --- AU REPERE LOCAL  :
!     ---------------
    call matrot(zr(lorien), pgl)
!
! --- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT :
!     --------------------------------------------------
    call jevech('PDEPLAR', 'L', jdepl)
!
    do i = 1, 14
        depglo(i) = zr(jdepl+i-1)
    end do
!
! --- PASSAGE DES DEPLACEMENTS DU REPERE GLOBAL AU REPERE LOCAL :
!     ---------------------------------------------------------
    call utpvgl(nno, nc, pgl, depglo, depl)
!
! --- BOUCLE SUR LES POINTS DE GAUSS :
!     ------------------------------
    do igau = 1, 3
!
! --- INITIALISATION :
!     ---------------
        call r8inir(7, zero, epsgen, 1)
!
! --- CALCUL DE LA MATRICE (B) RELIANT LES DEFORMATIONS GENERALISEES
! --- (DU/DX,GAMAXY,GAMAXZ,D(TETAX)/DX,D(TETAY)/DX,D(TETAZ/DX,D(GRX)/DX)
! --- AUX DEPLACEMENTS :
!     ----------------
        call jsd1ff(igau, xl, phiy, phiz, b)
!
! --- CALCUL DES DEFORMATIONS GENERALISEES AU POINT D'INTEGRATION
! --- COURANT :
!     -------
        do i = 1, 7
            do j = 1, 14
                epsgen(i) = epsgen(i) + b(i,j)*depl(j)
            enddo
        enddo
!
! --- CALCUL DES EFFORTS GENERALISES AU POINT D'INTEGRATION
! --- COURANT :
!     -------
        siggen(igau,1) = e*a*epsgen(1)
        siggen(igau,2) = alfay*g*a*epsgen(2)
        siggen(igau,3) = alfaz*g*a*epsgen(3)
        siggen(igau,4) = xjx*g*epsgen(4)
        siggen(igau,5) = e*xiy*epsgen(5)
        siggen(igau,6) = e*xiz*epsgen(6)
        siggen(igau,7) = e*xjg*epsgen(7)
!
    end do
!
! --- RECUPERATION ET AFFECTATION DU VECTEUR DES EFFORTS
! --- GENERALISES EN SORTIE :
!     ---------------------
    if (option .eq. 'SIEF_ELGA') then
        call jevech('PCONTRR', 'E', jeffo)
    else
! OPTION NON PROGRAMMEE
        ASSERT(.false.)
    endif
!
    k = 0
    do igau = 1, 3
        do i = 1, 7
            k = k + 1
            zr(jeffo+k-1) = siggen(igau,i)
        enddo
    end do
!
end subroutine
