subroutine mnlcst(parcho, adime, ninc, nd, nchoc,&
                  h, hf, xcst)
    implicit none
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!
!     MODE_NON_LINE PARTIE CONSTANTE G0
!     -    -               ---
! ----------------------------------------------------------------------
!
! REGROUPE LES TERMES CONSTANT DU PROBLEME A RESOUDRE
! ----------------------------------------------------------------------
! IN  PARCHO : K14  : NOM DE LA SD PARAMETRE DES CONTACTEURS
! IN  ADIME  : K14  : SD PARAMETRE POUR ADIMENSIONNEMENT
! IN  NINC   : I    : NOMBRE D INCONNUES DU SYSTEME
! IN  ND     : I    : NOMBRE DE DEGRES DE LIBERTE
! IN  NCHOC  : I    : NOMBRE DE CONTACTEURS
! IN  H      : I    : NOMBRE D'HARMONIQUES de X
! IN  HF     : I    : NOMBRE D'HARMONIQUES POUR F
! OUT XCST   : K14  : NOM DU VECTEUR DES TERMES CONSTANTS
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "blas/dscal.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: ninc, nd, nchoc, h, hf
    character(len=14) :: parcho, adime, xcst
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    real(kind=8) :: alpha, eta, jeu
    integer :: iadim, icst,     neqs, i
    real(kind=8), pointer :: jeumax(:) => null()
    real(kind=8), pointer :: raid(:) => null()
    character(len=8), pointer :: type(:) => null()
    real(kind=8), pointer :: orig(:) => null()
    real(kind=8), pointer :: vjeu(:) => null()
    integer, pointer :: vneqs(:) => null()
    real(kind=8), pointer :: reg(:) => null()
!
    call jemarq()
! ----------------------------------------------------------------------
! --- RECUPERATION POINTEUR DE XVECT, L(XVECT) ET XCDL
! ----------------------------------------------------------------------
    call jeveuo(adime, 'L', iadim)
    call jeveuo(xcst, 'E', icst)
    call dscal(ninc-1, 0.d0, zr(icst), 1)
! ----------------------------------------------------------------------
! --- EQUATION DE LA DYNAMIQUE
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! --- EQUATION SUPPLEMENTAIRE POUR DEFINIR LA FORCE NON-LINEAIRE
! ----------------------------------------------------------------------
    call jeveuo(parcho//'.RAID', 'L', vr=raid)
    call jeveuo(parcho//'.REG', 'L', vr=reg)
    call jeveuo(parcho//'.NEQS', 'L', vi=vneqs)
    call jeveuo(parcho//'.TYPE', 'L', vk8=type)
    call jeveuo(parcho//'.ORIG', 'L', vr=orig)
    call jeveuo(parcho//'.JEU', 'L', vr=vjeu)
    call jeveuo(parcho//'.JEUMAX', 'L', vr=jeumax)
    neqs=0
    do 110 i = 1, nchoc
        alpha=raid(i)/zr(iadim)
        eta=reg(i)
        jeu=vjeu(i)/jeumax(1)
        if (type(i)(1:7) .eq. 'CERCLE') then
! ---     -ORIG1^2 - ORIG2^2
            zr(icst+nd*(2*h+1)+(neqs+2)*(2*hf+1))= -(orig(1+3*(i-1))&
            /jeu)**2-(orig(1+3*(i-1)+1)/jeu)**2
! ---     ETA
            zr(icst+nd*(2*h+1)+(neqs+3)*(2*hf+1))=-eta
        else if (type(i)(1:6).eq.'PLAN') then
! ---     ETA
            zr(icst+nd*(2*h+1)+neqs*(2*hf+1))=-eta
        endif
        neqs=neqs+vneqs(i)
110  continue
! ----------------------------------------------------------------------
! --- AUTRES EQUATIONS
! ----------------------------------------------------------------------
!
    call jedema()
!
end subroutine
