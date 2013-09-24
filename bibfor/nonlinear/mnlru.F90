subroutine mnlru(imat, xcdl, parcho, adime, xvect,&
                 ninc, nd, nchoc, h, hf,&
                 xru)
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
!     MODE_NON_LINE CALCUL DE R(U)
!     -    -   -              - -
! ----------------------------------------------------------------------
!
! CALCUL R(U) = L(U) + Q(U,U)
! ----------------------------------------------------------------------
! IN  IMAT   : I(2) : DESCRIPTEUR DES MATRICES :
!                       - IMAT(1) => MATRICE DE RAIDEUR
!                       - IMAT(2) => MATRICE DE MASSE
! IN  XCDL   : K14  : INDICE DES CONDITIONS AUX LIMITES
! IN  PARCHO : K14  : NOM DE LA SD PARAMETRE DES CONTACTEURS
! IN  ADIME  : K14  : SD PARAMETRE POUR ADIMENSIONNEMENT
! IN  XVECT  : K14  : NOM DU VECTEUR SOLUTION
! IN  NINC   : I    : NOMBRE D INCONNUES DU SYSTEME
! IN  ND     : I    : NOMBRE DE DEGRES DE LIBERTE
! IN  NCHOC  : I    : NOMBRE DE CONTACTEURS
! IN  H      : I    : NOMBRE D'HARMONIQUES POUR LE DEPLACEMENT
! IN  HF     : I    : NOMBRE D'HARMONIQUES POUR LA FORCE
! OUT XRU    : K14  : NOM DU VECTEUR DE SORTIE, R(XVECT)=XRU
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mnlcst.h"
#include "asterfort/mnline.h"
#include "asterfort/mnlqnl.h"
#include "asterfort/wkvect.h"
    integer :: imat(2), ninc, nd, nchoc, h, hf
    character(len=14) :: xcdl, parcho, adime, xvect, xru
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    integer :: iru, ivint
    character(len=14) :: xvint
!
    call jemarq()
!    call jxveri(' ', ' ')
! ----------------------------------------------------------------------
! --- RECUPERATION DU POINTEUR DE R(XVECT)
! ----------------------------------------------------------------------
    call jeveuo(xru, 'E', iru)
    call dscal(ninc-1, 0.d0, zr(iru), 1)
! ----------------------------------------------------------------------
! --- CREATION D'UN VECTEUR INTERMEDIAIRE
! ----------------------------------------------------------------------
    xvint = '&&MNLRU.INT'
    call wkvect(xvint, 'V V R', ninc-1, ivint)
! ----------------------------------------------------------------------
! --- CALCUL DE R(XVECT)
! ----------------------------------------------------------------------
! --- CALCUL DE L0
    call dscal(ninc-1, 0.d0, zr(ivint), 1)
    call mnlcst(parcho, adime, ninc, nd, nchoc,&
                h, hf, xvint)
    call dcopy(ninc-1, zr(ivint), 1, zr(iru), 1)
! --- CALCUL DE L(XVECT)
    call dscal(ninc-1, 0.d0, zr(ivint), 1)
    call mnline(imat, xcdl, parcho, adime, xvect,&
                ninc, nd, nchoc, h, hf,&
                xvint)
    call daxpy(ninc-1, 1.d0, zr(ivint), 1, zr(iru),&
               1)
! --- CALCUL DE Q(XVECT,XVECT)
    call dscal(ninc-1, 0.d0, zr(ivint), 1)
    call mnlqnl(imat, xcdl, parcho, adime, xvect,&
                xvect, ninc, nd, nchoc, h,&
                hf, xvint)
! --- R(XVECT) = L0 + L(XVECT) + Q(XVECT,XVECT)
    call daxpy(ninc-1, 1.d0, zr(ivint), 1, zr(iru),&
               1)
! ----------------------------------------------------------------------
! --- DESTRUCTION DU VECTEUR INTERMEDIAIRE
! ----------------------------------------------------------------------
    call jedetr(xvint)
!
    call jedema()
!
end subroutine
