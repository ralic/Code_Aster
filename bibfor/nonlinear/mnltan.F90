subroutine mnltan(lcal, imat, numdrv, matdrv, xcdl,&
                  parcho, adime, xvect, ninc, nd,&
                  nchoc, h, hf, xtang)
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
!     MODE_NON_LINE CALCUL D'UN VECTEUR TANGENT
!     -    -   -                        ---
! ----------------------------------------------------------------------
!
! CALCUL LE VECTEUR TANGENT AU POINT XVECT
! ----------------------------------------------------------------------
! IN  LCAL   : L     : SI .TRUE. ALORS ON RECALCUL LA MATRICE
!                                SINON ON REMPLACE SEULEMENT LA
!                                           DERNIERE LIGNE DE LA MATRICE
! IN  IMAT   : I(2) : DESCRIPTEUR DES MATRICES :
!                       - IMAT(1) => MATRICE DE RAIDEUR
!                       - IMAT(2) => MATRICE DE MASSE
! IN  NUMDRV : K14  : NUME_DDL_GENE DE LA MATRICE JACOBIENNE
! IN  MATDRV : K19  : NOM DE  LA MATRICE JACOBIENNE
! IN  XCDL   : K14  : INDICE DES CONDITIONS AUX LIMITES
! IN  PARCHO : K14  : NOM DE LA SD PARAMETRE DES CONTACTEURS
! IN  ADIME  : K14  : SD PARAMETRE POUR ADIMENSIONNEMENT
! IN  XVECT  : K14  : NOM DU VECTEUR SOLUTION
! IN  NINC   : I    : NOMBRE D INCONNUES DU SYSTEME
! IN  ND     : I    : NOMBRE DE DEGRES DE LIBERTE ACTIFS
! IN  NCHOC  : I    : NOMBRE DE CONTACTEURS
! IN  H      : I    : NOMBRE D'HARMONIQUES POUR LE DEPLACEMENT
! IN  HF     : I    : NOMBRE D'HARMONIQUES POUR LA FORCE
! OUT XTANG  : K14  : NOM DU VECTEUR TANGENT AU POINT SOLUTION
! ----------------------------------------------------------------------
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getran.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mnldrv.h"
#include "asterfort/resoud.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
#include "blas/dnrm2.h"
#include "blas/dscal.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
    aster_logical :: lcal
    integer :: imat(2), ninc, nd, nchoc, h, hf
    character(len=14) :: numdrv, xcdl, parcho, adime, xvect, xtang
    character(len=19) :: matdrv
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    integer :: i, itang, iret, ib, ivplu
    real(kind=8) :: norme
    complex(kind=8) :: cbid
    cbid = dcmplx(0.d0, 0.d0)
!
    call jemarq()
! ----------------------------------------------------------------------
! --- CREATION VECTEURS TEMPORAIRES
! ----------------------------------------------------------------------
    call wkvect('&&mnltan.b', 'V V R', ninc, ib)
    call wkvect('&&mnltan.vecplu', 'V V R', ninc, ivplu)
! ----------------------------------------------------------------------
! --- CREATION D'UN VECTEUR ALEATOIRE (A AJOUTER A LA DERNIERE LIGNE
! ---                                          DE LA MATRICE JACOBIENNE)
! ----------------------------------------------------------------------
    do i = 1, ninc
        call getran(zr(ivplu-1+i))
    end do
! ----------------------------------------------------------------------
! --- RECUPERATION DU VECTEUR TANGENT
! ----------------------------------------------------------------------
    call jeveuo(xtang, 'E', itang)
! ----------------------------------------------------------------------
! --- CALCUL (OU RECUPERATION) DE LA MATRICE JACOBIENNE
! ----------------------------------------------------------------------
    call mnldrv(lcal, imat, numdrv, matdrv, xcdl,&
                parcho, adime, xvect, zr(ivplu), ninc,&
                nd, nchoc, h, hf)
! ----------------------------------------------------------------------
! --- ON CREE UN VECTEUR [0 ... 0 1]
! ----------------------------------------------------------------------
    zr(ib-1+ninc) = 1.d0
! ----------------------------------------------------------------------
! --- ON RESOUD TANGENTE=DRDV\[0 ... 0 1]
! ----------------------------------------------------------------------
    call resoud(matdrv, ' ', ' ', ' ', 1,&
                ' ', ' ', 'v', zr(ib), [cbid],&
                ' ', .false._1, 0, iret)
    call dcopy(ninc, zr(ib), 1, zr(itang), 1)
! ----------------------------------------------------------------------
! --- ON NORMALISE LE VECTEUR TANGENT
! ----------------------------------------------------------------------
    norme = dnrm2(ninc,zr(itang),1)
    call dscal(ninc, -1.d0/norme, zr(itang), 1)
! ----------------------------------------------------------------------
! --- ON DETRUIT LE VECTEUR TEMPORAIRE
! ----------------------------------------------------------------------
    call jedetr('&&mnltan.b')
    call jedetr('&&mnltan.vecplu')
!
    call jedema()
!
end subroutine
