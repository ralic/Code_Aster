subroutine ssvau1(nomacr, iavein, iaveou)
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mrconl.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/mtdscr.h"
#include "asterfort/rldlr8.h"
    character(len=8) :: nomacr
    integer :: iavein, iaveou
! ----------------------------------------------------------------------
!     BUT:
!         "CONDENSER" UN  VECTEUR CHARGEMENT D'UN MACR_ELEM_STAT :
!          EN ENTREE :
!            VECIN  (     1,NDDLI      )  =  F_I (EVENT. TOURNE)
!            VECIN  (NDDLI+1,NDDLI+NDDLE) =  F_E (EVENT. TOURNE)
!
!          EN SORTIE :
!            VECOUT(       1,NDDLI      ) = (KII**-1)*F_I
!            VECOUT(NDDLI+1,NDDLI+NDDLE)  =  FP_E
!            OU FP_E = F_E - K_EI*(KII**-1)*F_I
!
!     IN: NOMACR : NOM DU MACR_ELEM_STAT
!         IAVEIN : ADRESSE DANS ZR DU VECTEUR A CONDENSER.(VECIN)
!         IAVEOU : ADRESSE DANS ZR DU VECTEUR CONDENSE.(VECOUT)
!
!         IMPORTANT : LES 2 ADRESSES IAVEIN ET IAVEOU PEUVENT ETRE
!                     IDENTIQUES (CALCUL EN PLACE).
!
!     OUT:   LE VECTEUR VECOUT EST CALCULE.
! ----------------------------------------------------------------------
!
!
    character(len=8) :: kbid
    integer :: scdi, schc, iblo
    character(len=19) :: matas, stock, nu
!
!-----------------------------------------------------------------------
    integer :: iadesm, iascbl, iascdi, iaschc, iascib, iblold, j
    integer :: jrefa, jualf, k, kk, lmat, nbbloc, nddle
    integer :: nddli, nddlt
!-----------------------------------------------------------------------
    call jemarq()
!
    matas=nomacr//'.RIGIMECA'
    nu=nomacr
    nu=nu(1:14)//'.NUME'
    stock=nu(1:14)//'.SLCS'
    call jeveuo(stock//'.SCIB', 'L', iascib)
!
!
    call jeveuo(nomacr//'.DESM', 'L', iadesm)
    nddle=zi(iadesm-1+4)
    nddli=zi(iadesm-1+5)
    nddlt=nddli+nddle
!
!
!     -- ON RECOPIE VECIN DANS VECOUT POUR EVITER LES EFFETS DE BIAIS:
!     ---------------------------------------------------------------
    do 10,kk=1,nddlt
    zr(iaveou-1+kk)=zr(iavein-1+kk)
    10 end do
!
!
!     -- ON COMMENCE PAR CONDITIONNER LE SECOND MEMBRE INITIAL (.CONL)
!     ------------------- -------------------------------------------
    call mtdscr(matas)
    call jeveuo(matas(1:19)//'.&INT', 'E', lmat)
    call mrconl('MULT', lmat, nddlt, 'R', zr(iaveou),&
                1)
!
!
!     -- CALCUL DE QI0 = (K_II**(-1))*F_I DANS : VECOUT(1->NDDLI):
!     ------------------- ----------------------------------------
    call jeveuo(zk24(zi(lmat+1))(1:19)//'.REFA', 'L', jrefa)
    call jeveuo(zk24(jrefa-1+2)(1:14)//'.SLCS.SCHC', 'L', iaschc)
    call mtdsc2(zk24(zi(lmat+1)), 'SCDI', 'L', iascdi)
    call mtdsc2(zk24(zi(lmat+1)), 'SCBL', 'L', iascbl)
    call jelira(matas//'.UALF', 'NMAXOC', nbbloc, kbid)
!
    call rldlr8(zk24(zi(lmat+1)), zi(iaschc), zi(iascdi), zi(iascbl), nddli,&
                nbbloc, zr(iaveou), 1)
!
!
!     -- CALCUL DE FP_E = F_E-K_EI*QI0 DANS : VECOUT(NDDLI+1,NDDLT):
!     -----------------------------------------------------------------
    iblold=0
    do 30,j=1,nddle
    iblo=zi(iascib-1+nddli+j)
    scdi=zi(iascdi-1+nddli+j)
    schc=zi(iaschc-1+nddli+j)
    if (iblo .ne. iblold) then
        if (iblold .gt. 0) call jelibe(jexnum(matas//'.UALF', iblold))
        call jeveuo(jexnum(matas//'.UALF', iblo), 'L', jualf)
    endif
    iblold=iblo
    kk=0
    do 20,k=nddli+j+1-schc,nddli
    kk=kk+1
    zr(iaveou-1+nddli+j)=zr(iaveou-1+nddli+j)- zr(iaveou-1+k)*&
            zr(jualf-1+scdi-schc+kk)
20  continue
    30 end do
    if (iblold .gt. 0) call jelibe(jexnum(matas//'.UALF', iblold))
!
!
    call jelibe(zk24(jrefa-1+2)(1:14)//'.SLCS.SCHC')
!
    call jedema()
end subroutine
