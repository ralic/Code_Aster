subroutine crsvld(motfac, solveu, istop, nprec, syme,&
                  epsmat, mixpre, kmd)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jevtbl.h"
!
    integer :: istop, nprec
    real(kind=8) :: epsmat
    character(len=3) :: syme, mixpre, kmd
    character(len=16) :: motfac
    character(len=19) :: solveu
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
! ----------------------------------------------------------
!  BUT : REMPLISSAGE SD_SOLVEUR LDLT
!
! IN K19 SOLVEU  : NOM DU SOLVEUR DONNE EN ENTREE
! OUT    SOLVEU  : LE SOLVEUR EST CREE ET INSTANCIE
! IN  IN ISTOP   : PARAMETRE LIE AUX MOT-CLE STOP_SINGULIER
! IN  IN NPREC   :                           NPREC
! IN  K3 SYME    :                           SYME
! IN  R8 EPSMAT  :                           FILTRAGE_MATRICE
! IN  K3 MIXPRE  :                           MIXER_PRECISION
! IN  K3 KMD     :                           MATR_DISTRIBUEE
! ----------------------------------------------------------
!
!
!
!
    integer :: ibid
    character(len=8) :: renum
    integer, pointer :: slvi(:) => null()
    real(kind=8), pointer :: slvr(:) => null()
    character(len=24), pointer :: slvk(:) => null()
!
!------------------------------------------------------------------
    call jemarq()
!
! --- LECTURES PARAMETRES DEDIES AU SOLVEUR
    call getvtx(motfac, 'RENUM', iocc=1, scal=renum, nbret=ibid)
    ASSERT(ibid.eq.1)
!
! --- ON REMPLIT LA SD_SOLVEUR
    call jeveuo(solveu//'.SLVK', 'E', vk24=slvk)
    call jeveuo(solveu//'.SLVR', 'E', vr=slvr)
    call jeveuo(solveu//'.SLVI', 'E', vi=slvi)
!
    slvk(1) = 'LDLT'
    slvk(2) = 'XXXX'
    slvk(3) = 'XXXX'
    slvk(4) = renum
    slvk(5) = syme
    slvk(6) = 'XXXX'
    slvk(7) = 'XXXX'
    slvk(8) = 'XXXX'
    slvk(9) = 'XXXX'
    slvk(10)= 'XXXX'
    slvk(11)= 'XXXX'
    slvk(12)= 'XXXX'
!
    slvr(1) = 0.d0
    slvr(2) = 0.d0
    slvr(3) = jevtbl('TAILLE_BLOC')
    slvr(4) = 0.d0
!
    slvi(1) = nprec
    slvi(2) = -9999
    slvi(3) = istop
    slvi(4) = -9999
    slvi(5) = -9999
    slvi(6) = -9999
    slvi(7) = -9999
    slvi(8) = 0
!
    call jedema()
end subroutine
