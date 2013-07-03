subroutine crsvmf(motfac, solveu, istop, nprec, syme,&
                  epsmat, mixpre, kmd)
    implicit none
#include "jeveux.h"
!
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
!  BUT : REMPLISSAGE SD_SOLVEUR MULT_FRONT
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
    integer :: islvk, islvr, islvi, ibid
    character(len=8) :: renum
    integer :: iarg
!
!------------------------------------------------------------------
    call jemarq()
!
!
! --- LECTURES PARAMETRES DEDIES AU SOLVEUR
    call getvtx(motfac, 'RENUM', 1, iarg, 1,&
                renum, ibid)
    call assert(ibid.eq.1)
!
! --- ON REMPLIT LA SD_SOLVEUR
    call jeveuo(solveu//'.SLVK', 'E', islvk)
    call jeveuo(solveu//'.SLVR', 'E', islvr)
    call jeveuo(solveu//'.SLVI', 'E', islvi)
!
    zk24(islvk-1+1) = 'MULT_FRONT'
    zk24(islvk-1+2) = 'XXXX'
    zk24(islvk-1+3) = 'XXXX'
    zk24(islvk-1+4) = renum
    zk24(islvk-1+5) = syme
    zk24(islvk-1+6) = 'XXXX'
    zk24(islvk-1+7) = 'XXXX'
    zk24(islvk-1+8) = 'XXXX'
    zk24(islvk-1+9) = 'XXXX'
    zk24(islvk-1+10)= 'XXXX'
    zk24(islvk-1+11)= 'XXXX'
    zk24(islvk-1+12)= 'XXXX'
!
    zr(islvr-1+1) = 0.d0
    zr(islvr-1+2) = 0.d0
    zr(islvr-1+3) = 0.d0
    zr(islvr-1+4) = 0.d0
!
    zi(islvi-1+1) = nprec
    zi(islvi-1+2) = -9999
    zi(islvi-1+3) = istop
    zi(islvi-1+4) = -9999
    zi(islvi-1+5) = -9999
    zi(islvi-1+6) = -9999
    zi(islvi-1+7) = -9999
    zi(islvi-1+8) = 0
!
    call jedema()
end subroutine
