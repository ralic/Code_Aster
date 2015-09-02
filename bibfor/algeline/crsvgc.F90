subroutine crsvgc(motfac, solveu, istop, nprec, &
                  epsmat, mixpre, kmd, kellag, kxfem)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/gcncon.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!
    integer :: istop, nprec
    real(kind=8) :: epsmat
    character(len=3) :: mixpre, kmd, kellag
    character(len=8) :: kxfem
    character(len=16) :: motfac
    character(len=19) :: solveu
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------
!  BUT : REMPLISSAGE SD_SOLVEUR GCPC
!
! IN K19 SOLVEU  : NOM DU SOLVEUR DONNE EN ENTREE
! OUT    SOLVEU  : LE SOLVEUR EST CREE ET INSTANCIE
! IN  IN ISTOP   : PARAMETRE LIE AUX MOT-CLE STOP_SINGULIER
! IN  IN NPREC   :                           NPREC
! IN  R8 EPSMAT  :                           FILTRAGE_MATRICE
! IN  K3 MIXPRE  :                           MIXER_PRECISION
! IN  K3 KMD     :                           MATR_DISTRIBUEE
! IN  K3 KELLAG  :                           ELIM_LAGR
! IN  K8 KXFEM   :                           PRE_COND_XFEM
! ----------------------------------------------------------
!
!
!
!
    integer :: ibid,    nmaxit, niremp, reacpr, pcpiv
    real(kind=8) :: resire
    character(len=8) :: precon
    character(len=19) :: solvbd
    character(len=8) :: renum
    real(kind=8), pointer :: slvr(:) => null()
    character(len=24), pointer :: slvk(:) => null()
    integer, pointer :: slvi(:) => null()
!
!------------------------------------------------------------------
    call jemarq()
!
! --- LECTURES PARAMETRES DEDIES AU SOLVEUR
    call getvtx(motfac, 'PRE_COND', iocc=1, scal=precon, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'RENUM', iocc=1, scal=renum, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvis(motfac, 'NMAX_ITER', iocc=1, scal=nmaxit, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvr8(motfac, 'RESI_RELA', iocc=1, scal=resire, nbret=ibid)
    ASSERT(ibid.eq.1)
!
!
! --- LECTURES PARAMETRES LIES A LDLT_INC
!
!     -- INITIALISATION
    niremp=-9999
    reacpr=-9999
    pcpiv =-9999
    solvbd='XXXXXXXXXXXXXXXXXXX'
!
!     -- LECTURE
    if (precon .eq. 'LDLT_INC') then
        call getvis(motfac, 'NIVE_REMPLISSAGE', iocc=1, scal=niremp, nbret=ibid)
        ASSERT(ibid.eq.1)
    else if (precon.eq.'LDLT_SP') then
        call getvis(motfac, 'REAC_PRECOND', iocc=1, scal=reacpr, nbret=ibid)
        ASSERT(ibid.eq.1)
        call getvis(motfac, 'PCENT_PIVOT', iocc=1, scal=pcpiv, nbret=ibid)
        ASSERT(ibid.eq.1)
!
!       NOM DE SD SOLVEUR BIDON QUI SERA PASSEE A MUMPS
!       POUR LE PRECONDITIONNEMENT
        call gcncon('.', solvbd)
!
    else
        ASSERT(.false.)
    endif
!
! --- ON REMPLIT LA SD_SOLVEUR
    call jeveuo(solveu//'.SLVK', 'E', vk24=slvk)
    call jeveuo(solveu//'.SLVR', 'E', vr=slvr)
    call jeveuo(solveu//'.SLVI', 'E', vi=slvi)
!
    slvk(1) = 'GCPC'
    slvk(2) = precon
    slvk(3) = solvbd
    slvk(4) = renum
    slvk(5) = 'XXXX'
    slvk(6) = 'XXXX'
    slvk(7) = 'XXXX'
    slvk(8) = 'XXXX'
    slvk(9) = 'XXXX'
    slvk(10)= 'XXXX'
    slvk(11)= 'XXXX'
    slvk(12)= 'XXXX'
    slvk(13)= kellag
    slvk(14)= 'XXXX'
!
!     POUR NEWTON_KRYLOV LE RESI_RELA VARIE A CHAQUE
!     ITERATION DE NEWTON, CEPENDANT LE RESI_RELA DONNE
!     PAR L'UTILISATEUR TOUT DE MEME NECESSAIRE
!     C'EST POURQUOI ON EN FAIT UNE COPIE EN POSITION 1
    slvr(1) = resire
    slvr(2) = resire
    slvr(3) = 0.d0
    slvr(4) = 0.d0
!
    slvi(1) = -9999
    slvi(2) = nmaxit
    slvi(3) = -9999
    slvi(4) = niremp
    slvi(5) = 0
    slvi(6) = reacpr
    slvi(7) = pcpiv
    slvi(8) = 0
!
!
    call jedema()
end subroutine
