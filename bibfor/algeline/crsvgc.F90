subroutine crsvgc(motfac, solveu, istop, nprec, syme,&
                  epsmat, mixpre, kmd)
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
!  BUT : REMPLISSAGE SD_SOLVEUR GCPC
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
    integer :: ibid, islvk, islvi, islvr, nmaxit, niremp, reacpr, pcpiv
    real(kind=8) :: resire
    character(len=8) :: precon
    character(len=19) :: solvbd
    character(len=8) :: renum
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
    call jeveuo(solveu//'.SLVK', 'E', islvk)
    call jeveuo(solveu//'.SLVR', 'E', islvr)
    call jeveuo(solveu//'.SLVI', 'E', islvi)
!
    zk24(islvk-1+1) = 'GCPC'
    zk24(islvk-1+2) = precon
    zk24(islvk-1+3) = solvbd
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
!     POUR NEWTON_KRYLOV LE RESI_RELA VARIE A CHAQUE
!     ITERATION DE NEWTON, CEPENDANT LE RESI_RELA DONNE
!     PAR L'UTILISATEUR TOUT DE MEME NECESSAIRE
!     C'EST POURQUOI ON EN FAIT UNE COPIE EN POSITION 1
    zr(islvr-1+1) = resire
    zr(islvr-1+2) = resire
    zr(islvr-1+3) = 0.d0
    zr(islvr-1+4) = 0.d0
!
    zi(islvi-1+1) = -9999
    zi(islvi-1+2) = nmaxit
    zi(islvi-1+3) = -9999
    zi(islvi-1+4) = niremp
    zi(islvi-1+5) = 0
    zi(islvi-1+6) = reacpr
    zi(islvi-1+7) = pcpiv
    zi(islvi-1+8) = 0
!
!
    call jedema()
end subroutine
