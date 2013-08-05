subroutine crsvpe(motfac, solveu, istop, nprec, syme,&
                  epsmat, mixpre, kmd)
    implicit none
#include "jeveux.h"
!
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/gcncon.h"
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
!  BUT : REMPLISSAGE SD_SOLVEUR PETSC
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
    integer :: ibid, niremp, nmaxit, reacpr, pcpiv
    integer :: islvk, islvi, islvr
    real(kind=8) :: fillin, epsmax, resipc
    character(len=8) :: kalgo, kprec, renum
    character(len=19) :: solvbd
    integer :: iarg
!
!------------------------------------------------------------------
    call jemarq()
!
! --- LECTURES PARAMETRES DEDIES AU SOLVEUR
!     PARAMETRES FORCEMMENT PRESENTS
    call getvtx(motfac, 'ALGORITHME', 1, iarg, 1,&
                kalgo, ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'PRE_COND', 1, iarg, 1,&
                kprec, ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'RENUM', 1, iarg, 1,&
                renum, ibid)
    ASSERT(ibid.eq.1)
    call getvr8(motfac, 'RESI_RELA', 1, iarg, 1,&
                epsmax, ibid)
    ASSERT(ibid.eq.1)
    call getvis(motfac, 'NMAX_ITER', 1, iarg, 1,&
                nmaxit, ibid)
    ASSERT(ibid.eq.1)
    call getvr8(motfac, 'RESI_RELA_PC', 1, iarg, 1,&
                resipc, ibid)
    ASSERT(ibid.eq.1)
!
!     INITIALISATION DES PARAMETRES OPTIONNELS
    niremp = 0
    fillin = 1.d0
    reacpr = 0
    pcpiv = -9999
    solvbd = ' '
!
    if (kprec .eq. 'LDLT_INC') then
        call getvis(motfac, 'NIVE_REMPLISSAGE', 1, iarg, 1,&
                    niremp, ibid)
        ASSERT(ibid.eq.1)
        call getvr8(motfac, 'REMPLISSAGE', 1, iarg, 1,&
                    fillin, ibid)
        ASSERT(ibid.eq.1)
!     PARAMETRES OPTIONNELS LIES AU PRECONDITIONNEUR SP
    else if (kprec.eq.'LDLT_SP') then
        call getvis(motfac, 'REAC_PRECOND', 1, iarg, 1,&
                    reacpr, ibid)
        ASSERT(ibid.eq.1)
        call getvis(motfac, 'PCENT_PIVOT', 1, iarg, 1,&
                    pcpiv, ibid)
        ASSERT(ibid.eq.1)
!       NOM DE SD SOLVEUR BIDON QUI SERA PASSEE A MUMPS
!       POUR LE PRECONDITIONNEMENT
        call gcncon('.', solvbd)
!     PARAMETRES OPTIONNELS LIES AU MULTIGRILLE ALGEBRIQUE ML
    else if (kprec.eq.'ML') then
!     PARAMETRES OPTIONNELS LIES AU MULTIGRILLE ALGEBRIQUE BOOMERAMG
    else if (kprec.eq.'BOOMER') then
!     PAS DE PARAMETRES POUR LES AUTRES PRECONDITIONNEURS
        else if (kprec.eq.'JACOBI' .or.&
     &         kprec.eq.'SOR'    .or.&
     &         kprec.eq.'SANS') then
!     RIEN DE PARTICULIER...
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
    zk24(islvk-1+1) = 'PETSC'
    zk24(islvk-1+2) = kprec
    zk24(islvk-1+3) = solvbd
    zk24(islvk-1+4) = renum
    zk24(islvk-1+5) = syme
    zk24(islvk-1+6) = kalgo
    zk24(islvk-1+7) = 'XXXX'
    zk24(islvk-1+8) = 'XXXX'
    zk24(islvk-1+9) = 'XXXX'
    zk24(islvk-1+10)= kmd
    zk24(islvk-1+11)= 'XXXX'
    zk24(islvk-1+12)= 'XXXX'
!
!
!     POUR NEWTON_KRYLOV LE RESI_RELA VARIE A CHAQUE
!     ITERATION DE NEWTON, CEPENDANT LE RESI_RELA DONNE
!     PAR L'UTILISATEUR TOUT DE MEME NECESSAIRE
!     C'EST POURQUOI ON EN FAIT UNE COPIE EN POSITION 1
    zr(islvr-1+1) = epsmax
    zr(islvr-1+2) = epsmax
    zr(islvr-1+3) = fillin
    zr(islvr-1+4) = resipc
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
! FIN ------------------------------------------------------
    call jedema()
end subroutine
