subroutine asmchc(matas)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=*) :: matas
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! OBJET :
!        TRAITEMENT DES CHARGES CINEMATIQUES DANS UNE MATRICE ASSEMBLEE
!        CALCUL DES OBJETS  .CCLL,  .CCVA,  .CCII
!-----------------------------------------------------------------------
! VAR  MATAS   K*19    : NOM DE LA MATR_ASSE
!-----------------------------------------------------------------------
    complex(kind=8) :: dcmplx
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    character(len=1) :: base
    character(len=8) :: kbid
    character(len=14) :: nu
    character(len=19) :: mat, nomsto
    integer :: typmat, ielim, jelim, kdeb, kfin, nccva, ilig, jcol
    integer :: jsmhc, jvalm, jvalm2, jccva, jccll, nelim
    integer :: iret2, jnequ, ieq, k, deciel, nterm, neq, ier, imatd
    integer :: nblocm, jccii, decjel, iremp, keta
    aster_logical :: nonsym
    integer, pointer :: elim(:) => null()
    integer, pointer :: remplis(:) => null()
    integer, pointer :: ccid(:) => null()
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: smdi(:) => null()
    integer, pointer :: nulg(:) => null()
!----------------------------------------------------------------------
    call jemarq()
    mat = matas
!     CALL CHEKSD('SD_MATR_ASSE',MAT,IRET)
    call jeexin(mat//'.CCVA', ier)
    ASSERT(ier.eq.0)
    call jeexin(mat//'.CCID', ier)
    if (ier .eq. 0) goto 9999
!
    call jelira(mat//'.REFA', 'CLAS', cval=base)
    call jeveuo(mat//'.REFA', 'E', vk24=refa)
    nu = refa(2)(1:14)
    call jeexin(nu//'.NUML.DELG', imatd)
    if (imatd .ne. 0) then
        call jeveuo(nu//'.NUML.NEQU', 'L', jnequ)
        call jeveuo(nu//'.NUML.NULG', 'L', vi=nulg)
    else
        call jeveuo(nu//'.NUME.NEQU', 'L', jnequ)
    endif
    neq = zi(jnequ)
!
!
!     -- ON DETRUIT LES OBJETS S'ILS EXISTENT DEJA :
    ASSERT(refa(3).ne.'ELIMF')
    call jedetr(mat//'.CCLL')
    call jedetr(mat//'.CCVA')
    call jedetr(mat//'.CCII')
!
!     -- CALCUL DE ELIM(*) ET NELIM :
!     -----------------------------------
!     ELIM    I(*)    : TABLEAU ENTIER DE DIM = NEQ DONNANT LES
!                       LES NUMEROS DES EQUATIONS A ELIMINER ET LEUR
!                       NUMERO D'ELIMINATION
!                       ZI(KKELI-1+IEQ) = / 0      -> PAS ELIMINE
!                                         / IELIM  -> ELIMINE
!     NELIM   I       : NOMBRE D'EQUATIONS DE LA MATRICE A ELIMINER
    AS_ALLOCATE(vi=elim, size=neq)
    call jeveuo(mat//'.CCID', 'L', vi=ccid)
    nelim=0
    do 1 ieq = 1, neq
        if (imatd .ne. 0) then
            keta=ccid(nulg(ieq))
        else
            keta=ccid(ieq)
        endif
        ASSERT(keta.eq.1 .or. keta.eq.0)
        if (keta .eq. 1) then
            nelim=nelim+1
            elim(ieq)=nelim
        else
            elim(ieq)=0
        endif
  1 end do
!
!
!
    if (nelim .eq. 0) then
        refa(3)='ELIMF'
        goto 9999
    endif
!     -----------------------------------------------------------------
!
    nomsto = nu//'.SMOS'
!
!
    call jeexin(nomsto//'.SMHC', iret2)
    ASSERT(iret2.gt.0)
    call jeveuo(nomsto//'.SMHC', 'L', jsmhc)
    call jeveuo(nomsto//'.SMDI', 'L', vi=smdi)
!
!
!
!     -- CALCUL DE .CCLL :
!     -----------------------------------------
    call wkvect(mat//'.CCLL', base//' V I ', 3*nelim, jccll)
!
    kfin=0
    do 21 jcol = 1, neq
        kdeb = kfin + 1
        kfin = smdi(jcol)
        jelim = elim(jcol)
!
        if (jelim .ne. 0) then
            zi(jccll-1+3*(jelim-1)+1) = jcol
            do 11 k = kdeb, kfin - 1
                ilig = zi4(jsmhc-1+k)
                ielim = elim(ilig)
                if (ielim .eq. 0) zi( jccll-1+3*(jelim-1)+2)=zi(jccll-1+ 3*(jelim-1)+2 ) +1
 11         continue
!
        else
            do 12 k = kdeb, kfin - 1
                ilig = zi4(jsmhc-1+k)
                ielim = elim(ilig)
                if (ielim .ne. 0) zi( jccll-1+3*(ielim-1)+2)=zi(jccll-1+ 3*(ielim-1)+2 ) +1
 12         continue
        endif
 21 end do
!
!
!     -- CALCUL DE NCCVA ET .CCLL(3*(I-1)+3) :
!     -----------------------------------------
    deciel=0
    do ielim = 1, nelim
        nterm=zi(jccll-1+3*(ielim-1)+2)
        zi(jccll-1+3*(ielim-1)+3)=deciel
        deciel=deciel+nterm
    end do
    nccva=max(deciel,1)
!
!
!     -- RECUPERATION DE .VALM
!        CALCUL DE TYPMAT ET NONSYM :
!     ------------------------------------
    call jelira(jexnum(mat//'.VALM', 1), 'TYPE', cval=kbid)
    typmat = 1
    if (kbid(1:1) .eq. 'C') typmat = 2
    nonsym=.false.
    call jelira(mat//'.VALM', 'NMAXOC', nblocm)
    ASSERT(nblocm.eq.1 .or. nblocm.eq.2)
    if (nblocm .eq. 2) nonsym=.true.
    call jeveuo(jexnum(mat//'.VALM', 1), 'E', jvalm)
    if (nonsym) call jeveuo(jexnum(mat//'.VALM', 2), 'E', jvalm2)
!
!
!     -- ALLOCATION DE .CCVA ET .CCII :
!     ------------------------------------
    call wkvect(mat//'.CCVA', base//' V '//kbid(1:1), nccva, jccva)
    call wkvect(mat//'.CCII', base//' V I', nccva, jccii)
!
!
!     -- REMPLISSAGE DE .CCII ET .CCVA :
!     -----------------------------------------
!
    AS_ALLOCATE(vi=remplis, size=nelim)
    kfin=0
    do 121 jcol = 1, neq
        kdeb = kfin + 1
        kfin = smdi(jcol)
        jelim = elim(jcol)
!
        if (jelim .ne. 0) then
            deciel=zi(jccll-1+3*(jelim-1)+3)
            do 111 k = kdeb, kfin - 1
                ilig = zi4(jsmhc-1+k)
                ielim = elim(ilig)
                if (ielim .eq. 0) then
                    remplis(jelim)=remplis(jelim)+1
                    iremp=remplis(jelim)
                    zi(jccii-1+deciel+iremp)=ilig
                    if (typmat .eq. 1) then
                        zr(jccva-1+deciel+iremp)= zr(jvalm-1+k)
                    else
                        zc(jccva-1+deciel+iremp)= zc(jvalm-1+k)
                    endif
                endif
111         continue
!
        else
            do 112 k = kdeb, kfin - 1
                ilig = zi4(jsmhc-1+k)
                ielim = elim(ilig)
                decjel=zi(jccll-1+3*(ielim-1)+3)
                if (ielim .ne. 0) then
                    remplis(ielim)=remplis(ielim)+1
                    iremp=remplis(ielim)
                    zi(jccii-1+decjel+iremp)=jcol
                    if (typmat .eq. 1) then
                        if (nonsym) then
                            zr(jccva-1+decjel+iremp)= zr(jvalm2-1+k)
                        else
                            zr(jccva-1+decjel+iremp)= zr(jvalm-1+k)
                        endif
                    else
                        if (nonsym) then
                            zc(jccva-1+decjel+iremp)= zc(jvalm2-1+k)
                        else
                            zc(jccva-1+decjel+iremp)= zc(jvalm-1+k)
                        endif
                    endif
                endif
112         continue
        endif
!
121 end do
!
!
!---  "SIMPLIFICATION" DE .VALM : 1. SUR LA DIAGONALE ET 0. EN DEHORS
!---------------------------------------------------------------------
!     DANS LE CAS PARALLELE SUR N PROCS, IL EST A NOTER QU'UN N SE
!     TROUVERA SUR LA DIAGONALE ET QU'UN N L'EQUILIBRERA DANS LE
!     SECOND MEMBRE
    kfin=0
    do 221 jcol = 1, neq
        kdeb = kfin + 1
        kfin = smdi(jcol)
        jelim = elim(jcol)
!
        if (jelim .ne. 0) then
            if (typmat .eq. 1) then
                zr(jvalm-1+kfin)=1.d0
                if (nonsym) zr(jvalm2-1+kfin)=1.d0
            else
                zc(jvalm-1+kfin)=dcmplx(1.d0,0.d0)
                if (nonsym) zc(jvalm2-1+kfin)=dcmplx(1.d0,0.d0)
            endif
        endif
!
        if (jelim .ne. 0) then
            do 211 k = kdeb, kfin -1
                if (typmat .eq. 1) then
                    zr(jvalm-1+k)=0.d0
                    if (nonsym) zr(jvalm2-1+k)=0.d0
                else
                    zc(jvalm-1+k)=dcmplx(0.d0,0.d0)
                    if (nonsym) zc(jvalm2-1+k)=dcmplx(0.d0,0.d0)
                endif
211         continue
!
        else
            do 212 k = kdeb, kfin -1
                ilig = zi4(jsmhc-1+k)
                ielim = elim(ilig)
                if (ielim .ne. 0) then
                    if (typmat .eq. 1) then
                        zr(jvalm-1+k)=0.d0
                        if (nonsym) zr(jvalm2-1+k)=0.d0
                    else
                        zc(jvalm-1+k)=dcmplx(0.d0,0.d0)
                        if (nonsym) zc(jvalm2-1+k)=dcmplx(0.d0,0.d0)
                    endif
                endif
212         continue
        endif
!
221 end do
!
    refa(3)='ELIMF'
!
!
!
!
9999 continue
    AS_DEALLOCATE(vi=remplis)
    AS_DEALLOCATE(vi=elim)
!     CALL CHEKSD('sd_matr_asse',MAT,IRET)
    call jedema()
end subroutine
