subroutine mtmchc(matas, action)
    implicit none
#include "jeveux.h"
!
#include "asterfort/asmchc.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=*) :: matas, action
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! OBJET :
!        TRAITEMENT DES CHARGES CINEMATIQUES DANS UNE MATRICE ASSEMBLEE
!        SI ACTION='ELIMF' :
!            - ON UTILISE .CCID POUR :
!                CALCULER .CCVA, .CCLL, .CCII
!                MODIFIER .VALM
!        SI ACTION='ELIML' :
!            - ON UTILISE .CCVA POUR :
!                RETABLIR (EN PARTIE SEULEMENT) .VALM
!                DETRUIRE .CCVA, .CCLL, .CCII
!-----------------------------------------------------------------------
! VAR  MATAS   K*19    : NOM DE LA MATR_ASSE
! IN   ACTION  K*5     : /'ELIMF' /'ELIML'
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    character(len=8) :: kbid
    character(len=14) :: nu
    character(len=19) :: mat, nomsto
    integer :: typmat, ielim, jelim, kdeb, kfin, kkeli, ilig, jcol
    integer :: jsmhc, jsmdi, jvalm, jvalm2, jccva, jccll, nelim
    integer :: jrefa, jnequ, ieq, k, deciel, neq, ier, jnulg
    integer :: iret, nblocm, jremp, decjel, iremp, jccid, keta, imatd
    logical :: nonsym
!----------------------------------------------------------------------
    call jemarq()
    mat = matas
!     CALL CHEKSD('sd_matr_asse',MAT,IRET)
    call jeveuo(mat//'.REFA', 'E', jrefa)
    call jeexin(mat//'.CCID', ier)
    if (zk24(jrefa-1+3) .eq. ' ') then
        call assert(ier.eq.0)
        goto 9999
    else
        call assert(ier.gt.0)
    endif
!
    if (action .eq. 'ELIMF') then
        call assert(zk24(jrefa-1+3).eq.'ELIML')
        call asmchc(mat)
        goto 9999
    else if (action.eq.'ELIML') then
        call assert(zk24(jrefa-1+3).eq.'ELIMF')
!        TRAITEMENT CI-DESSOUS
    else
        call assert(.false.)
    endif
!
!
    call jeveuo(mat//'.CCVA', 'L', jccva)
    call jeveuo(mat//'.CCLL', 'L', jccll)
    call jeveuo(mat//'.CCID', 'L', jccid)
!
!
!
    nu = zk24(jrefa-1+2)(1:14)
    call jeexin(nu//'.NUML.DELG', imatd)
    if (imatd .ne. 0) then
        call jeveuo(nu//'.NUML.NEQU', 'L', jnequ)
        call jeveuo(nu//'.NUML.NULG', 'L', jnulg)
    else
        call jeveuo(nu//'.NUME.NEQU', 'L', jnequ)
    endif
    neq = zi(jnequ)
!
!
    nomsto = nu//'.SMOS'
    call jeveuo(nomsto//'.SMHC', 'L', jsmhc)
    call jeveuo(nomsto//'.SMDI', 'L', jsmdi)
!
!
!     -- RECUPERATION DE .VALM
!        CALCUL DE TYPMAT ET NONSYM :
!     ------------------------------------
    call jelira(jexnum(mat//'.VALM', 1), 'TYPE', iret, kbid)
    typmat = 1
    if (kbid(1:1) .eq. 'C') typmat = 2
    nonsym=.false.
    call jelira(mat//'.VALM', 'NMAXOC', nblocm, kbid)
    call assert(nblocm.eq.1 .or. nblocm.eq.2)
    if (nblocm .eq. 2) nonsym=.true.
    call jeveuo(jexnum(mat//'.VALM', 1), 'E', jvalm)
    if (nonsym) call jeveuo(jexnum(mat//'.VALM', 2), 'E', jvalm2)
!
!
!     -- CALCUL DE ELIM(*) ET NELIM :
!     -----------------------------------
!     ELIM    I(*)    : TABLEAU ENTIER DE DIM = NEQ DONNANT LES
!                       LES NUMEROS DES EQUATIONS A ELIMINER ET LEUR
!                       NUMERO D'ELIMINATION
!                       ZI(KKELI-1+IEQ) = / 0      -> PAS ELIMINE
!                                         / IELIM  -> ELIMINE
!     NELIM   I       : NOMBRE D'EQUATIONS DE LA MATRICE A ELIMINER
    call wkvect('&&MTMCHC.ELIM', 'V V I', neq, kkeli)
    call jeveuo(mat//'.CCID', 'L', jccid)
    nelim=0
    do 1, ieq=1,neq
    if (imatd .ne. 0) then
        keta=zi(jccid-1+zi(jnulg+ieq-1))
    else
        keta=zi(jccid-1+ieq)
    endif
    call assert(keta.eq.1 .or. keta.eq.0)
    if (keta .eq. 1) then
        nelim=nelim+1
        zi(kkeli-1+ieq)=nelim
    else
        zi(kkeli-1+ieq)=0
    endif
    1 end do
!
!
!     -- RECOPIE DE .CCVA DANS .VALM :
!     -----------------------------------------
    call wkvect('&&MTMCHC.REMPLIS', 'V V I', nelim, jremp)
    kfin=0
    do 121 jcol = 1, neq
        kdeb = kfin + 1
        kfin = zi(jsmdi-1+jcol)
        jelim = zi(kkeli-1+jcol)
!
        if (jelim .ne. 0) then
            deciel=zi(jccll-1+3*(jelim-1)+3)
            do 111, k=kdeb, kfin - 1
            ilig = zi4(jsmhc-1+k)
            ielim = zi(kkeli-1+ilig)
            if (ielim .eq. 0) then
                zi(jremp-1+jelim)=zi(jremp-1+jelim)+1
                iremp=zi(jremp-1+jelim)
                if (typmat .eq. 1) then
                    zr(jvalm-1+k)=zr(jccva-1+deciel+iremp)
                else
                    zc(jvalm-1+k)=zc(jccva-1+deciel+iremp)
                endif
            endif
111          continue
!
        else
            do 112 k = kdeb, kfin - 1
                ilig = zi4(jsmhc-1+k)
                ielim = zi(kkeli-1+ilig)
                decjel=zi(jccll-1+3*(ielim-1)+3)
                if (ielim .ne. 0) then
                    zi(jremp-1+ielim)=zi(jremp-1+ielim)+1
                    iremp=zi(jremp-1+ielim)
                    if (typmat .eq. 1) then
                        if (nonsym) then
                            zr(jvalm2-1+k)=zr(jccva-1+decjel+iremp)
                        else
                            zr(jvalm-1+k)=zr(jccva-1+decjel+iremp)
                        endif
                    else
                        if (nonsym) then
                            zc(jvalm2-1+k)=zc(jccva-1+decjel+iremp)
                        else
                            zc(jvalm-1+k)=zc(jccva-1+decjel+iremp)
                        endif
                    endif
                endif
112          continue
        endif
!
121  end do
!
!
    zk24(jrefa-1+3)='ELIML'
    call jedetr(mat//'.CCVA')
    call jedetr(mat//'.CCLL')
    call jedetr(mat//'.CCII')
    call jedetr('&&MTMCHC.REMPLIS')
    call jedetr('&&MTMCHC.ELIM')
!
!
9999  continue
!     CALL CHEKSD('sd_matr_asse',MAT,IRET)
    call jedema()
end subroutine
