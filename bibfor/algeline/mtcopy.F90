subroutine mtcopy(matin, matout, ier)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdscr.h"
#include "asterfort/utmess.h"
#include "asterfort/vrrefe.h"
    character(len=*) :: matin, matout
    integer :: ier
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     RECOPIE LES VALEURS DE LA MATRICE MATIN  DANS LA MATRICE MATOUT
!     ------------------------------------------------------------------
!     PRECAUTION D'EMPLOI :
!        1) LA MATRICE "MATOUT" DOIT EXISTER ET AVOIR LA MEME STRUCTURE
!     QUE "MATIN"
!        2) ON RECOPIE LE .CCID DE MATIN DANS
!     MATOUT, SI MATOUT POSSEDAIT DEJA CE CHAMP ON LE DETRUIT.
!     ------------------------------------------------------------------
!     RAPPEL :   UNE MATRICE  "MAT" EXISTE
!          S'IL EXISTE UN OBJET SIMPLE  MAT//"REFE"
!          ET UNE COLLECTION NUMEROTEE  MAT//"VALE"
!     ------------------------------------------------------------------
!
!
!     ------------------------------------------------------------------
    integer :: lmatou, lmatin, nimpou
    character(len=8) :: nomddl
    character(len=19) :: mati19, mato19
    character(len=24) :: nmatou, nmatin
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data nomddl/'        '/
!     ------------------------------------------------------------------
!
!     --- CONTROLE DES REFERENCES ---
    call jemarq()
    call vrrefe(matin, matout, ier)
    mati19 = matin
    mato19 = matout
    if (ier .ne. 0) then
        valk(1) = mati19
        valk(2) = mato19
        call utmess('F', 'ALGELINE2_11', nk=2, valk=valk)
!
    else
!        --- TYPE DES VALEURS, NOMBRE DE BLOCS, LONGUEUR D'UN BLOC ---
        call mtdscr(matin)
        nmatin = matin(1:19)//'.&INT'
        call jeveuo(matin(1:19)//'.&INT', 'E', lmatin)
        call mtdscr(matout)
        nmatou = matout(1:19)//'.&INT'
        call jeveuo(matout(1:19)//'.&INT', 'E', lmatou)
!
! --- GESTION DES .CCID .CCLL .CCVA
!
        nimpou = zi(lmatou+7)
        if (nimpou .ne. 0) then
            call jedetr(mato19//'.CCID')
            call jedetr(mato19//'.CCLL')
            call jedetr(mato19//'.CCII')
            call jedetr(mato19//'.CCVA')
            zi(lmatou+7) = 0
            zi(lmatou+15) = 0
            zi(lmatou+16) = 0
        endif
!
!
! --- RECOPIE DU .VALE ET DE .CCID, .CCLL, .CCVA
        call mtcmbl(1, 'R', [1.d0], nmatin, nmatou,&
                    nomddl, ' ', 'ELIM=')
    endif
!
    call jedema()
end subroutine
