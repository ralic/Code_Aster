subroutine orient(mdgene, sst, jcoor, ino, coordo,&
                  itran)
    implicit none
! ----------------------------------------------------------------------
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
!
!     BUT: CALCUL DES COORDONNEES D'UN NOEUD D'UNE SOUS-STRUCTURE
!          DANS LE REPERE PHYSIQUE
!
! IN  : MDGENE : NOM UTILISATEUR DU MODELE GENERALISE
! IN  : SST    : NOM DE LA SOUS-STRUCTURE
! IN  : JCOOR  : ADRESSE JEVEUX DU .COORDO.VALE DU MAILLAGE DE SST
! IN  : INO    : DECALAGE DONNANT L'ADRESSE JEVEUX DU NOEUD
! OUT : COORDO : COORDONNEES DU NOEUD DANS LE REPERE PHYSIQUE
! IN  : ITRAN  : ENTIER = 1 : PRISE EN COMPTE DE LA TRANSLATION
!                       = 0 : NON PRISE EN COMPTE DE LA TRANSLATION
!
! ----------------------------------------------------------------------
!
!
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/intet0.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/pmppr.h"
#include "asterfort/r8inir.h"
!
!
!-----------------------------------------------------------------------
    integer :: ibid, itran, k, l, llrot, lltra, nbcmpm
!
!-----------------------------------------------------------------------
    parameter   (nbcmpm=10)
    character(len=8) :: sst
    character(len=24) :: mdgene
    integer :: jcoor, ino
    real(kind=8) :: matrot(nbcmpm, nbcmpm), xanc(3), xnew, coordo(3), r8bid
    real(kind=8) :: matbuf(nbcmpm, nbcmpm), mattmp(nbcmpm, nbcmpm)
!
!     ------------------------------------------------------------------
!
    call jemarq()
    call jenonu(jexnom(mdgene(1:14)//'.MODG.SSNO', sst), ibid)
    call jeveuo(jexnum(mdgene(1:14)//'.MODG.SSOR', ibid), 'L', llrot)
    call jeveuo(jexnum(mdgene(1:14)//'.MODG.SSTR', ibid), 'L', lltra)
!
    call intet0(zr(llrot), mattmp, 3)
    call intet0(zr(llrot+1), matrot, 2)
    r8bid = 0.d0
    call r8inir(nbcmpm*nbcmpm, r8bid, matbuf, 1)
    call pmppr(mattmp, nbcmpm, nbcmpm, 1, matrot,&
               nbcmpm, nbcmpm, 1, matbuf, nbcmpm,&
               nbcmpm)
    r8bid = 0.d0
    call r8inir(nbcmpm*nbcmpm, r8bid, matrot, 1)
    call intet0(zr(llrot+2), mattmp, 1)
    call pmppr(matbuf, nbcmpm, nbcmpm, 1, mattmp,&
               nbcmpm, nbcmpm, 1, matrot, nbcmpm,&
               nbcmpm)
!
    do 10 k = 1, 3
        xanc(k)=zr(jcoor+(ino-1)*3+k-1)
10  end do
!
    do 20 k = 1, 3
        xnew=0.d0
        do 30 l = 1, 3
            xnew=xnew+matrot(k,l)*xanc(l)
30      continue
        if (itran .eq. 1) then
            coordo(k)=xnew+zr(lltra+k-1)
        else if (itran.eq.0) then
            coordo(k)=xnew
        else
            ASSERT(.false.)
        endif
20  end do
!
    call jedema()
end subroutine
