subroutine vrdesc(objet1, objet2, ier)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: objet1, objet2
    integer :: ier
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     VERIFICATION QUE DEUX OBJETS ONT MEME DOMAINE DE DEFINITION
!         ==> COMPARAISON DES "DESC"
!     ------------------------------------------------------------------
! IN  : OBJET1  : NOM DU 1-ER OBJET
! IN  : OBJET2  : NOM DU 2-ND OBJET
! OUT : IER     : IS   : CODE RETOUR
!                = 0 PAS D'ERREUR
!                > 0 NOMBRE DE DESCRIPTEURS DIFFERENTS
!     ------------------------------------------------------------------
!
    integer :: nbval, ival1, ival2
    character(len=19) :: nom1, nom2
    character(len=24) :: desc1, desc2
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibid, idesc1, idesc2, ival
!-----------------------------------------------------------------------
    call jemarq()
    ier = 0
    nom1 = objet1
    nom2 = objet2
    desc1 = nom1//'.DESC'
!
    call jeexin(desc1, ibid)
    if (ibid .gt. 0) then
        desc1 = nom1//'.DESC'
        desc2 = nom2//'.DESC'
    else
        desc1 = nom1//'.CELD'
        desc2 = nom2//'.CELD'
    endif
!
!
!     --- RECUPERATION DES LONGUEURS DES TABLEAUX DE REFERENCE ---
    call jelira(desc1, 'LONMAX', ival1)
    call jelira(desc2, 'LONMAX', ival2)
    if (ival1 .ne. ival2) then
        ier = ier + abs(ival1-ival2)
        nbval = min(ival1,ival2)
    else
        nbval = ival1
    endif
!
!     --- RECUPERATION DES TABLEAUX D'INFORMATIONS DE REFERENCE ---
    call jeveuo(desc1, 'L', idesc1)
    call jeveuo(desc2, 'L', idesc2)
!
!     --- CONTROLE DES REFERENCES ---
    do 10 ival = 0, nbval-1
        if (zi(idesc1+ival) .ne. zi(idesc2+ival)) ier = ier + 1
10  end do
!
!     --- LIBERATION (AVEC I/O EN DIFFERE) ---
!
    call jedema()
end subroutine
