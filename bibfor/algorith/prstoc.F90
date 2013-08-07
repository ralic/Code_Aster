subroutine prstoc(vecsol, vestoc, j, k, iad,&
                  nbvale, nbrefe, nbdesc)
    implicit none
!
!--------------------------------------------------------------------
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
!--------------------------------------------------------------------
!
!         ROUTINE STOCKANT LE VECTEUR PRESSION
!         ISSUE D' UNE RESOLUTION DE LAPLACE
! IN : VECSOL : VECTEUR SOLUTION K19
! IN : J : INDICE DE BOUCLE
! IN : IAD : ADRESSE DU VECTEUR DES NOMS DES CHAMNOS STOCKES
! IN : NBVALE,NBREFE,NBDESC : DIMENSIONS DE VECTEURS POUR UN CHAMNO
!---------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: ivale, idesc, irefe, ivalp, idesp, irefp, j, k
    integer :: nbrefe, nbvale, nbdesc, iad, nbvec
    character(len=19) :: vecsol, vestoc
    character(len=24) :: chaine
!
! -------------------------------------------------------------------
!----------------CREATION DU VECTEUR PRESSION -----------------------
!
! -----------CREATION DU TABLEAU DE VECTEURS CONTENANT---------------
!--------------------------LA PRESSION-------------------------------
!
!-----------------------------------------------------------------------
    integer :: kb
!-----------------------------------------------------------------------
    call jemarq()
    chaine = 'CBIDON'
!
    call codent(j, 'D0', chaine(1:5))
    zk24(iad+k-1) = vestoc(1:14)//chaine(1:5)
!
    call wkvect(zk24(iad+k-1)(1:19)//'.VALE', 'V V R', nbvale, ivalp)
    call wkvect(zk24(iad+k-1)(1:19)//'.REFE', 'V V K24', nbrefe, irefp)
    call wkvect(zk24(iad+k-1)(1:19)//'.DESC', 'V V I', nbdesc, idesp)
!
    call jeveuo(vecsol//'.VALE', 'L', ivale)
    call jelira(vecsol//'.VALE', 'LONMAX', nbvec)
    call jeveuo(vecsol//'.DESC', 'L', idesc)
    call jeveuo(vecsol//'.REFE', 'L', irefe)
!
!-------------STOCKAGE DANS LE VECTEUR CREE -------------------------
!
    call dcopy(nbvec, zr(ivale), 1, zr(ivalp), 1)
!
    do 13 kb = 1, nbdesc
        zi(idesp+kb-1) = zi(idesc+kb-1)
13  continue
!
    do 14 kb = 1, nbrefe
        zk24(irefp+kb-1) = zk24(irefe+kb-1)
14  continue
!
!
    call detrsd('CHAM_NO', vecsol)
!
    call jedema()
end subroutine
