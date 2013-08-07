subroutine liglma(ligrel, nbma, linuma, linute)
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=*) :: linuma, linute
    character(len=19) :: ligrel
    integer :: nbma
!
! ----------------------------------------------------------------------
! BUT : EXTRACTION D'UN LIGREL LA LISTE DES NUMEROS DE MAILLES ET LA
!       LISTE DES NUMERO DE TYPE_ELEMENT
!
! IN/JXIN  LIGREL  : LIGREL
! OUT      NBMA    : NOMBRE DE MAILLES AFFECTEES DANS LE LIGREL
! IN/JXOUT LINUMA  : OBJET (V I) QUI CONTIENDRA LES NUMEROS DES MAILLES
!                    ASSOCIEES AUX ELEMENTS DU LIGREL
! IN/JXOUT LINUTE  : OBJET (V I) QUI CONTIENDRA LES NUMEROS DES
!                    TYPE_ELEMENT ASSOCIES AUX ELEMENTS DU LIGREL
! ----------------------------------------------------------------------
    integer :: nbgrel, igrel, iel, numa, nute, n1, nbel
    integer :: jnuma, jnute, ico, jliel
!
! ----------------------------------------------------------------------
    call jemarq()
!
    call jelira(ligrel//'.LIEL', 'NMAXOC', nbgrel)
!
!     -- CALCUL DE NBMA :
    nbma=0
    do 10,igrel=1,nbgrel
    call jelira(jexnum(ligrel//'.LIEL', igrel), 'LONMAX', n1)
    nbel=n1-1
    nbma=nbma+nbel
    10 end do
    ASSERT(nbma.gt.0)
!
!     -- CALCUL DE LINUMA ET LINUTE :
    call wkvect(linuma, 'V V I', nbma, jnuma)
    call wkvect(linute, 'V V I', nbma, jnute)
!
    ico=0
    do 30,igrel=1,nbgrel
    call jelira(jexnum(ligrel//'.LIEL', igrel), 'LONMAX', n1)
    call jeveuo(jexnum(ligrel//'.LIEL', igrel), 'L', jliel)
    nbel=n1-1
    nute=zi(jliel-1+nbel)
    do 20,iel=1,nbel
    ico=ico+1
    numa=zi(jliel-1+iel)
    zi(jnuma-1+ico)=numa
    zi(jnute-1+ico)=nute
20  continue
    30 end do
!
    call jedema()
end subroutine
