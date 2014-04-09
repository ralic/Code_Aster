subroutine arlini(mail  ,base  ,dime  ,nbno  , &
                  nbma  ,nbnoma)

! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================

    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/assert.h"
#include "asterfort/jexnom.h"
#include "asterfort/wkvect.h"
#include "asterfort/jecreo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"

!     ARGUMENTS:
!     ----------
    character(len=8) :: mail
    character(len=1) :: base
    integer :: dime
    integer :: nbma,nbno,nbnoma

! ----------------------------------------------------------------------

! UTILITAIRE - SD MAILLAGE
! PREPARE LES OBJETS DE BASE DE LA SD MAILLAGE

! ----------------------------------------------------------------------


! IN  MAIL   : NOM DU MAILLAGE
! IN  BASE   : BASE DE CREATION 'G' OU 'V'
! IN  DIME   : DIMENSION DE L'ESPACE (2 OU 3)
! IN  NBNO   : NOMBRE DE NOEUDS DU MAILLAGE
! IN  NBMA   : NOMBRE DE MAILLES DU MAILLAGE
! IN  NBNOMA : LONGUEUR DU VECTEUR CONNECTIVITE DES MAILLES


    character(len=24) :: maidim,cooref,cooval,coodsc
    integer :: jdime ,jrefe ,jcoor ,jcods
    character(len=24) :: typmai
    integer :: jtypm
    character(len=24) :: nomnoe,nommai,connex
    integer :: ntgeo

! ----------------------------------------------------------------------

    call jemarq()

! --- NOM DES OBJETS JEVEUX

    maidim = mail(1:8)//'.DIME'
    cooref = mail(1:8)//'.COORDO    .REFE'
    cooval = mail(1:8)//'.COORDO    .VALE'
    coodsc = mail(1:8)//'.COORDO    .DESC'
    nomnoe = mail(1:8)//'.NOMNOE         '
    nommai = mail(1:8)//'.NOMMAI         '
    typmai = mail(1:8)//'.TYPMAIL        '
    connex = mail(1:8)//'.CONNEX         '

! --- VERIFICATIONS

    if ((dime < 2) .or. (dime > 3)) then
        ASSERT(.false.)
    endif

    if ((nbma <= 0) .or. (nbno <= 0) .or. (nbnoma <= 0)) then
        ASSERT(.false.)
    endif

! --- RECUPERATION DU NUMERO IDENTIFIANT LE TYPE DE CHAM_NO GEOMETRIE

    call jenonu(jexnom('&CATA.GD.NOMGD','GEOM_R'),ntgeo)

! --- DIMENSIONS

    call wkvect(maidim,base//' V I'  ,6,jdime)
    zi(jdime    ) = nbno
    zi(jdime + 1) = 0
    zi(jdime + 2) = nbma
    zi(jdime + 3) = 0
    zi(jdime + 4) = 0
    zi(jdime + 5) = dime

! --- CHAM_NO DES COORDONNEES DES NOEUDS

    call wkvect(cooref,base//' V K24',2,jrefe)
    zk24(jrefe  ) = mail
    zk24(jrefe+1) = ' '

    call wkvect(cooval,base//' V R',3*nbno,jcoor)

    call jecreo(coodsc,base//' V I')
    call jeecra(coodsc,'LONMAX',3,' ')
    call jeecra(coodsc,'DOCU',0,'CHNO')
    call jeveuo(coodsc,'E',jcods)
    zi(jcods)   =  ntgeo
    zi(jcods+1) = -3
    zi(jcods+2) = 14

! --- NOMS DES NOEUDS

    call jecreo(nomnoe,base//' N K8')
    call jeecra(nomnoe,'NOMMAX',nbno,' ')

! --- TYPE DES MAILLES

    call wkvect(typmai,base//' V I',nbma,jtypm)

! --- NOM DES MAILLES

    call jecreo(nommai,base//' N K8')
    call jeecra(nommai,'NOMMAX',nbma,' ')

! --- CONNECTIVITES DES MAILLES

    call jecrec(connex,base//' V I','NU','CONTIG','VARIABLE',nbma)
    call jeecra(connex,'LONT',nbnoma,' ')

    call jedema()

end subroutine
