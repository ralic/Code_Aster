subroutine slismo(stolcz, stomoz, basz)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=*) :: stomoz, stolcz, basz
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CALCUL D'UN STOC_MORSE A PARTIR D'UN STOC_LCIEL (POUR CONTENIR
!     LA MEME MATRICE)
!
!     REMARQUE : CETTE ROUTINE DEVRAIT ETRE TRES RAREMENT UTILISEE CAR
!     LE STOCKAGE MORSE PRODUIT EST EN GENERAL TROP VOLUMINEUX PUISQU'
!     IL CONTIENT TOUS LES TERMES SOUS LA LIGNE DE CIEL
!     ------------------------------------------------------------------
! IN  JXIN  K19 STOLCZ     : NOM D'UNE S.D. STOC_LCIEL
! IN  JXOUT K19 STOMOZ     : NOM D'UNE S.D. STOC_MORSE
! IN        K1  BASZ       : BASE DE CREATION POUR STOLCZ
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!     VARIABLES LOCALES
    character(len=1) :: base
    character(len=19) :: stomor, stolci
    integer :: jsmde,  neq, ieq, ilig
    integer :: jsmhc, jsmdi,  hcol, nterm, kterm
    integer, pointer :: scde(:) => null()
    integer, pointer :: schc(:) => null()
!     ------------------------------------------------------------------
!
!
!
    call jemarq()
    stolci=stolcz
    stomor=stomoz
    base=basz
!
!     -- ON DETRUIT STOMOR S'IL EXISTE DEJA :
    call detrsd('STOC_MORSE', stomor)
!
!
    call jeveuo(stolci//'.SCDE', 'L', vi=scde)
    call jeveuo(stolci//'.SCHC', 'L', vi=schc)
!
    neq=scde(1)
!
!     -- CALCUL DE NTERM :
    nterm=0
    do 1, ieq=1,neq
    hcol=schc(ieq)
    nterm=nterm+hcol
    1 end do
!
!
!     -- OBJET .SMDE :
    call wkvect(stomor//'.SMDE', base//' V I', 6, jsmde)
    zi(jsmde-1+1)=neq
    zi(jsmde-1+2)=nterm
    zi(jsmde-1+3)=1
!
!
!     -- OBJET .SMDI :
    call wkvect(stomor//'.SMDI', base//' V I', neq, jsmdi)
    nterm=0
    do 2, ieq=1,neq
    hcol=schc(ieq)
    nterm=nterm+hcol
    zi(jsmdi-1+ieq)=nterm
    2 end do
!
!
!     -- OBJET .SMHC :
    call wkvect(stomor//'.SMHC', base//' V S', nterm, jsmhc)
    kterm=0
    do 3, ieq=1,neq
    hcol=schc(ieq)
    ASSERT(hcol.le.ieq)
    do 4, ilig=ieq-hcol+1,ieq
    kterm=kterm+1
    zi4(jsmhc-1+kterm)=ilig
 4  continue
    3 end do
!
!
!
    call jedema()
end subroutine
