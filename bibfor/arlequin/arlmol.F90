subroutine arlmol(nomo,mailar,modarl,tabcor)

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
#include "asterfort/detrsd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jexnum.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnom.h"
#include "asterfort/jenonu.h"
#include "asterfort/assert.h"
#include "asterfort/adalig.h"
#include "asterfort/cormgi.h"
#include "asterfort/initel.h"
#include "asterfort/jedema.h"

!     ARGUMENTS:
!     ----------
    character(len=8) ::  mailar,modarl
    character(len=8) ::  nomo
    character(len=24) :: tabcor

    integer ::      ima,nbma,ibid
    integer ::      jnbno,jad,jlgrf,jdime,jtyel,jtabco
    character(len=8) ::  k8bid
    character(len=19) :: ligrmo
    integer ::      numori,ityel

! ----------------------------------------------------------------------

! ROUTINE ARLEQUIN

! CREATION DU LIGREL DU PSEUDO-MODELE

! ----------------------------------------------------------------------


! IN  NOMO   : NOM DU MODELE
! IN  MAILAR : NOM DU PSEUDO-MAILLAGE
! IN  TABCOR : TABLEAU DE CORRESPONDANCE
!            POUR CHAQUE NOUVEAU NUMERO ABSOLU DANS MAILAR
!             -> ANCIEN NUMERO ABSOLU DANS MAIL
!             -> SI NEGATIF, LA NOUVELLE MAILLE EST ISSUE D'UNE
!                DECOUPE DE LA MAILLE DE NUMERO ABSOLU ABS(NUM) DANS
!                MAIL
! IN  MODARL : NOM DU PSEUDO-MODELE

! ----------------------------------------------------------------------

    call jemarq()

! --- DESTRUCTION DU LIGREL S'IL EXISTE

    call detrsd('LIGREL',ligrmo)

! --- INITIALISATIONS

    ligrmo = modarl(1:8)//'.MODELE'

! --- INFORMATIONS SUR LE MODELE ORIGINAL

    call jeveuo(nomo(1:8)//'.MAILLE','L',jtyel)

! --- ACCES AU TABLEAU DE CORRESPONDANCE

    call jeveuo(tabcor,'L',jtabco)

! --- INFORMATIONS SUR LE MAILLAGE

    call jeveuo(mailar(1:8)//'.DIME','L',jdime)
    nbma = zi(jdime - 1 + 3)

! --- CREATION DE .NOMA + ATTRIBUT DOCU

    call wkvect(ligrmo//'.LGRF','V V K8',1,jlgrf)
    zk8(jlgrf-1+1) = mailar
    call jeecra(ligrmo//'.LGRF','DOCU',ibid,'MECA')

! --- CREATION DE L'OBJET .LIEL: ON LE CREE AU MAX. AUTANT DE LIEL
! --- QUE DE MAILLES

    call jecrec(ligrmo//'.LIEL','V V I','NU','CONTIG','VARIABLE', &
                nbma)
    call jeecra(ligrmo//'.LIEL','LONT',2*nbma,k8bid)

    do 90 ima = 1,nbma
    
    ! --- CREATION OBJET DE LA COLLECTION
    
        call jecroc(jexnum(ligrmo//'.LIEL',ima))
        call jeecra(jexnum(ligrmo//'.LIEL',ima),'LONMAX',2,k8bid)
        call jeveuo(jexnum(ligrmo//'.LIEL',ima),'E',jad)
    
    ! --- NUMERO DANS LE MAILLAGE ORIGINAL
    
        numori = zi(jtabco+ima-1)
    
    ! --- TYPE DE LA MAILLE DANS LE PSEUDO-MODELE
    
        if (numori < 0) then
            numori = abs(numori)
        endif
        ityel = zi(jtyel-1+numori)
    
    ! --- PAS D'EF AFFECTE SUR LA MAILLE !
    
        if (ityel == 0) then
            ASSERT(.false.)
        else
            zi(jad)   = ima
            zi(jad+1) = ityel
        endif
    90 end do

! --- PAS DE NOEUDS TARDIFS

    call wkvect(ligrmo//'.NBNO','V V I',1,jnbno)
    zi(jnbno-1+1) = 0

! --- ADAPTATION DE .LIEL

    call adalig(ligrmo)

! --- CREATION DE L'OBJET .REPE

    Call cormgi('V',ligrmo)

! --- INITIALISATION DU LIGREL (OBJETS PRNM/PRNS)

    call initel(ligrmo)

    call jedema()

end subroutine
