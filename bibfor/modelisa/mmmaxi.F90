function mmmaxi(modelz, lisma, nbma)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    logical :: mmmaxi
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/infbav.h"
#include "asterfort/infmue.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lteatt.h"
#include "asterfort/utmasu.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: modelz
    character(len=24) :: lisma
    integer :: nbma
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CETTE FONCTION PERMET DE DETERMINER SI LES ELEMENTS D'UN GROUPE DE
! MAILLES DE CONTACT SONT TOUS AXISYMETRIQUES
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  LISMA  : LISTE DES MAILLES A VERIFIER
! IN  NBMA   : NOMBRE DE MAILLES CONTENU DANS LA LISTE LISMA
! OUT MMMAXI : .TRUE.  LE GROUPE DE MAILLE NE CONTIENT QUE DES ELEMENTS
!                      AXISYMETRIQUES
!              .FALSE. LE GROUPE DE MAILLE NE CONTIENT QUE DES ELEMENTS
!                      NON AXIS
!              ERREUR <F> LE GROUPE DE MAILLE CONTIENT UN MELANGE
!                         D'ELEMENT AXIS ET NON AXIS
!
!
!
!
    character(len=8) :: noma, modele
    character(len=16) :: notype, maisup
    character(len=24) :: typele
    integer :: numail, nutyel
    integer :: ibid, ier, ima
    integer :: numsup, nbaxis, mailvo(1)
    integer :: jmaisu, ilmail, itypel, jcoor
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    modele = modelz
    nbaxis = 0
    mmmaxi = .false.
    typele = modele(1:8)//'.MAILLE'
    maisup = '&&MMMAXI.MAISUP'
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma, ier)
!
! --- RECHERCHE DES MAILLES 2D SUPPORTS DES MAILLES DE LISMA
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(lisma, 'L', ilmail)
    call infmue()
    call utmasu(noma, '2D', nbma, zi(ilmail), maisup,&
                zr(jcoor), 0, mailvo, .false.)
    call infbav()
    call jeveuo(maisup, 'L', jmaisu)
!
! --- DETERMINATION DU TYPE DES MAILLES DE LISMA
!
    call jeveuo(typele, 'L', itypel)
!
    do 100 ima = 1, nbma
        numsup = zi(jmaisu-1+ima)
        if (numsup .ne. 0) then
            nutyel = zi(itypel-1+numsup)
            if (nutyel .ne. 0) then
                goto 50
            endif
        endif
        numail = zi(ilmail-1+ima)
        nutyel = zi(itypel-1+numail)
        if (nutyel .eq. 0) then
            goto 100
        endif
50      continue
        call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), notype)
        if (lteatt(notype,'AXIS','OUI')) then
            nbaxis = nbaxis +1
        endif
100  end do
!
    if (nbaxis .eq. nbma) then
        mmmaxi = .true.
    else if (nbaxis.eq.0) then
        mmmaxi = .false.
    else
        call utmess('F', 'CONTACT2_12')
    endif
!
    call jedetr(maisup)
    call jedema()
!
end function
