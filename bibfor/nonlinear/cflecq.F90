subroutine cflecq(iform, noma, nomo, defico, nsuco,&
                  nnoco0, listno, poinsn, nnoco)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/iscoqu.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: nomo, noma
    integer :: iform, nsuco
    integer :: nnoco0, nnoco
    character(len=24) :: defico
    character(len=24) :: listno
    character(len=24) :: poinsn
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES - ELIMINATION)
!
! CREATION D'UNE LISTE DES NOEUDS MILIEUX DES FACES DES COQUES_3D
!
! ----------------------------------------------------------------------
!
!
! IN  IFORM  : FORMULATION DISCRETE (1) OU CONTINUE (2)
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  DEFICO : NOM SD CONTACT DEFINITION
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! IN  NNOCO0 : NOMBRE TOTAL DE NOEUDS DES SURFACES
! OUT POINSN : POINTEUR MISE A JOUR POUR PSURNO
! OUT LISTNO : LISTE DES NOEUDS RESTANTES (LONGUEUR NNOCO)
! OUT NNOCO  : NOMBRE DE NOEUDS AU FINAL
!
!
!
!
    character(len=24) :: contno, contma
    integer :: jnoco, jmaco
    integer ::  jelino
    integer :: jdecno, jno
    integer :: jdecma
    integer :: isuco, i, ino, k, ima, nutyp, noeumi
    integer :: elimno, nbma, nbno
    integer :: iatyma, itypma
    integer :: nummai, numnoe
    character(len=8) :: nomtm, nommai, nomnoe
    logical(kind=1) :: lcoque
    integer, pointer :: indino(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CREATION DES VECTEURS DE TRAVAIL TEMPORAIRES
!
    AS_ALLOCATE(vi=indino, size=nnoco)
    call wkvect(poinsn, 'V V I', nsuco+1, jelino)
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    contno = defico(1:16)//'.NOEUCO'
    contma = defico(1:16)//'.MAILCO'
    call jeveuo(contma, 'L', jmaco)
    call jeveuo(contno, 'L', jnoco)
!
! --- INITIALISATIONS
!
    elimno = 0
    call jeveuo(noma(1:8)//'.TYPMAIL', 'L', iatyma)
!
! --- REPERAGE DES NOEUDS MILIEUX POUR CHAQUE SURFACE
!
    do 110 isuco = 1, nsuco
!
        call cfnbsf(defico, isuco, 'MAIL', nbma, jdecma)
        zi(jelino+isuco) = zi(jelino+isuco-1)
!
        do 90 ima = 1, nbma
!
!         --- NUMERO MAILLE COURANTE
!
            nummai = zi(jmaco+jdecma+ima-1)
!
!         --- TYPE MAILLE COURANTE
!
            itypma = iatyma - 1 + nummai
            nutyp = zi(itypma)
            call jenuno(jexnum('&CATA.TM.NOMTM', nutyp), nomtm)
            call jenuno(jexnum(noma//'.NOMMAI', nummai), nommai)
!
            if (nomtm(1:5) .eq. 'QUAD9') then
                call iscoqu(nomo, nummai, lcoque)
                noeumi = 9
            else if (nomtm(1:5).eq.'TRIA7') then
                call iscoqu(nomo, nummai, lcoque)
                noeumi = 7
            else
                lcoque = .false.
            endif
!
!         --- NUMERO ABSOLU DU NOEUD VISE
!
            if (lcoque) then
                call jeveuo(jexnum(noma//'.CONNEX', nummai), 'L', jdecno)
                numnoe = zi(jdecno+noeumi-1)
                call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
            endif
!
!         --- ELIMINATION NOEUD MILIEU
!
            if (lcoque) then
                call cfnbsf(defico, isuco, 'NOEU', nbno, jdecno)
                do 20 i = 1, nbno
                    ino = zi(jnoco+jdecno+i-1)
                    if (ino .eq. numnoe) then
                        indino(1+jdecno+i-1) = 1
                        zi(jelino+isuco) = zi(jelino+isuco) + 1
                        elimno = elimno + 1
                        goto 90
                    endif
20              continue
            endif
!
90      continue
!
!
110  end do
!
! --- COQUE_3D NON UTILISABLE EN FORMULATION CONTINUE
!
    if ((iform.eq.2) .and. (elimno.gt.0)) then
        call utmess('F', 'CONTACT_94')
    endif
!
! --- RECOPIE DES NOEUDS NON ELIMINES DANS TABLEAU DE TRAVAIL
!
    nnoco = nnoco0 - elimno
    call wkvect(listno, 'V V I', nnoco, jno)
!
! --- TRAITEMENT DES NOEUDS
!
    k = 0
    do 120 i = 1, nnoco0
        if (indino(i) .eq. 0) then
            k = k + 1
            zi(jno+k-1) = zi(jnoco+i-1)
        endif
120  end do
    ASSERT(k.eq.nnoco)
!
! --- DESTRUCTION DES VECTEURS DE TRAVAIL TEMPORAIRES
!
    AS_DEALLOCATE(vi=indino)
!
    call jedema()
end subroutine
