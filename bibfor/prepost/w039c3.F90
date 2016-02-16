subroutine w039c3(carele, modele, ifi, form, titre, aunoeud)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/carelo.h"
#include "asterfort/chpchd.h"
#include "asterfort/detrsd.h"
#include "asterfort/imprsd.h"
#include "asterfort/irceme.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/lxlgut.h"
!
    integer :: ifi
    character(len=8) :: carele, modele
    character(len=80) :: titre
    character(len=*) :: form
    logical :: aunoeud
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     BUT:
!       IMPRIMER LES REPERES LOCAUX DES ELEMENTS
! ----------------------------------------------------------------------
!     IN MODELE  : MODELE
!     IN CARELE  : CARA_ELEM
! ----------------------------------------------------------------------
!     VARIABLES LOCALES
!
    integer :: iret,jaux
    character(len=1) :: nomcmp(3)
    character(len=8) :: typech, sdcarm, carele8
    character(len=19) :: chrel1, chrel2, chrel3, chrelno1, chrelno2, chrelno3, ligrel, celmod
    character(len=19) :: chrmed(3)
    character(len=64) :: nommed(3)
    character(len=85) :: titrz,messk(3)
    aster_logical :: l3d
    data  nomcmp / 'X' , 'Y' , 'Z' /
! ----------------------------------------------------------------------
    call jemarq()
!
    chrel1 = carele//'.REPLO_1'
    chrel2 = carele//'.REPLO_2'
    chrel3 = carele//'.REPLO_3'
!
    call carelo(modele, carele, 'V', chrel1, chrel2, chrel3)
!
    call jeexin(chrel3//'.CELD', iret)
!   il n'y a que deux vecteurs dans le cas 2d
    if (iret .ne. 0) then
        l3d = .true.
    else
        l3d = .false.
    endif
!   IMPRESSION DES CHAMPS DE VECTEURS
!
    if ( aunoeud .and. (form.eq.'MED') ) then
!       Passage des champs ELEM en ELNO
!       on complete le carele par des '_'
        jaux = lxlgut(carele)
        carele8 ='________'
        carele8(1:jaux) = carele(1:jaux)
!
        chrelno1 = carele8//'.REPLC_1'
        chrelno2 = carele8//'.REPLC_2'
        chrelno3 = carele8//'.REPLC_3'
!
!       Récupération du LIGREL
        ligrel = modele//'.MODELE'
!
        celmod = '&&W039C3.CELMOD'
        call alchml(ligrel, 'TOU_INI_ELNO', 'PGEOM_R', 'V', celmod, iret, ' ')
        if (iret .ne. 0) then
            messk(1)=ligrel
            messk(2)='PGEOM_R'
            messk(3)='TOU_INI_ELNO'
            call utmess('F', 'UTILITAI3_23', nk=3, valk=messk)
        endif
!
        call chpchd(chrel1, 'ELNO', celmod, 'OUI', 'V', chrelno1)
        call chpchd(chrel2, 'ELNO', celmod, 'OUI', 'V', chrelno2)
        if ( l3d ) then
            call chpchd(chrel3, 'ELNO', celmod, 'OUI', 'V', chrelno3)
        endif
        call detrsd('CHAMP', celmod)
!
        nommed(1) = 'RepLocal_X'
        nommed(2) = 'RepLocal_Y'
        nommed(3) = 'RepLocal_Z'
        chrmed(1) = chrelno1
        chrmed(2) = chrelno2
        chrmed(3) = chrelno3
        sdcarm=' '
        typech='ELNO'
        modele = ' '
    else
        nommed(1) = chrel1
        nommed(2) = chrel2
        nommed(3) = chrel3
        chrmed(1) = chrel1
        chrmed(2) = chrel2
        chrmed(3) = chrel3
        sdcarm=' '
        typech='ELEM'
    endif
!
    if (form .eq. 'MED') then
!     -------------------------
        call irceme(ifi, nommed(1), chrmed(1), typech, modele, 0, nomcmp, ' ', ' ', 0,&
                    0.d0, 0, 0, [0], sdcarm, iret)
        ASSERT(iret.eq.0)
!
        call irceme(ifi, nommed(2), chrmed(2), typech, modele, 0, nomcmp, ' ', ' ', 0,&
                    0.d0, 0, 0, [0], sdcarm, iret)
        ASSERT(iret.eq.0)
!
        if (l3d) then
            call irceme(ifi, nommed(3), chrmed(3), typech, modele, 0, nomcmp, ' ', ' ', 0,&
                        0.d0, 0, 0, [0], sdcarm, iret)
            ASSERT(iret.eq.0)
        endif
!
    else if (form.eq.'RESULTAT') then
!     ---------------------------
        titrz ='1er '//titre
        call imprsd('CHAMP', chrel1, ifi, titrz)
        titrz ='2eme '//titre
        call imprsd('CHAMP', chrel2, ifi, titrz)
        if (l3d) then
            titrz ='3eme '//titre
            call imprsd('CHAMP', chrel3, ifi, titrz)
        endif
    else
        ASSERT(.false.)
    endif
!
    call detrsd('CHAM_ELEM', chrel1)
    call detrsd('CHAM_ELEM', chrel2)
    if (l3d) call detrsd('CHAM_ELEM', chrel3)
!
    if ( aunoeud .and. (form.eq.'MED') ) then
        call detrsd('CHAM_ELEM', chrelno1)
        call detrsd('CHAM_ELEM', chrelno2)
        if (l3d) call detrsd('CHAM_ELEM', chrelno3)
    endif
!
    call jedema()
end subroutine
