subroutine cbconv(char)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
! BUT : CREATION ET CHARGEMENT DE L'OBJET 'CHAR'.CHTH.CONVE.VALE
!       CONTENANT LE NOM DU CHAMP DE VITESSE DE TRANSPORT
!       DANS LE CAS DE L'EQUATION DE DIFFUSION-CONVECTION
!
! SORTIE EN ERREUR : SI LE MOT FACTEUR CONVECTION SE TROUVE PLUS D'UNE
!                    FOIS DANS LA COMMANDE
!
! ARGUMENT D'ENTREE:
!      CHAR  : NOM UTILISATEUR DU RESULTAT DE CHARGE
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/chpver.h"
#include "asterfort/getvid.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=8) :: char, vitess
    character(len=19) :: carte
!
!     CREATION ET CHARGEMENT DE L'OBJET 'CHAR'.CHTH.CONVE
!
!-----------------------------------------------------------------------
    integer ::  ier, nconv, nvites
    character(len=8), pointer :: vale(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    vitess = '        '
    call getfac('CONVECTION', nconv)
    if (nconv .gt. 1) then
        call utmess('F', 'MODELISA3_60')
    else if (nconv.eq.1) then
        carte = char//'.CHTH.CONVE'
        call jecreo(carte//'.VALE', 'G V K8')
        call jeecra(carte//'.VALE', 'LONMAX', 1)
        call jeveuo(carte//'.VALE', 'E', vk8=vale)
        call getvid('CONVECTION', 'VITESSE', iocc=1, scal=vitess, nbret=nvites)
        vale(1) = vitess
        call chpver('F', vale(1), 'NOEU', 'DEPL_R', ier)
    endif
    call jedema()
end subroutine
