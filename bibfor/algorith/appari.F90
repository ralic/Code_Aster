subroutine appari(sdappa, questz, vali)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: sdappa
    integer :: vali
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! CONSTANTES - PARAMETRE ENTIER
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  QUESTI : QUESTION
!               APPARI_NBZONE      NBRE DE ZONES
!               APPARI_NDIMG       DIMENSION DE L'ESPACE
!               APPARI_NTPT        NBRE DE POINTS
!               PROJ_NEWT_ITER     NBRE ITERATION POUR NEWTON PROJECTION
! OUT VALI   : REPONSE A LA QUESTION
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: apinfi
    integer :: jpinfi
    character(len=24) :: questi
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- ACCES SDAPPA
!
    apinfi = sdappa(1:19)//'.INFI'
    call jeveuo(apinfi, 'L', jpinfi)
!
! --- INITIALISATIONS
!
    vali = 0
    questi = questz
!
! --- QUESTION
!
    if (questi .eq. 'APPARI_NBZONE') then
        vali = zi(jpinfi+1-1)
    else if (questi.eq.'APPARI_NTPT') then
        vali = zi(jpinfi+2-1)
    else if (questi.eq.'APPARI_NTMA') then
        vali = zi(jpinfi+3-1)
    else if (questi.eq.'PROJ_NEWT_ITER') then
        vali = zi(jpinfi+4-1)
    else if (questi.eq.'APPARI_NDIMG') then
        vali = zi(jpinfi+5-1)
    else if (questi.eq.'APPARI_NTNO') then
        vali = zi(jpinfi+6-1)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine
