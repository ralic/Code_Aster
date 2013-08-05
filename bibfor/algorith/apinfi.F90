subroutine apinfi(sdappa, questz, ip, vali)
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
    integer :: ip
    integer :: vali
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! INTERROGATION DE LA SDAPPA - ENTIER
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  QUESTI : QUESTION
!              'APPARI_TYPE'     TYPE D'APPARIEMENT (ND, MAILLE, ETC)
!              'APPARI_ENTITE'   NUMERO D'ENTITE APPARIEE
!              'APPARI_ZONE'     ZONE SUPPORT DU POINT
!              'APPARI_MAILLE'   MAILLE SUPPORT DU POINT
!              'APPARI_NOEUD'    NOEUD SUPPORT DU POINT
! IN  IP     : INDICE DU POINT
! OUT VALI   : REPONSE A LA QUESTION
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: appar
    integer :: jappa
    character(len=16) :: questi
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- ACCES SDAPPA
!
    appar = sdappa(1:19)//'.APPA'
!
! --- INITIALISATIONS
!
    vali = 0
    questi = questz
!
! --- QUESTION
!
    if (questi .eq. 'APPARI_TYPE') then
        call jeveuo(appar, 'L', jappa)
        vali = zi(jappa+4*(ip-1)+1-1)
    else if (questi.eq.'APPARI_ENTITE') then
        call jeveuo(appar, 'L', jappa)
        vali = zi(jappa+4*(ip-1)+2-1)
    else if (questi.eq.'APPARI_ZONE') then
        call jeveuo(appar, 'L', jappa)
        vali = zi(jappa+4*(ip-1)+3-1)
    else if (questi.eq.'APPARI_MAILLE') then
        call jeveuo(appar, 'L', jappa)
        vali = zi(jappa+4*(ip-1)+4-1)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine
