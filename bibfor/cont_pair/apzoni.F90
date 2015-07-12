subroutine apzoni(sdappa, i_zone, questi_, vali)
!
implicit none
!
#include "asterfort/apmmvd.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: sdappa
    character(len=*), intent(in) :: questi_
    integer, intent(in) :: i_zone
    integer, intent(out) :: vali
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Ask datastructure - By zone - Integer
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  questi           : question
!      NBPT             NBRE DE POINTS DE LA ZONE
!      NBNOM            NBRE NOEUDS ATTACHES A LA SURFACE MAITRE
!      NBMAM            NBRE MAILLES ATTACHEES A LA SURFACE MAITRE
!      NBNOE            NBRE NOEUDS ATTACHES A LA SURFACE ESCLAVE
!      NBMAE            NBRE MAILLES ATTACHEES A LA SURFACE ESCLAVE
!      JDECNM           DECALAGE DANS LES VECTEURS POUR LE PREMIER
!                       NOEUD DE LA SURFACE MAITRE
!      JDECMM           DECALAGE DANS LES VECTEURS POUR LA PREMIERE
!                       MAILLE DE LA SURFACE MAITRE
!      JDECNE           DECALAGE DANS LES VECTEURS POUR LE PREMIER
!                       NOEUD DE LA SURFACE ESCLAVE
!      JDECME           DECALAGE DANS LES VECTEURS POUR LA PREMIERE
!                        MAILLE DE LA SURFACE ESCLAVE
!      JDECIV           DECALAGE POUR TABLEAU DES CONNECTIVITES
!                       INVERSES
!      DIRE_APPA        1 SI APPARIEMENT DE DIRECTION FIXE
!                       0 SINON
!      TYPE_APPA        0 SI APPARIEMENT NODAL
!                       1 SI APPARIEMENT MAITRE-ESCLAVE
!      TYPE_NORM_MAIT   ORIENTATION BASE LOCALE MAIT.
!                       0 SI AUTO
!                       1 SI FIXE
!                       2 SI VECT_Y
!      TYPE_NORM_ESCL   ORIENTATION BASE LOCALE ESCL.
!                       0 SI AUTO
!                       1 SI FIXE
!                       2 SI VECT_Y
!      CALC_NORM_ESCL   1 SI CALCUL DE LA NORMALE SUR NOEUD ESCLAVE
!                       0 SINON
!      CALC_NORM_MAIT   1 SI CALCUL DE LA NORMALE SUR NOEUD MAITRE
!                       0 SINON
! In  i_zone           : index of contact zone
! Out vali             : answer
!
! --------------------------------------------------------------------------------------------------
!
    integer :: zinzi
    character(len=24) :: sdappa_inzi
    integer, pointer :: v_sdappa_inzi(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    vali  = 0
    zinzi = apmmvd('ZINZI')
    sdappa_inzi = sdappa(1:19)//'.INZI'
    call jeveuo(sdappa_inzi, 'L', vi = v_sdappa_inzi)
!
    if (questi_ .eq. 'NBPT') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+1)
    else if (questi_.eq.'NBNOM') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+2)
    else if (questi_.eq.'NBNOE') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+3)
    else if (questi_.eq.'NBMAM') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+4)
    else if (questi_.eq.'NBMAE') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+5 )
    else if (questi_.eq.'JDECNM') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+6 )
    else if (questi_.eq.'JDECMM') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+7 )
    else if (questi_.eq.'JDECNE') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+8 )
    else if (questi_.eq.'JDECME') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+9 )
    else if (questi_.eq.'DIRE_APPA') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+10)
    else if (questi_.eq.'TYPE_APPA') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+11)
    else if (questi_.eq.'TYPE_NORM_MAIT') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+12)
    else if (questi_.eq.'TYPE_NORM_ESCL') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+13)
    else if (questi_.eq.'CALC_NORM_ESCL') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+14)
    else if (questi_.eq.'CALC_NORM_MAIT') then
        vali = v_sdappa_inzi(zinzi*(i_zone-1)+15)
    else
        ASSERT(.false.)
    endif
!
end subroutine
