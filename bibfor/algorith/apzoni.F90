subroutine apzoni(sdappa, izone, questz, vali)
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
#include "asterfort/apmmvd.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: sdappa
    integer :: izone, vali
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! INFO. DE TYPE ENTIER SUR LA ZONE COURANTE
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  IZONE  : NUMERO DE LA ZONE
! IN  QUESTI : QUESTION
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
! OUT VALI   : REPONSE A LA QUESTION
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: apinzi
    integer :: jpinzi
    character(len=24) :: questi
    integer :: zinzi
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- ACCES SDAPPA
!
    apinzi = sdappa(1:19)//'.INZI'
    call jeveuo(apinzi, 'L', jpinzi)
!
! --- INITIALISATIONS
!
    vali = 0
    questi = questz
    zinzi = apmmvd('ZINZI')
!
! --- REPONSE
!
    if (questi .eq. 'NBPT') then
        vali = zi(jpinzi+zinzi*(izone-1)+1 -1)
    else if (questi.eq.'NBNOM') then
        vali = zi(jpinzi+zinzi*(izone-1)+2 -1)
    else if (questi.eq.'NBNOE') then
        vali = zi(jpinzi+zinzi*(izone-1)+3 -1)
    else if (questi.eq.'NBMAM') then
        vali = zi(jpinzi+zinzi*(izone-1)+4 -1)
    else if (questi.eq.'NBMAE') then
        vali = zi(jpinzi+zinzi*(izone-1)+5 -1)
    else if (questi.eq.'JDECNM') then
        vali = zi(jpinzi+zinzi*(izone-1)+6 -1)
    else if (questi.eq.'JDECMM') then
        vali = zi(jpinzi+zinzi*(izone-1)+7 -1)
    else if (questi.eq.'JDECNE') then
        vali = zi(jpinzi+zinzi*(izone-1)+8 -1)
    else if (questi.eq.'JDECME') then
        vali = zi(jpinzi+zinzi*(izone-1)+9 -1)
!
    else if (questi.eq.'DIRE_APPA') then
        vali = zi(jpinzi+zinzi*(izone-1)+10-1)
    else if (questi.eq.'TYPE_APPA') then
        vali = zi(jpinzi+zinzi*(izone-1)+11-1)
!
    else if (questi.eq.'TYPE_NORM_MAIT') then
        vali = zi(jpinzi+zinzi*(izone-1)+12-1)
    else if (questi.eq.'TYPE_NORM_ESCL') then
        vali = zi(jpinzi+zinzi*(izone-1)+13-1)
!
    else if (questi.eq.'CALC_NORM_ESCL') then
        vali = zi(jpinzi+zinzi*(izone-1)+14-1)
    else if (questi.eq.'CALC_NORM_MAIT') then
        vali = zi(jpinzi+zinzi*(izone-1)+15-1)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine
