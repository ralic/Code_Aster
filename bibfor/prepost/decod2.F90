subroutine decod2(rec, irec, ifield, itype, ilu,&
                  rlu, trouve)
    implicit  none
#include "asterc/ismaem.h"
#include "asterc/r8vide.h"
#include "asterfort/lxliis.h"
#include "asterfort/lxlir8.h"
#include "asterfort/trfmot.h"
    character(len=*) :: rec(20)
    integer :: irec, ifield, itype, ilu
    real(kind=8) :: rlu
    logical(kind=1) :: trouve
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!----------------------------------------------------------------------
!
!    EXTRACTION DE L'ENREGISTREMENT IREC ET DU CHAMP IFIELD
!               DU NUMERO D'ORDRE OU DE L'INSTANT OU DE LA
!               FREQUENCE
!
! IN  : REC    : K80  : TABLEAU DE CARACTERES CONTENANT L'ENTETE DU
!                       DATASET
! IN  : IREC   : I    : NUMERO DE L'ENREGISTREMENT A TRAITER
! IN  : IFIELD : I    : NUMERO DU CHAMP A TRAITER
! IN  : ITYPE  : I    : TYPE DE VALEUR LUE (0:ENTIER, 1:REELLE)
! OUT : ILU    : I    : VALEUR ENTIERE LUE (NUMERO D'ORDRE)
! OUT : RLU    : I    : VALEUR REELLE LUE (INSTANT OU FREQUENCE)
! OUT : TROUVE : L    : .TRUE.  ON A TROUVE LA VALEUR ATTENDUE
!                       .FALSE. ON N A PAS TROUVE LA VALEUR ATTENDUE
!
!---------------------------------------------------------------------
    character(len=80) :: field
    integer :: ier
!
!- INITIALISATION
    trouve = .true.
    ilu=ismaem()
    rlu=r8vide()
!
!- SI IREC OU IFIELD INVALIDE : ON RETOURNE DES VALEURS VIDES :
    if ((irec.le.0) .or. (ifield.le.0)) then
        trouve=.false.
        goto 9999
    endif
!
!- RECHERCHE DU CHAMP A TRAITER
!
    call trfmot(rec(irec), field, ifield)
!
    if (itype .eq. 0) then
!
!- DECODAGE D'UN ENTIER ET VERIFICATION
!
        call lxliis(field, ilu, ier)
        if (ier .eq. 1) trouve = .false.
!
    else
!
!- DECODAGE D'UN REEL ET VERIFICATION
!
        call lxlir8(field, rlu, ier)
        if (ier .eq. 1) trouve = .false.
!
    endif
9999  continue
end subroutine
