subroutine projcy(nomres)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!    P. RICHARD     DATE 11/03/91
!-----------------------------------------------------------------------
!  BUT:  CALCULER LES SOUS-MATRICES OBTENUES A PARTIR DES PROJECTIONS
    implicit none
!     DES MATRICES MASSE ET RAIDEUR SUR LES MODES ET LES DEFORMEES
!    STATIQUECS
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UTILISATEUR DU RESULTAT
!
!
!
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/prcycb.h"
#include "asterfort/prcymn.h"
#include "asterfort/utmess.h"
    character(len=8) :: nomres, typint
    character(len=24) :: repmat, soumat
    character(len=24) :: valk
    logical(kind=1) :: nook
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer ::  llref
    character(len=8), pointer :: cycl_type(:) => null()
!-----------------------------------------------------------------------
    data nook /.true./
!-----------------------------------------------------------------------
!
!--------------------RECUPERATION DES CONCEPTS AMONT--------------------
!
    call jemarq()
    call jeveuo(nomres//'.CYCL_REFE', 'L', llref)
!
!-------------CAS DE LA DONNEE D'UNE BASE MODALE------------------------
!
    soumat='&&OP0080.CYCLIC.SOUS.MAT'
    repmat='&&OP0080.CYCLIC.REPE.MAT'
!
!--------------RECUPERATION DU TYPE D'INTERFACE-------------------------
!
    call jeveuo(nomres//'.CYCL_TYPE', 'L', vk8=cycl_type)
    typint=cycl_type(1)
!
!----------------CALCUL SOUS-MATRICES DANS LE CAS CRAIG-BAMPTON---------
!                        ET CRAIG-BAMPTON HARMONIQUE
!
    if (typint .eq. 'CRAIGB  ' .or. typint .eq. 'CB_HARMO') then
        call prcycb(nomres, soumat, repmat)
        nook=.false.
    endif
!
!----------------CALCUL SOUS-MATRICES DANS LE CAS MAC NEAL--------------
!
    if (typint .eq. 'MNEAL  ') then
        call prcymn(nomres, soumat, repmat)
        nook=.false.
    endif
!
!----------------CALCUL SOUS-MATRICES DANS LE CAS AUCUN-----------------
!        (=MAC NEAL SANS FLEXIBILITE RESIDUELLE)
!
    if (typint .eq. 'AUCUN   ') then
        call prcymn(nomres, soumat, repmat)
        nook=.false.
    endif
!
!--------------AUTRE CAS -----------------------------------------------
!
    if (nook) then
        valk = typint
        call utmess('F', 'ALGORITH14_3', sk=valk)
    endif
!
    call jedema()
end subroutine
