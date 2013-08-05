subroutine mmbouc(resoco, nombcl, typope, valbcl)
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
! REPONSABLE
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    character(len=4) :: nombcl
    character(len=4) :: typope
    integer :: valbcl
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! GESTION DES BOUCLES
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD POUR LA RESOLUTION DU CONTACT
! IN  NOMBCL : NOM DE LA BOUCLE
!               CONT - CONTRAINTES ACTIVES
!               FROT - SEUILS DE FROTTEMENT
!               GEOM - GEOMETRIE
! IN  TYPOPE : TYPE DE L'OPERATION
!               READ - LECTURE
!               INIT - INITIALISATION A ZERO
!               INCR - INCREMENTATION
! OUT VALBCL : VALEUR DE LA BOUCLE
!
!
!
!
    character(len=24) :: mboucl
    integer :: jmbouc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    mboucl = resoco(1:14)//'.MBOUCL'
    call jeveuo(mboucl, 'E', jmbouc)
!
    if (nombcl .eq. 'CONT') then
        if (typope .eq. 'INIT') then
            zi(jmbouc-1+1) = 0
        else if (typope.eq.'INCR') then
            zi(jmbouc-1+1) = zi(jmbouc-1+1) +1
        else if (typope.eq.'READ') then
            valbcl = zi(jmbouc-1+1)
        else
            ASSERT(.false.)
        endif
        valbcl = zi(jmbouc-1+1)
    else if (nombcl.eq.'FROT') then
        if (typope .eq. 'INIT') then
            zi(jmbouc-1+2) = 0
        else if (typope.eq.'INCR') then
            zi(jmbouc-1+2) = zi(jmbouc-1+2) +1
        else if (typope.eq.'READ') then
            valbcl = zi(jmbouc-1+2)
        else
            ASSERT(.false.)
        endif
        valbcl = zi(jmbouc-1+2)
    else if (nombcl.eq.'GEOM') then
        if (typope .eq. 'INIT') then
            zi(jmbouc-1+3) = 0
        else if (typope.eq.'INCR') then
            zi(jmbouc-1+3) = zi(jmbouc-1+3) +1
        else if (typope.eq.'READ') then
            valbcl = zi(jmbouc-1+3)
        else
            ASSERT(.false.)
        endif
        valbcl = zi(jmbouc-1+3)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
