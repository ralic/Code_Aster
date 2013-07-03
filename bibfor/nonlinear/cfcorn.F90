subroutine cfcorn(newgeo, numno, coorno)
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: newgeo
    integer :: numno
    real(kind=8) :: coorno(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - UTILITAIRE)
!
! COORDONNEES D'UN NOEUD
!
! ----------------------------------------------------------------------
!
!
! IN  NEWGEO : GEOMETRIE ACTUALISEE
! IN  NUMNO  : NUMERO ABSOLU DU NOEUD DANS LE MAILLAGE
! OUT COORNO : COORDONNEES DU NOEUD
!
!
!
!
    integer :: jcoor
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    coorno(1) = 0.d0
    coorno(2) = 0.d0
    coorno(3) = 0.d0
!
! --- COORDONNEES DU NOEUD
!
    call jeveuo(newgeo(1:19)//'.VALE', 'L', jcoor)
    coorno(1) = zr(jcoor+3*(numno -1))
    coorno(2) = zr(jcoor+3*(numno -1)+1)
    coorno(3) = zr(jcoor+3*(numno -1)+2)
!
    call jedema()
!
end subroutine
