subroutine apcopt(sdappa, ip, coorpt)
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
    character(len=19) :: sdappa
    integer :: ip
    real(kind=8) :: coorpt(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! COORDONNEES DU POINT
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  IP     : NUMERO DU POINT
! OUT COORPT : COORDONNEES DU POINT
!
!
!
!
    character(len=24) :: appoin
    integer :: jpoin
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SDAPPA
!
    appoin = sdappa(1:19)//'.POIN'
    call jeveuo(appoin, 'L', jpoin)
!
! --- COORDONNEES DU NOEUD
!
    coorpt(1) = zr(jpoin+3*(ip-1)+1-1)
    coorpt(2) = zr(jpoin+3*(ip-1)+2-1)
    coorpt(3) = zr(jpoin+3*(ip-1)+3-1)
!
    call jedema()
!
end subroutine
