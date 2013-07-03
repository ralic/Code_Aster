subroutine nmdpmf(compor)
    implicit none
#include "asterc/getvid.h"
#include "asterfort/carces.h"
#include "asterfort/cescar.h"
#include "asterfort/cesfus.h"
#include "asterfort/cesred.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mess.h"
    character(len=19) :: compor
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-luc.flejou at edf.fr
!
!     POUR LES MULTIFIBRES, FUSION AVEC LA CARTE CREEE DANS
!        AFFE_MATERIAU / AFFE_COMPOR AVEC RCCOMP.F
!
! IN/OUT  COMPOR    carte de comportement
! ----------------------------------------------------------------------
#include "jeveux.h"
    integer :: n1, ibid, iarg, iret
    logical :: lcumu(2), lcoc(2)
    character(len=6) :: nompro
    character(len=8) :: chmat, licmp
    character(len=19) :: chs(2), chs3, chsx
    real(kind=8) :: lcoer(2)
    complex(kind=8) :: lcoec(2)
!
    parameter (nompro='NMDPMF')
! ----------------------------------------------------------------------
    data lcumu/.false.,.false./
    data lcoc/.false.,.false./
    data lcoer/1.d0,1.d0/
! ----------------------------------------------------------------------
    call jemarq()
!
!     EN PRESENCE DE MULTIFIBRE, ON FUSIONNE LES CARTES
!     S'IL EXISTE UNE POUTRE MULTIFIBRE LA CARTE A ETE CREEE LORS
!        DE AFFE_MATERIAU / AFFE_COMPOR AVEC RCCOMP
!
!     CHAM_ELEM_S DE TRAVAIL
    chs(1)='&&'//nompro//'.CHS1'
    chs(2)='&&'//nompro//'.CHS2'
    chs3  ='&&'//nompro//'.CHS3'
    chsx  ='&&'//nompro//'.CHSX'
!
!     ON RECUPERE LA CARTE COMPOR ==> CHAM_ELEM_S
    call carces(compor, 'ELEM', ' ', 'V', chs(1),&
                'A', ibid)
!
!     ON RECUPERE LA CARTE COMPOR DE AFFE_COMPOR ==> CHAM_ELEM_S
    call getvid(' ', 'CHAM_MATER', 1, iarg, 1,&
                chmat, n1)
!
!     VERIFICATION QU'UN COMPORTEMENT MULTI-FIBRES A ETE AFFECTE
    call exisd('CARTE', chmat//'.COMPOR', iret)
    if (iret .eq. 0) call u2mess('F', 'COMPOR1_73')
!
    call carces(chmat//'.COMPOR', 'ELEM', ' ', 'V', chsx,&
                'A', ibid)
!     ON ENLEVE LA COMPOSANTE 'DEFORM' DE LA CARTE
    licmp = 'DEFORM'
    call cesred(chsx, 0, ibid, -1, licmp,&
                'V', chs(2))
!
!     FUSION DES CHAM_ELEM_S + COPIE DANS "COMPOR"
    call detrsd('CARTE', compor)
    call cesfus(2, chs, lcumu, lcoer, lcoec,&
                lcoc, 'V', chs3)
    call cescar(chs3, compor, 'V')
!
!     MENAGE
    call detrsd('CHAM_ELEM_S', chs(1))
    call detrsd('CHAM_ELEM_S', chs(2))
    call detrsd('CHAM_ELEM_S', chs3)
    call detrsd('CHAM_ELEM_S', chsx)
!
    call jedema()
end subroutine
