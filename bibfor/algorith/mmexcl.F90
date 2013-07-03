subroutine mmexcl(resoco, typint, iptc, iptm, ndexfr,&
                  typapp, lexfro)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    integer :: typint
    integer :: ndexfr
    integer :: iptc, iptm
    integer :: typapp
    logical :: lexfro
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
!
! REMPLIT LA SD APPARIEMENT POUR LES CAS D'EXCLUSION
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  TYPINT : TYPE D'INTEGRATION
! IN  TYPAPP : TYPE D'APPARIEMENT
!                -1 EXCLU - SANS_NOEUD
!                -2 EXCLU - TOLE_APPA
!                -3 EXCLU - TOLE_PROJ_EXT
!                 1 APPARIEMENT MAIT/ESCL
!                 2 APPARIEMENT NODAL
! IN  IPTM   : NUMERO DU POINT D'INTEGRATION DANS LA MAILLE
! IN  IPTC   : NUMERO DE LA LIAISON DE CONTACT
! IN  NDEXFR : ENTIER CODE POUR LES NOEUDS INTERDITS DANS
!              SANS_GROUP_NO_FR OU SANS_NOEUD_FR
! OUT LEXFRO : .TRUE. SI LE POINT D'INTEGRATION DOIT ETRE EXCLUS DU
!                     FROTTEMENT
!
! ----------------------------------------------------------------------
!
    integer :: ztabf
    character(len=24) :: tabfin
    integer :: jtabf
    integer :: lnexfr(9)
    logical :: prtole, projin
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONTACT
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'E', jtabf)
!
    ztabf = cfmmvd('ZTABF')
!
! --- INITIALISATIONS
!
    prtole = .true.
    projin = .true.
!
! --- TRAITEMENT DES NOEUDS EXCLUS
!
    if (typapp .eq. -2) then
        prtole = .false.
    else if (typapp.eq.-3) then
        projin = .false.
    endif
!
! --- PAS DE NOEUDS EXCLU
!
    zr(jtabf+ztabf*(iptc-1)+18) = 0.d0
!
! --- NOEUD EXCLUS PAR SANS_GROUP_NO (TYPAPP=-1)
!
    if (typapp .eq. -1) then
        call assert(typint.eq.1)
        zr(jtabf+ztabf*(iptc-1)+18) = 1.d0
    endif
!
! --- POINTS DE CONTACT EXCLUS PAR TOLE_APPA
!
    if (.not.prtole) then
        zr(jtabf+ztabf*(iptc-1)+18) = 1.d0
    endif
!
! --- POINTS DE CONTACT EXCLUS PAR PROJECTION HORS ZONE
!
    if (.not. projin) then
        zr(jtabf+ztabf*(iptc-1)+18) = 1.d0
    endif
!
! --- NOEUDS EXCLUS PAR SANS_GROUP_NO_FR
!
    lexfro = .false.
    zr(jtabf+ztabf*(iptc-1)+19) = 0.d0
!     -- SEULS LES 9 PREMIERS NOEUDS EXISTENT DANS LA MAILLE
    if (iptm .le. 9) then
        call isdeco(ndexfr, lnexfr, 9)
        if (lnexfr(iptm) .eq. 1) then
            lexfro = .true.
            zr(jtabf+ztabf*(iptc-1)+19) = ndexfr
        endif
    endif
!
    call jedema()
!
end subroutine
