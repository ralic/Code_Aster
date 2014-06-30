subroutine dinuar(sddisc, numins, force, numarc, numrep)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/diinst.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrpo.h"
    character(len=19) :: sddisc
    integer :: numins, numarc, numrep
    logical(kind=1) :: force
!
! ----------------------------------------------------------------------
!
! ARCHIVAGE - UTILITAIRE
!
! NUMERO D'ARCHIVAGE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  NUMINS : NUMERO INSTANT COURANT
! IN  FORCE  : VRAI SI ON SOUHAITE FORCER L'ARCHIVAGE DE TOUS LES CHAMPS
! OUT NUMARC : NUMERO D'ARCHIVAGE
! OUT NUMREP : NUMERO DE REUSE POUR LA TABLE PARA_CALC
!
! ----------------------------------------------------------------------
!
    character(len=24) :: arcinf
    integer :: jarinf
    real(kind=8) :: inst
    logical(kind=1) :: larch
    character(len=19) :: sdarch
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    larch = .false.
!
! --- ACCES SD ARCHIVAGE
!
    sdarch = sddisc(1:14)//'.ARCH'
    arcinf = sdarch(1:19)//'.AINF'
    call jeveuo(arcinf, 'E', jarinf)
!
! --- ARCHIVAGE INSTANT INITIAL FORCE
!
    if (numins .eq. 0) then
        larch = .true.
    else
        inst = diinst(sddisc,numins)
        call nmcrpo(sdarch, numins, inst, larch)
    endif
!
! --- ARCHIVAGE FORCE
!
    if (force) then
        larch = .true.
    endif
!
! --- NUMERO D'ARCHIVAGE
!
    if (larch) then
        numarc = zi(jarinf-1+1)
    else
        numarc = - 1
    endif
!
! --- NOUVEAU NUMERO D'ARCHIVAGE (POUR LE PROCHAIN APPEL)
!
    if (larch) then
        zi(jarinf-1+1) = zi(jarinf-1+1) + 1
    endif
!
! --- NUMERO DE REUSE POUR LA TABLE PARA_CALC
!
    numrep = zi(jarinf-1+3)
!
    call jedema()
!
end subroutine
