subroutine ntcrar(result, sddisc, lreuse)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmarex.h"
#include "asterfort/nmarnr.h"
#include "asterfort/nmarpr.h"
#include "asterfort/nmcrpx.h"
#include "asterfort/nmdide.h"
#include "asterfort/wkvect.h"
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
!
    character(len=19) :: sddisc
    character(len=8) :: result
    aster_logical :: lreuse
!
! ----------------------------------------------------------------------
!
! ROUTINE THER_* (STRUCTURES DE DONNES)
!
! CREATION SD ARCHIVAGE
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM DE LA SD RESULTAT
! IN  LREUSE : .TRUE. SI CONCEPT REENTRANT
! IN  SDDISC : SD DISCRETISATION
!
! ----------------------------------------------------------------------
!
    integer :: nocc, iocc
    integer :: numder, numrep, numreo
    character(len=16) :: motfac, motpas
    integer :: numarc
    character(len=24) :: arcinf
    integer :: jarinf
    character(len=19) :: sdarch
    integer :: ifm, niv
    character(len=1) :: base
    real(kind=8) :: insder
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<THERNONLINE> ... CREATION SD ARCHIVAGE'
    endif
!
! --- INITIALISATIONS
!
    motfac = 'ARCHIVAGE'
    motpas = 'PAS_ARCH'
    base = 'V'
    iocc = 1
    numarc = -1
    numreo = -1
    numrep = -1
    call getfac(motfac, nocc)
    ASSERT(nocc.le.1)
!
! --- NOM SD ARCHIVAGE
!
    sdarch = sddisc(1:14)//'.ARCH'
    arcinf = sdarch(1:19)//'.AINF'
!
! --- DERNIER NUMERO ARCHIVE DANS L'EVOL SI REUSE
!
    call nmdide(lreuse, result, numder, insder)
!
! --- LECTURE LISTE INSTANTS D'ARCHIVAGE
!
    call nmcrpx(motfac, motpas, iocc, sdarch, base)
!
! --- CONSTRUCTION CHAMPS EXCLUS DE L'ARCHIVAGE
!
    call nmarex(motfac, sdarch)
!
! --- RECUPERATION DU PREMIER NUMERO A ARCHIVER
!
    call nmarpr(result, sddisc, lreuse, numder, insder,&
                numarc)
!
! --- RECUPERATION NUMERO REUSE - TABLE OBSERVATION
!
    call nmarnr(result, 'OBSERVATION', numreo)
!
! --- NUMERO D'ARCHIVE COURANT ET NUMERO DE REUSE
!
    ASSERT(numarc.ge.0)
    ASSERT(numreo.ge.0)
    call wkvect(arcinf, 'V V I', 3, jarinf)
    zi(jarinf-1+1) = numarc
    zi(jarinf-1+2) = numreo
    zi(jarinf-1+3) = numrep
!
    call jedema()
!
end subroutine
