subroutine nxnoli(modele, mate, carele, lostat, lreuse,&
                  lnonl, levol, para, sddisc, sdcrit,&
                  sdieto, lisch2)
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
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/gnomsd.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ntarch.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsrusd.h"
#include "asterfort/u2mess.h"
    real(kind=8) :: para(*)
    logical :: lnonl, lostat, lreuse, levol
    character(len=19) :: sddisc, sdcrit
    character(len=24) :: modele, mate, carele, sdieto
    character(len=19) :: lisch2
!
! ----------------------------------------------------------------------
!
! ROUTINE THER_* (SD EVOL_THER)
!
! PREPARATION DE LA SD EVOL_THER
!
! ----------------------------------------------------------------------
!
!
!
!
!
!
    character(len=24) :: arcinf
    integer :: jarinf
    character(len=16) :: k16b1, k16b2
    character(len=19) :: sdarch
    integer :: numarc, numins
    integer :: ifm, niv
    character(len=24) :: noobj, result
    logical :: force
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<THERMIQUE> PREPARATION DE LA SD EVOL_THER'
    endif
!
! --- INSTANT INITIAL
!
    numins = 0
    force = .true.
!
! --- DETERMINATION DU NOM DE LA SD INFO_CHARGE STOCKEE
! --- DANS LA SD RESULTAT
!
    noobj = '12345678.1234.EXCIT'
    call gnomsd(' ', noobj, 10, 13)
    lisch2 = noobj(1:19)
!
! --- ACCES SD ARCHIVAGE
!
    sdarch = sddisc(1:14)//'.ARCH'
    arcinf = sdarch(1:19)//'.AINF'
!
! --- NUMERO ARCHIVAGE COURANT
!
    call jeveuo(arcinf, 'L', jarinf)
    numarc = zi(jarinf+1 -1)
!
! --- CREATION DE LA SD EVOL_THER OU NETTOYAGE DES ANCIENS NUMEROS
!
    call getres(result, k16b1, k16b2)
    if (lreuse) then
        ASSERT(numarc.ne.0)
        call rsrusd(result, numarc)
    else
        ASSERT(numarc.eq.0)
        call rscrsd('G', result, 'EVOL_THER', 100)
    endif
!
! --- ARCHIVAGE ETAT INITIAL
!
    if ((.not.lreuse) .and. (.not.lostat) .and. levol) then
        call u2mess('I', 'ARCHIVAGE_4')
        call ntarch(numins, modele, mate, carele, lnonl,&
                    para, sddisc, sdcrit, sdieto, lisch2,&
                    force)
    endif
!
    call jedema()
!
end subroutine
