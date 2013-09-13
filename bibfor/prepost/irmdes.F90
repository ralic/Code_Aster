subroutine irmdes(idfimd, titre, nbtitr, infmed)
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     ECRITURE FORMAT MED - LA DESCRIPTION
!        -  -         -        ---
!-----------------------------------------------------------------------
!     ENTREE:
!       IDFIMD : IDENTIFIANT DU FICHIER MED
!       TITRE  : TITRE
!       INFMED : NIVEAU DES INFORMATIONS A IMPRIMER
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterc/gtoptk.h"
#include "asterfort/as_mficow.h"
#include "asterfort/enlird.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
    integer :: idfimd
    integer :: infmed, nbtitr
!
    character(len=*) :: titre(*)
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: codret
    integer :: iaux
!
    character(len=8) :: saux08
    character(len=8) :: cvers
    character(len=16) :: mach, os, proc
    character(len=24) :: ladate
    character(len=200) :: descri
!
    integer :: ltit, ideb, it, iret
!
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
!====
! 2. REPERAGE DES CONDITIONS DU CALCUL
!====
!
! 2.1. ==> LA DATE
!
    call enlird(ladate)
!
! 2.2. ==> REPERAGE DE LA VERSION UTILISEE
    cvers = ' '
    call gtoptk('versionD0', cvers, iret)
!
! 2.3. ==> REPERAGE DE LA MACHINE
!
    mach = ' '
    os = ' '
    proc = ' '
    call gtoptk('hostname', mach, iret)
    call gtoptk('system', os, iret)
    call gtoptk('processor', proc, iret)
!
!====
! 3. LA DESCRIPTION
!====
!
! 3.1. ==> CREATION
!
    descri = ' '
    descri (1:13) = 'CODE_ASTER - '
!                      1234567890123
    descri (14:21) = cvers
    descri (22:24) = ' - '
!                       234
    call gtoptk('versLabel', descri(25:60), iret)
    descri (61:79) = mach//' - '
    descri (80:98) = os//' - '
!
    descri (99:116) = ladate(1:15)//' - '
!
    ltit = 0
    ideb = 117
    do 30 it = 1, nbtitr
        iaux = lxlgut(titre(it))
        ltit = ltit + iaux + 1
        if (ltit .gt. 84) goto 32
        descri(ideb:ideb+iaux) = titre(it)(1:iaux)//'-'
        ideb = ideb + iaux + 1
        if (ideb .gt. 200) goto 32
30  end do
32  continue
!
! 3.2. ==> ECRITURE
!
    if (infmed .gt. 1) then
        call utmess('I', 'MED_38', sk=descri)
    endif
!
    call as_mficow(idfimd, descri, codret)
    if (codret .ne. 0) then
        saux08='mficow'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
!====
! 4. LA FIN
!====
!
    call jedema()
!
end subroutine
