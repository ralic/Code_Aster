subroutine mdexpm(nofimd, idfimd, nomamd, existm, ndim,&
                  codret)
!_____________________________________________________________________
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!        FORMAT MED : EXISTENCE DU PREMIER MAILLAGE DANS UN FICHIER
!               - -   --           -       -
! ______________________________________________________________________
! .        .     .        .                                            .
! .  NOM   . E/S . TAILLE .           DESCRIPTION                      .
! .____________________________________________________________________.
! . NOFIMD .  E  .   1    . NOM DU FICHIER MED                         .
! . NOFIMD .  E  .   1    . OU NUMERO DU FICHIER DEJA OUVERT           .
! . NOMAMD .  S  .   1    . NOM DU MAILLAGE MED VOULU                  .
! . EXISTM .  S  .   1    . .TRUE. OU .FALSE., SELON QUE LE MAILLAGE   .
! .        .     .        . EST PRESENT OU NON                         .
! . NDIM   .  S  .   1    . LA DIMENSION DU MAILLAGE QUAND IL EXISTE   .
! . CODRET .  S  .   1    . CODE DE RETOUR DES MODULES                 .
! ______________________________________________________________________
!
!====
! 0. DECLARATIONS ET DIMENSIONNEMENT
!====
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterfort/as_mficlo.h"
#include "asterfort/as_mfiope.h"
#include "asterfort/as_mmhmii.h"
#include "asterfort/as_mmhnmh.h"
#include "asterfort/assert.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
    character(len=*) :: nofimd, nomamd
!
    logical :: existm, ficexi, dejouv
!
    integer :: ndim, codret
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    integer :: edlect
    parameter (edlect=0)
    integer :: ednstr
    parameter (ednstr=0)
!
!
    integer :: idfimd, nbmaie
    integer :: iaux, jaux, kaux, tyaux
!
    character(len=8) :: saux08
    character(len=64) :: saux64
    character(len=200) :: daux
! ______________________________________________________________________
!
!====
! 1. ON OUVRE LE FICHIER EN LECTURE
!    ON PART DU PRINCIPE QUE SI ON N'A PAS PU OUVRIR, C'EST QUE LE
!    FICHIER N'EXISTE PAS, DONC SANS MAILLAGE A FORTIORI
!====
!
    existm = .false.
    codret = 0
    inquire(file=nofimd,exist=ficexi)
!
    if (.not. ficexi) then
!
        existm = .false.
        codret = 0
!
    else
!
        if (idfimd .eq. 0) then
            call as_mfiope(idfimd, nofimd, edlect, iaux)
            dejouv = .false.
        else
            dejouv = .true.
            iaux = 0
        endif
        if (iaux .eq. 0) then
!====
! 2. LE MAILLAGE EST-IL PRESENT ?
!====
!
! 2.1. ==> COMBIEN DE MAILLAGES DANS LE FICHIER
!
            call as_mmhnmh(idfimd, nbmaie, codret)
            if (codret .ne. 0) then
                saux08='mmhnmh'
                call utmess('F', 'DVP_97', sk=saux08, si=codret)
            endif
!
! 2.2. ==> RECHERCHE DU NOM ET DE LA DIMENSION DU PREMIER MAILLAGE
!
            if (nbmaie .eq. 0) then
!
                existm = .false.
!
            else
!
!                 12345678901234567890123456789012
                saux64 = '                                '// '                 '
                daux = ' '
                iaux = 1
                call as_mmhmii(idfimd, iaux, saux64, kaux, tyaux,&
                               daux, codret)
                if (codret .ne. 0) then
                    saux08='mmhmii'
                    call utmess('F', 'DVP_97', sk=saux08, si=codret)
                endif
                if (tyaux .ne. ednstr) then
                    call utmess('A', 'MED_81')
                endif
!
                iaux = len(nomamd)
                jaux = lxlgut(saux64)
                ASSERT(jaux.le.iaux)
!
                nomamd = ' '
                nomamd(1:jaux) = saux64(1:jaux)
                ndim = kaux
                existm = .true.
!
            endif
!
! 2.3. ==> FERMETURE DU FICHIER
!
            if (.not.dejouv) then
                call as_mficlo(idfimd, codret)
                if (codret .ne. 0) then
                    saux08='mficlo'
                    call utmess('F', 'DVP_97', sk=saux08, si=codret)
                endif
                idfimd = 0
            endif
!
        endif
    endif
!
end subroutine
