subroutine ntarch(numins, modele, mate, carele, lnonl,&
                  para, sddisc, sdcrit, sdieto, lisch2,&
                  force)
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
!
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/didern.h"
#include "asterfort/diinst.h"
#include "asterfort/dinuar.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmarce.h"
#include "asterfort/ntarc0.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/u2mess.h"
#include "asterfort/uttcpg.h"
    character(len=24) :: sdieto
    integer :: numins
    real(kind=8) :: para(*)
    logical :: lnonl, force
    character(len=19) :: sddisc, sdcrit
    character(len=24) :: modele, mate, carele
    character(len=19) :: lisch2
!
! ----------------------------------------------------------------------
!
! ROUTINE THER_* (ALGORITHME)
!
! ARCHIVAGE
!
! ----------------------------------------------------------------------
!
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: instam, instan
    integer :: iret
    integer :: numarc
    integer :: jinst
    character(len=19) :: k19bid
    character(len=16) :: k16b1, k16b2
    character(len=8) :: k8bid, result
    integer :: ibid
    character(len=24) :: k24bla
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- IMPRESSION EVENTUELLE DES MESURES DE TEMPS
!
    call uttcpg('IMPR', 'INCR')
    k24bla = ' '
!
! --- DERNIER PAS : ON ARCHIVE TOUS LES CHAMPS
!
    if (didern(sddisc,numins)) then
        force = .true.
    endif
!
! --- NUMERO D'ARCHIVAGE
!
    call dinuar(sddisc, numins, force, numarc, ibid)
!
! --- INSTANT COURANT
!
    instan = diinst(sddisc,numins)
!
! ----------------------------------------------------------------------
!
    if (numarc .ge. 0) then
!
! ----- NOM SD RESULTAT
!
        call getres(result, k16b1, k16b2)
!
! ----- INSTANT DEJA ARCHIVE ?
!
        if (numarc .ge. 2) then
            call rsadpa(result, 'L', 1, 'INST', numarc-1,&
                        0, jinst, k8bid)
            instam = zr(jinst)
            if (instan .le. instam) goto 999
        endif
!
! ----- AFFICHAGE
!
        call u2mess('I', 'ARCHIVAGE_5')
!
! ----- EXTENSION DE RESULT SI TROP PETIT (DOUBLEMENT)
!
        call rsexch(' ', result, 'TEMP', numarc, k19bid,&
                    iret)
        if (iret .eq. 110) then
            call rsagsd(result, 0)
        endif
!
! ----- ARCHIVAGE DES PARAMETRES
!
        call ntarc0(result, modele, mate, carele, sdcrit,&
                    lisch2, lnonl, para, numarc, instan)
!
! ----- ARCHIVAGE DES CHAMPS
!
        call nmarce(sdieto, result, k24bla, sddisc, instan,&
                    numarc, force)
    endif
!
999  continue
!
    call jedema()
!
end subroutine
