subroutine cfsans(defico, npt, jeux, enti, zone)
!
implicit none
!
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterc/r8prem.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfr.h"
#include "asterfort/utmess.h"
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
    character(len=24) :: defico
    integer :: npt
    character(len=24) :: jeux, enti, zone
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - VERIF)
!
! AFFICHAGE DES INTERPENETRATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  NPT    : NOMBRE DE POINTS EN MODE VERIF
! IN  JEUX   : NOM DE LA SD STOCKANT LE JEU
! IN  ENTI   : NOM DE LA SD STOCKANT LES NOMS DES ENTITES APPARIEES
! IN  ZONE   : NOM DE LA SD STOCKANT LA ZONE A LAQUELLE APPARTIENT LE
!              POINT
!
!
!
!
    character(len=16) :: noment, nompt
    integer :: interp
    logical(kind=1) :: lstop
    real(kind=8) :: jeu, jeuref, varc
    integer :: ipt, izone
    integer :: jjeux, jenti, jzone
!
! ----------------------------------------------------------------------
!
    interp = 0
!
! - Access to data
!
    call jeveuo(jeux, 'L', jjeux)
    call jeveuo(zone, 'L', jzone)
    call jeveuo(enti, 'L', jenti)
!
! - Alarm or error ?
!
    lstop = cfdisl(defico,'STOP_INTERP')
!
    do ipt = 1, npt
!
! ----- Information about contact point
!
        jeu = zr(jjeux+ipt-1)
        izone = zi(jzone+ipt-1)
        nompt = zk16(jenti+2*(ipt-1)+1-1)
        noment = zk16(jenti+2*(ipt-1)+2-1)
!
! ----- Parameters
!
        jeuref = mminfr(defico,'TOLE_INTERP',izone)
!
! ----- Test
!
        varc = 0.d0 
        if (jeu.ne.r8vide()) then
            if (jeu.gt.r8prem()) then
                varc = 0.d0 
            else
                if (abs(jeu).le.jeuref) then
                    varc = 0.d0           
                else
                    varc = 3.d0 
                    interp = interp+1     
                endif
            endif
        endif
    end do
!
! - Print
!
    if (interp .ge. 1) then
        if (lstop) then
            call utmess('F', 'CONTACT_93', si = interp)
        else
            call utmess('A', 'CONTACT_93', si = interp)
        endif
    endif
!
end subroutine
