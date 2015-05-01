subroutine caracm(char, nzoco, iform)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
    integer :: nzoco
    integer :: iform
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! CREATION DES SDS DE DEFINITION DU CONTACT
! SDS DEDIEES AUX METHODES MAILLEES
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  IFORM  : TYPE DE FORMULATION (DISCRET/CONTINU/XFEM)
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
!
!
!
!
    aster_logical :: lmail
    character(len=24) :: defico
    integer :: ztole, zdirn, zmeth
    character(len=24) :: methco, dirapp, toleco, dirnor
    integer :: jmeth, jdirap, jtole, jdirno
    character(len=24) :: jeufo1, jeufo2
    integer :: jjfo1, jjfo2
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    lmail = (iform.eq.1).or.(iform.eq.2)
!
! --- CREATION DES SD DE BASE
!
    methco = defico(1:16)//'.METHCO'
    dirapp = defico(1:16)//'.DIRAPP'
    dirnor = defico(1:16)//'.DIRNOR'
    jeufo1 = defico(1:16)//'.JFO1CO'
    jeufo2 = defico(1:16)//'.JFO2CO'
    toleco = defico(1:16)//'.TOLECO'
!
    zmeth = cfmmvd('ZMETH')
    zdirn = cfmmvd('ZDIRN')
    ztole = cfmmvd('ZTOLE')
!
    if (lmail) then
        call wkvect(methco, 'G V I', zmeth*nzoco, jmeth)
        call wkvect(dirapp, 'G V R', 3*nzoco, jdirap)
        call wkvect(dirnor, 'G V R', zdirn*nzoco, jdirno)
        call wkvect(jeufo1, 'G V K8', nzoco, jjfo1)
        call wkvect(jeufo2, 'G V K8', nzoco, jjfo2)
        call wkvect(toleco, 'G V R', ztole*nzoco, jtole)
    endif
!
    call jedema()
!
end subroutine
