subroutine infmaj()
    implicit none
!-----------------------------------------------------------------------
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
!      MOT CLE INFO
!-----------------------------------------------------------------------
!      UTILITAIRE DE MISE A JOUR POUR LE MOT CLE INFO
!-----------------------------------------------------------------------
!      ---PAS D'ARGUMENTS
!-----------------------------------------------------------------------
!
!------DEBUT DU COMMON INF001-------------------------------------------
!      NIVUTI    :NIVEAU DEMANDE PAR L'UTILISATEUR  : 1 OU 2
!      NIVPGM    :NIVEAU ACCESSIBLE AU PROGRAMMEUR  : 0 , 1 OU 2
!      UNITE     :UNITE LOGIQUE DU FICHIER MESSAGE
!
#include "asterc/getexm.h"
#include "asterfort/getvis.h"
#include "asterfort/iunifi.h"
    integer :: nivuti, nivpgm, unite
    common / inf001 / nivuti , nivpgm , unite
!-----FIN DE INF001-----------------------------------------------------
!
    integer :: info, nbval
    integer :: linfo
!
    info = 1
!
    linfo = getexm ( ' ' , 'INFO' )
!
    if (linfo .eq. 1) then
        call getvis(' ', 'INFO', scal=info, nbret=nbval)
    endif
!
    nivuti = info
    nivpgm = info
    unite = iunifi('MESSAGE')
!
end subroutine
