subroutine infniv(ifm, niv)
    implicit none
    integer :: ifm, niv
!-----------------------------------------------------------------------
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
!       MOT CLE INFO
!-----------------------------------------------------------------------
!      UTILITAIRE DE MISE A NIVEAU POUR LE MOT CLE INFO
!-----------------------------------------------------------------------
!      IN     : PAS D'ARGUMENTS D ENTREE
!      OUT    :IFM    :INTEGER   :UNITE LOGIQUE DU FICHIER MESSAGE
!      OUT    :NIV    :INTEGER   :NIVEAU DU PROGRAMMEUR
!-----------------------------------------------------------------------
!
!------DEBUT DU COMMON INF001-------------------------------------------
!      NIVUTI    :NIVEAU DEMANDE PAR L'UTILISATEUR  : 1 OU 2
!      NIVPGM    :NIVEAU ACCESSIBLE AU PROGRAMMEUR  : 0 , 1 OU 2
!      UNITE     :UNITE LOGIQUE DU FICHIER MESSAGE
!
    integer :: nivuti, nivpgm, unite
    common/inf001/nivuti,nivpgm,unite
!-----FIN DE INF001-----------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    ifm = unite
    niv = nivpgm
end subroutine
