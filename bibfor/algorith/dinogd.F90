function dinogd(nocham)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    character(len=8) :: dinogd
    character(len=16) :: nocham
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE
!
! CONNAISSANT LE NOM DU CHAMP, ON RENVOIT LE NOM DE LA GRANDEUR
!
! ----------------------------------------------------------------------
!
!
! IN  NOCHAM : NOM DU CHAMP
! OUT DINOGD : NOM DE LA GRANDEUR
!
    if (nocham .eq. 'DEPL') dinogd = 'DEPL_R'
    if (nocham .eq. 'VITE') dinogd = 'DEPL_R'
    if (nocham .eq. 'ACCE') dinogd = 'DEPL_R'
    if (nocham .eq. 'VALE_CONT') dinogd = 'INFC_R'
    if (nocham .eq. 'SIEF_ELGA') dinogd = 'SIEF_R'
    if (nocham .eq. 'VARI_ELGA') dinogd = 'VARI_R'
    if (nocham .eq. 'TEMP') dinogd = 'TEMP_R'
    if (nocham .eq. 'FORC_NODA') dinogd = 'FORC_R'
!
end function
