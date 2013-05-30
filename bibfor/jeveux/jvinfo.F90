function jvinfo(kactio, info)
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
    implicit none
    integer :: jvinfo
    character(len=*) :: kactio
    integer :: info
! ----------------------------------------------------------------------
! DEFINITION DU NIVEAU DES IMPRESSIONS JEVEUX
!
! IN  KACTIO  = AFFECT AFFECTATION DU NIVEAU DES IMPRESSIONS
!             = INIT   MISE A 0
!             = RECUP  RECUPERATION DU NIVEAU DES IMPRESSIONS
! IN  INFO   VALEUR DU NIVEAU DES IMPRESSIONS (AFFECT UNIQUEMENT)
! ----------------------------------------------------------------------
    integer :: ifnivo, nivo
    common /jvnivo/  ifnivo, nivo
! DEB ------------------------------------------------------------------
    character(len=8) :: k8ch
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    k8ch=kactio
    if (ifnivo .ne. 22021986) then
        nivo = 0
        ifnivo = 22021986
    endif
    if (k8ch(1:6) .eq. 'AFFECT') then
        nivo = info
    else if (k8ch(1:4) .eq. 'INIT') then
        nivo = 0
    else if (k8ch(1:5) .eq. 'RECUP') then
    endif
    jvinfo = nivo
! FIN ------------------------------------------------------------------
end function
