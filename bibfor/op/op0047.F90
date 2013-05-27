subroutine op0047()
    implicit   none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   - FONCTIONS REALISEES:
!       COMMANDE PRE_IDEAS
!       COMMANDE PRE_GMSH
!       INTERFACE ENTRE MAILLAGE IDEAS ET FICHIER MAILLAGE ASTER
!   - OUT :
!       IERR   : NON UTILISE
!     ------------------------------------------------------------
    include 'asterc/getres.h'
    include 'asterc/getvis.h'
    include 'asterc/getvtx.h'
    include 'asterfort/gmsast.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/stbast.h'
    integer :: nfie, nfis, n
    logical :: lgrcou
    character(len=8) :: k8b
    character(len=16) :: k16b, cmd
    integer :: iarg
!
    call infmaj()
    call getres(k8b, k16b, cmd)
!
    if (cmd(5:9) .eq. 'IDEAS') then
        lgrcou = .false.
        call getvis(' ', 'UNITE_IDEAS', 1, iarg, 1,&
                    nfie, n)
        call getvtx(' ', 'CREA_GROUP_COUL', 1, iarg, 1,&
                    k8b, n)
        if (k8b(1:3) .eq. 'OUI') then
            lgrcou = .true.
        else
            lgrcou = .false.
        endif
!
!
    else if (cmd(5:8).eq.'GMSH') then
        call getvis(' ', 'UNITE_GMSH', 1, iarg, 1,&
                    nfie, n)
!
    endif
!
    call getvis(' ', 'UNITE_MAILLAGE', 1, iarg, 1,&
                nfis, n)
!
!
    if (cmd(5:9) .eq. 'IDEAS') then
        call stbast(nfie, nfis, lgrcou)
!
    else if (cmd(5:8).eq.'GMSH') then
        call gmsast(nfie, nfis)
!
    endif
!
end subroutine
