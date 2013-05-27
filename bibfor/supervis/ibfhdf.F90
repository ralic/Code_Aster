subroutine ibfhdf(fichdf)
    implicit none
    include 'asterc/getres.h'
    include 'asterc/getvtx.h'
    character(len=80) :: fichdf
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!     LECTURE DU MOT CLE HDF DANS LA COMMANDE DEBUT
!
! ----------------------------------------------------------------------
    character(len=16) :: cbid, nomcmd, fhdf
    integer :: nfhdf
    integer :: iarg
!
    fichdf = ' '
    call getres(cbid, cbid, nomcmd)
    if (nomcmd .eq. 'POURSUITE') then
        call getvtx(' ', 'FORMAT_HDF', 1, iarg, 1,&
                    fhdf, nfhdf)
        if (nfhdf .gt. 0) then
            if (fhdf .eq. 'OUI') then
                fichdf = 'bhdf.1'
            endif
        endif
    endif
!
end subroutine
