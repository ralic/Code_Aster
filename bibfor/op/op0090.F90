subroutine op0090()
    implicit none
!     ------------------------------------------------------------------
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
! person_in_charge: mathieu.courtois at edf.fr
!     OPERATEUR "RECU_FONCTION"
!     ------------------------------------------------------------------
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/chpve2.h'
    include 'asterfort/rfbefl.h'
    include 'asterfort/rfinte.h'
    include 'asterfort/rfnapp.h'
    include 'asterfort/rfnoch.h'
    include 'asterfort/rfrcha.h'
    include 'asterfort/rfresu.h'
    include 'asterfort/rfrgen.h'
    include 'asterfort/rftabl.h'
    integer :: nreg, nrb, nch, ng, ier
    integer :: nta, nres, nc, nna
    character(len=8) :: k8b
    character(len=19) :: cham19, resu, tabres, tabtyp(8), nappe
    integer :: iarg
    data tabtyp/'NOEU#DEPL_R','NOEU#TEMP_R','NOEU#PRES_R',&
     &            'ELXX#SIEF_R','ELXX#VARI_R','ELXX#EPSI_R',&
     &            'ELXX#FLUX_R','ELXX#PRES_R'/
!     ------------------------------------------------------------------
!
!     -----------------------------------------------------------------
!                      --- CAS D'UN CHAM_GD ---
!     -----------------------------------------------------------------
    call getvid(' ', 'CHAM_GD', 0, iarg, 1,&
                cham19, nch)
    if (nch .ne. 0) then
        call chpve2(cham19, 8, tabtyp, ier)
        call rfrcha()
        goto 10
    endif
!
!     -----------------------------------------------------------------
!                       --- CAS D'UN RESULTAT ---
!     -----------------------------------------------------------------
    call getvid(' ', 'RESULTAT ', 0, iarg, 1,&
                resu, nres)
    if (nres .ne. 0) then
        call rfresu()
        goto 10
    endif
!
!     -----------------------------------------------------------------
!                   --- CAS D'UN NOEUD DE CHOC ---
!     -----------------------------------------------------------------
    call getvtx(' ', 'NOEUD_CHOC', 0, iarg, 1,&
                k8b, nc)
    call getvtx(' ', 'GROUP_NO_CHOC', 0, iarg, 1,&
                k8b, ng)
    if (nc+ng .ne. 0) then
        call rfnoch()
        goto 10
    endif
!
!     -----------------------------------------------------------------
!                    --- CAS D'UN RESU_GENE ---
!     -----------------------------------------------------------------
    call getvid(' ', 'RESU_GENE', 0, iarg, 1,&
                resu, nreg)
    if (nreg .ne. 0) then
        call rfrgen(resu)
        goto 10
    endif
!
!     -----------------------------------------------------------------
!                       --- CAS D'UNE TABLE ---
!     -----------------------------------------------------------------
    call getvid(' ', 'TABLE', 0, iarg, 1,&
                tabres, nta)
    if (nta .ne. 0) then
        call rftabl(tabres)
        goto 10
    endif
!
!     -----------------------------------------------------------------
!                 --- CAS D'UNE BASE_ELAS_FLUI ---
!     -----------------------------------------------------------------
    call getvid(' ', 'BASE_ELAS_FLUI', 0, iarg, 1,&
                resu, nrb)
    if (nrb .ne. 0) then
        call rfbefl(resu)
        goto 10
    endif
!
!     -----------------------------------------------------------------
!                 --- CAS D'UNE SD_INTERSPECTRE ---
!     -----------------------------------------------------------------
    call getvid(' ', 'INTE_SPEC', 0, iarg, 1,&
                resu, nrb)
    if (nrb .ne. 0) then
        call rfinte(resu)
        goto 10
    endif
!
!     -----------------------------------------------------------------
!     -----------------------------------------------------------------
!                     --- CAS D'UNE NAPPE ---
!     -----------------------------------------------------------------
    call getvid(' ', 'NAPPE', 0, iarg, 1,&
                nappe, nna)
    if (nna .ne. 0) then
        call rfnapp(nappe)
        goto 10
    endif
!
!     -----------------------------------------------------------------
10  continue
!
end subroutine
