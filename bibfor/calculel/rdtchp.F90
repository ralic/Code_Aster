subroutine rdtchp(corrn, corrm, ch1, ch2, base,&
                  noma, nomare, ligrel, cret)
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/carces.h'
    include 'asterfort/celces.h'
    include 'asterfort/cescar.h'
    include 'asterfort/cescel.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/cnscno.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/rdtces.h'
    include 'asterfort/rdtcns.h'
    integer :: cret
    character(len=1) :: base
    character(len=8) :: noma, nomare
    character(len=24) :: corrn, corrm
    character(len=19) :: ch1, ch2, ligrel
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
! person_in_charge: jacques.pellet at edf.fr
!-------------------------------------------------------------------
! BUT: REDUIRE UNE SD_CHAMP SUR UN MAILLAGE REDUIT
!
!  CH1    : IN  : CHAMP A REDUIRE
!  CH2    : OUT : CHAMP REDUIT
!  NOMA   : IN  : MAILLAGE AVANT REDUCTION
!  NOMARE : IN  : MAILLAGE REDUIT
!  LIGREL : IN  : LIGREL REDUIT
!  CORRN  : IN  : NOM DE L'OBJET CONTENANT LA CORRESPONDANCE
!                 INO_RE -> INO
!  CORRM  : IN  : NOM DE L'OBJET CONTENANT LA CORRESPONDANCE
!                 IMA_RE -> IMA
!-------------------------------------------------------------------
!
    integer :: ibid
    integer :: iret, nncp
    character(len=16) :: option
    character(len=4) :: tych
    character(len=19) :: ch1s, ch2s
!     -----------------------------------------------------------------
!
    call jemarq()
!
    call assert(noma.ne.nomare)
    call assert(ch1.ne.ch2)
!
    call dismoi('F', 'TYPE_CHAMP', ch1, 'CHAMP', ibid,&
                tych, ibid)
!
    ch1s='&&RDTCHP'//'.CH1S'
    ch2s='&&RDTCHP'//'.CH2S'
!
    cret=0
    if (tych .eq. 'NOEU') then
        call cnocns(ch1, 'V', ch1s)
        call rdtcns(nomare, corrn, ch1s, 'V', ch2s)
        call cnscno(ch2s, ' ', 'NON', base, ch2,&
                    ' ', cret)
        call detrsd('CHAM_NO_S', ch1s)
        call detrsd('CHAM_NO_S', ch2s)
!
!
    else if (tych(1:2).eq.'EL') then
        call celces(ch1, 'V', ch1s)
        call rdtces(nomare, corrm, ch1s, 'V', ch2s,&
                    cret)
        if (cret .eq. 0) then
            call dismoi('F', 'NOM_OPTION', ch1, 'CHAMP', ibid,&
                        option, ibid)
            call cescel(ch2s, ligrel, option, ' ', 'OUI',&
                        nncp, base, ch2, 'F', iret)
            call assert(iret.eq.0)
            call assert(nncp.eq.0)
        endif
        call detrsd('CHAM_ELEM_S', ch1s)
        call detrsd('CHAM_ELEM_S', ch2s)
!
!
    else if (tych.eq.'CART') then
        call carces(ch1, 'ELEM', ' ', 'V', ch1s,&
                    'A', iret)
        call assert(iret.eq.0)
        call rdtces(nomare, corrm, ch1s, 'V', ch2s,&
                    cret)
        call cescar(ch2s, ch2, base)
        call assert(iret.eq.0)
        call detrsd('CHAM_ELEM_S', ch1s)
        call detrsd('CHAM_ELEM_S', ch2s)
!
!
    else
        call assert(.false.)
    endif
!
!
!
!
    call jedema()
!
end subroutine
