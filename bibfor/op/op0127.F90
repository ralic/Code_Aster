subroutine op0127()
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
!***********************************************************************
!    P. RICHARD     DATE 13/07/90
!-----------------------------------------------------------------------
!    BUT: CREER LA NUMEROTATION DES DEGRES DE LIBERTE GENERALISES
!    CONCEPT CREE: NUME_DDL_GENE
!-----------------------------------------------------------------------
!
    implicit none
!
!
!
!
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/crnslv.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jevtbl.h'
    include 'asterfort/nugeel.h'
    include 'asterfort/numgcy.h'
    include 'asterfort/numgen.h'
    include 'asterfort/nummod.h'
    include 'asterfort/smosli.h'
    include 'asterfort/strmag.h'
    character(len=8) :: nomres, modgen, modmec, option
    character(len=16) :: nomcon, nomope
    character(len=14) :: nugene
    character(len=24) :: typrof
    integer :: ibid1, ibid2, iopt
    real(kind=8) :: rtbloc
    integer :: iarg
!
!
!-----------------------------------------------------------------------
    call infmaj()
!
!-----RECUPERATION DU MODELE AMONT
!
    call getvid(' ', 'MODELE_GENE', 1, iarg, 1,&
                modgen, ibid1)
    call getvid(' ', 'BASE', 1, iarg, 1,&
                modmec, ibid2)
!
!
    call getvtx(' ', 'STOCKAGE', 0, iarg, 1,&
                typrof, ibid2)
!
    call getres(nomres, nomcon, nomope)
    nugene=nomres
!
!
!-----NUMEROTATION DES DEGRES DE LIBERTE
    if (ibid1 .ne. 0) then
        call getvtx(' ', 'METHODE', 1, iarg, 1,&
                    option, iopt)
        if (option .eq. 'CLASSIQU') then
            call numgen(nugene, modgen)
            call strmag(nugene, typrof)
        else if (option(1:7).eq.'INITIAL') then
            call numgcy(nugene, modgen)
        else if (option(1:7).eq.'ELIMINE') then
            call nugeel(nugene, modgen)
!          WRITE(6,*)' '
!          WRITE(6,*)'*** ON FORCE LE STOCKAGE PLEIN ***'
!          WRITE(6,*)' '
            typrof='PLEIN'
            call strmag(nugene, typrof)
        endif
    else if (ibid2.ne.0) then
        call nummod(nugene, modmec)
    endif
    call crnslv(nugene, 'LDLT', 'SANS', 'G')
!
!     -- ON CREE LE PROFIL LCIEL:
    rtbloc=jevtbl('TAILLE_BLOC')
    call smosli(nugene//'.SMOS', nugene//'.SLCS', 'G', rtbloc)
!
end subroutine
