subroutine op0023()
    implicit   none
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     COMMANDE:  TEST_RESU
! ----------------------------------------------------------------------
!     REMARQUES:  RESU:( RESULTAT:
!                        PRECISION: ( PREC1 , PREC2 )          L_R8
!                        CRITERE  : ( CRIT1 , CRIT2 )          L_TXM
!     PREC1 ET CRIT1 SONT LA PRECISION ET LE CRITERE DU TEST
!     PREC2 ET CRIT2 SONT LA PRECISION ET LE CRITERE DE L'EXTRACTION
! ----------------------------------------------------------------------
    include 'asterc/getfac.h'
    include 'asterc/getvtx.h'
    include 'asterc/iisnan.h'
    include 'asterc/r8nnem.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/trcart.h'
    include 'asterfort/trchel.h'
    include 'asterfort/trchno.h'
    include 'asterfort/trgene.h'
    include 'asterfort/trjeve.h'
    include 'asterfort/trresu.h'
    include 'asterfort/ulexis.h'
    include 'asterfort/ulopen.h'
    real(kind=8) :: tstnan, resnan
    integer :: ific, nocc, n
    logical :: nfac
    character(len=8) :: repons
    character(len=16) :: nomfi
    integer :: iarg
!     ------------------------------------------------------------------
!     TEST DU MECANISME DE NAN
    call getvtx(' ', 'TEST_NAN', 1, iarg, 1,&
                repons, n)
    if (repons .eq. 'OUI') then
        tstnan = r8nnem ( )
        resnan = tstnan*1.d0
        if (iisnan(resnan) .ne. 0) resnan = 0.d0
    endif
!
    call infmaj()
!
    nomfi = ' '
    ific = iunifi('RESULTAT')
    if (.not. ulexis( ific )) then
        call ulopen(ific, ' ', nomfi, 'NEW', 'O')
    endif
    write (ific,1000)
    nfac=.false.
!
!     --- TRAITEMENT D'UN OBJET JEVEUX  ---
!
    call getfac('OBJET', nocc)
    if (nocc .ne. 0) then
                     call trjeve(ific, nocc)
                     if (nfac) then
                        call u2mess('F', 'CALCULEL6_96')
                     else
                        nfac=.true.
                     endif
    endif

!
!     --- TRAITEMENT D'UN CHAM_NO ---
!
    call getfac('CHAM_NO', nocc)
    if (nocc .ne. 0) then
                     call trchno(ific, nocc)
                     if (nfac) then
                        call u2mess('F', 'CALCULEL6_96')
                     else
                        nfac=.true.
                     endif
    endif
!
!     --- TRAITEMENT D'UN CHAM_ELEM ---
!
    call getfac('CHAM_ELEM', nocc)
    if (nocc .ne. 0) then
                     call trchel(ific, nocc)
                     if (nfac) then
                        call u2mess('F', 'CALCULEL6_96')
                     else
                        nfac=.true.
                     endif
    endif
!
!     --- TRAITEMENT D'UNE CARTE ---
!
    call getfac('CARTE', nocc)
    if (nocc .ne. 0) then
                     call trcart(ific, nocc)
                     if (nfac) then
                        call u2mess('F', 'CALCULEL6_96')
                     else
                        nfac=.true.
                     endif
    endif
!
!     --- TRAITEMENT D'UN CONCEPT RESULTAT ---
!
    call getfac('RESU', nocc)
    if (nocc .ne. 0) then
                     call trresu(ific, nocc)
                     if (nfac) then
                        call u2mess('F', 'CALCULEL6_96')
                     else
                        nfac=.true.
                     endif
    endif
!
!     --- TRAITEMENT D'UN CONCEPT GENE ---
!
    call getfac('GENE', nocc)
    if (nocc .ne. 0) then
                     call trgene(ific, nocc)
                     if (nfac) then
                        call u2mess('F', 'CALCULEL6_96')
                     else
                        nfac=.true.
                     endif
    endif
!
    1000 format (/,80 ('-'))
end subroutine
