subroutine calvol(np1, nbm, icoupl, indic, kmod00,&
                  cmod00, amor00, puls00, pulsi, amori,&
                  masgi, tpfl, veci1, vecr1, vecr2,&
                  vecr5, vecr3, vgap, vecr4, locfl0,&
                  amflu0, xsi0)
! aslint: disable=W1504
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DES CARACTERISTIQUES EN VOL
! -----------
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/coefmo.h'
    include 'asterfort/matini.h'
    include 'asterfort/vecini.h'
    integer :: np1, nbm, icoupl, indic
    real(kind=8) :: kmod00(np1, *), cmod00(np1, *), amor00(*), puls00(*)
    real(kind=8) :: pulsi(*), amori(*), masgi(*)
    character(len=8) :: tpfl
    integer :: veci1(*)
    real(kind=8) :: vecr1(*), vecr2(*), vecr5(*), vecr3(*), vgap, vecr4(*)
    logical :: locfl0(*)
    real(kind=8) :: amflu0(np1, *), xsi0(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i
    real(kind=8) :: xcf, r8b1, r8b2
    complex(kind=8) :: c16b
    logical :: lk
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL     COEFMO, MATINI, LCINVN
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call matini(np1, np1, 0.d0, kmod00)
    call matini(np1, np1, 0.d0, cmod00)
    call matini(np1, np1, 0.d0, amflu0)
    call vecini(np1, 0.d0, amor00)
    call vecini(np1, 0.d0, puls00)
!
    do 10 i = 1, nbm
        kmod00(i,i) = pulsi(i)*pulsi(i)
        cmod00(i,i) = amori(i)/masgi(i)
        amor00(i) = amori(i)/masgi(i)
        puls00(i) = pulsi(i)
10  end do
!
    if (icoupl .eq. 1) then
!....... LK = .FALSE. INDIQUE QU'ON NE CALCULE PAS LES TERMES DE RAIDEUR
        lk = .false.
        do 20 i = 1, nbm
            if (locfl0(i)) then
                call coefmo(tpfl, lk, nbm, i, indic,&
                            r8b1, puls00(i), vgap, xsi0(i), veci1,&
                            vecr1, vecr2, vecr3, vecr4, vecr5,&
                            r8b2, c16b, xcf)
                amflu0(i,i) = xcf/masgi(i)
            endif
20      continue
    endif
!
! --- FIN DE CALVOL.
end subroutine
