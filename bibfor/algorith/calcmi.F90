subroutine calcmi(np1, nbm, dt0, dt, vitg,&
                  depg, vitg0, depg0, fmod, fmod0,&
                  amor, amor0, puls, puls0, trans,&
                  pulsd, s0, z0, sr0, za1,&
                  za2, za3, zin)
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
! TOLE  CRP_21
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DES DDLS GENERALISES A L'INSTANT N+1 PAR
! -----------   METHODE INTEGRALE (VERSION MULTI-MODALE)
!
!               APPELANTS : ALITMI, NEWTON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/algint.h'
    include 'asterfort/intfor.h'
    include 'asterfort/matran.h'
    include 'asterfort/parmat.h'
    include 'asterfort/tstpar.h'
    include 'asterfort/vecini.h'
    integer :: np1, nbm
    real(kind=8) :: dt0, dt, vitg(*), depg(*), vitg0(*), depg0(*), fmod(*)
    real(kind=8) :: fmod0(*), amor(*), amor0(*), puls(*), puls0(*)
    real(kind=8) :: trans(2, 2, *), pulsd(*)
    complex(kind=8) :: s0(*), z0(*), sr0(*), za1(*), za2(*), za3(*), zin(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: itestm
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL    ALGINT, LCINVN, INTFOR, MATRAN, PARMAT, TSTPAR
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!  0. INITIALISATIONS
!     ---------------
    itestm = 0
    call vecini(np1, 0.d0, depg)
    call vecini(np1, 0.d0, vitg)
!
!  1. TEST SUR LES MODIF. DES PARAMETRES DE CALCUL
!     --------------------------------------------
    call tstpar(itestm, nbm, amor, amor0, puls,&
                puls0, dt, dt0)
!
!  2. CALCUL DE LA MATRICE DE TRANSFERT
!     ---------------------------------
    if (itestm .eq. 0) then
        call parmat(nbm, dt, amor, puls, pulsd,&
                    s0, z0, sr0, za1, za2,&
                    za3)
        call matran(nbm, s0, z0, puls, pulsd,&
                    trans)
    endif
!
!  3. CALCUL DU TERME DE FORCAGE
!     --------------------------
    call intfor(nbm, fmod, fmod0, za1, za2,&
                za3, zin)
!
!  4. CALCUL DES DDLS GENERALISES A L'INSTANT N+1
!     -------------------------------------------
    call algint(nbm, vitg, vitg0, depg, depg0,&
                zin, trans, pulsd, s0)
!
! --- FIN DE CALCMI.
end subroutine
