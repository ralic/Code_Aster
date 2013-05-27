subroutine caltra(np1, np4, nbm, nfour, ttrans,&
                  ttran0, vitgtr, depgtr, vitg0, depg0,&
                  masgi, amor, puls, pulsd, mtrans,&
                  s0, z0, sr0, za1, za2,&
                  za3, za4, za5, zitr, zin,&
                  fextt0, fexttr, dttr, omegaf, aa,&
                  bb, ntrans)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! DESCRIPTION : CALCUL DES DDLS GENERALISES A L'INSTANT TTRANS PAR
! ------------  METHODE INTEGRALE (VERSION MULTI-MODALE) PERMETTANT
!               LE PASSAGE DU TRANSITOIRE
!
!               APPELANT : TRANSI
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/algint.h'
    include 'asterfort/intfor.h'
    include 'asterfort/intftr.h'
    include 'asterfort/matran.h'
    include 'asterfort/parmat.h'
    include 'asterfort/parmtr.h'
    include 'asterfort/vecini.h'
    integer :: np1, np4, nbm, nfour
    real(kind=8) :: ttrans, ttran0, vitgtr(*), depgtr(*), vitg0(*), depg0(*)
    real(kind=8) :: masgi(*), amor(*), puls(*), pulsd(*), mtrans(2, 2, *)
    complex(kind=8) :: s0(*), z0(*), sr0(*), za1(*), za2(*), za3(*), za4(np4, *)
    complex(kind=8) :: za5(np4, *), zitr(*), zin(*)
    real(kind=8) :: fextt0(*), fexttr(*), dttr, omegaf(*), aa(np4, *)
    real(kind=8) :: bb(np4, *)
    integer :: ntrans
!
! VARIABLES LOCALES
! -----------------
    integer :: i
    real(kind=8) :: dttran
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL    ALGINT, LCINVN, INTFOR, INTFTR, MATRAN, PARMAT, PARMTR
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!-----------------------------------------------------------------------
!  0. INITIALISATIONS
!-----------------------------------------------------------------------
!
    call vecini(np1, 0.d0, depgtr)
    call vecini(np1, 0.d0, vitgtr)
!
!-----------------------------------------------------------------------
!  1. PREMIERE ESTIMATION DE LA DUREE DU TRANSITOIRE
!-----------------------------------------------------------------------
!
    if (ntrans .eq. 0) then
!
!--1.1   CALCUL DES PARAMETRES METHODE INTEGRALE
!
        dttran = ttrans - ttran0
        call parmtr(np4, nfour, nbm, dttran, amor,&
                    puls, pulsd, s0, z0, omegaf,&
                    za4, za5)
!
!--1.2   CALCUL DE LA MATRICE DE TRANSFERT
!
        call matran(nbm, s0, z0, puls, pulsd,&
                    mtrans)
!
!--1.3   CALCUL DU TERME DE FORCAGE
!
        call intftr(np4, nfour, nbm, za4, za5,&
                    aa, bb, zitr)
        do 13 i = 1, nbm
            zitr(i) = zitr(i) / masgi(i)
13      continue
!
!--1.4   CALCUL DES DDLS GENERALISES A L'INSTANT N+1
!
        call algint(nbm, vitgtr, vitg0, depgtr, depg0,&
                    zitr, mtrans, pulsd, s0)
!
!-----------------------------------------------------------------------
!  2. DEUXIEME ESTIMATION DE LA DUREE DU TRANSITOIRE
!-----------------------------------------------------------------------
!
    else if (ntrans.eq.1) then
!
!--2.1   CALCUL DES PARAMETRES METHODE INTEGRALE
!
        call parmat(nbm, dttr, amor, puls, pulsd,&
                    s0, z0, sr0, za1, za2,&
                    za3)
!
!--2.2   CALCUL DE LA MATRICE DE TRANSFERT
!
        call matran(nbm, s0, z0, puls, pulsd,&
                    mtrans)
!
!--2.3   CALCUL DU TERME DE FORCAGE
!
        call intfor(nbm, fexttr, fextt0, za1, za2,&
                    za3, zin)
        do 23 i = 1, nbm
            zin(i) = zin(i) / masgi(i)
23      continue
!
!--2.4   CALCUL DES DDLS GENERALISES A L'INSTANT N+1
!
        call algint(nbm, vitgtr, vitg0, depgtr, depg0,&
                    zin, mtrans, pulsd, s0)
!
!-----------------------------------------------------------------------
!  3. ESTIMATIONS ULTERIEURES
!-----------------------------------------------------------------------
!
    else
!
!--3.1   CALCUL DU TERME DE FORCAGE
!
        call intfor(nbm, fexttr, fextt0, za1, za2,&
                    za3, zin)
        do 31 i = 1, nbm
            zin(i) = zin(i) / masgi(i)
31      continue
!
!--3.2   CALCUL DES DDLS GENERALISES A L'INSTANT N+1
!
        call algint(nbm, vitgtr, vitg0, depgtr, depg0,&
                    zin, mtrans, pulsd, s0)
!
    endif
!
! --- FIN DE CALTRA.
end subroutine
