subroutine hujma2(mod, imat, nmat, tempf, angmas,&
                  sigd, vind, materd, materf, ndt,&
                  ndi, nvi, nr, matcst)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: alexandre.foucault at edf.fr
    implicit   none
!       ----------------------------------------------------------------
!       RECUPERATION DU MATERIAU A TEMPF ET AJUSTEMENT SEUILS
!       IN  MOD    :  TYPE DE MODELISATION
!           IMAT   :  ADRESSE DU MATERIAU CODE
!           NMAT   :  DIMENSION 1 DE MATER
!           TEMPF  :  TEMPERATURE A T + DT
!          ANGMAS  :  ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM - 3CMP)
!           SIGD   :  ETAT CONTRAINTES A T
!           VIND   :  VARIABLE INTERNE A T
!       OUT MATERF :  COEFFICIENTS MATERIAU A T+DT (TEMPF )
!                     MATER(*,I) = CARACTERISTIQUES MATERIAU
!                                    I = 1  CARACTERISTIQUES ELASTIQUES
!                                    I = 2  CARACTERISTIQUES PLASTIQUES
!           MATERD : PARAMETRES MATERIAU A T
!           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
!                     'NON' SINON OU 'NAP' SI NAPPE DANS 'VECMAT.F'
!           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!           NR     :  NB DE COMPOSANTES SYSTEME NL
!           NVI    :  NB DE VARIABLES INTERNES
!       ----------------------------------------------------------------
    include 'asterc/r8prem.h'
    include 'asterc/r8vide.h'
    include 'asterfort/hujcrd.h'
    include 'asterfort/hujcri.h'
    include 'asterfort/hujmat.h'
    include 'asterfort/hujori.h'
    include 'asterfort/hujprj.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: mod
    character(len=3) :: matcst
    integer :: imat, nmat, ndt, ndi, nvi, nr
    real(kind=8) :: tempf, materd(nmat, 2), materf(nmat, 2), vind(50)
    real(kind=8) :: sigd(6), angmas(3)
!
    logical :: reorie
    real(kind=8) :: zero, bid66(6, 6), seuil, tin(3), piso, q
    real(kind=8) :: ptrac, b, phi, m, pc0, degr, d, un, trois
    real(kind=8) :: matert(22, 2)
    integer :: i, j
    parameter   ( zero  = 0.d0 )
    parameter   ( un    = 1.d0 )
    parameter   ( trois = 3.d0 )
    parameter   ( degr  = 0.0174532925199D0 )
!     ------------------------------------------------------------------
! ----------------------------------------------------------------------
! ---  NR     :  NB DE COMPOSANTES MAXIMUM DU SYSTEME NL
! ----------------------------------------------------------------------
    nr = 18
!
! ----------------------------------------------------------------------
! ---  RECUPERATION DE MATERF, NDT, NDI, NVI ET MATERD
! ----------------------------------------------------------------------
    matcst = 'OUI'
    call hujmat(mod, imat, tempf, matert, ndt,&
                ndi, nvi)
!
    do 10 i = 1, 22
        do 20 j = 1, 2
            materd(i,j) = matert(i,j)
            materf(i,j) = matert(i,j)
20      continue
10  end do
!
! ----------------------------------------------------------------------
! --- CONTROLE DE LA DIMENSION DE LA MODELISATION
! --- AJUSTEMENT NECESSAIRE SI MODELISATION TYPE D_PLAN
! ----------------------------------------------------------------------
    if (mod(1:6) .eq. 'D_PLAN') then
        sigd(5) = zero
        sigd(6) = zero
        ndt = 6
    endif
! ----------------------------------------------------------------------
! --- CONTROLE DES EQUILIBRES DE SEUILS PLASTIQUES
! ----------------------------------------------------------------------
! --- 1 ORIENTATION DES CONTRAINTES SELON ANGMAS VERS REPERE LOCAL
    if (angmas(1) .eq. r8vide()) call u2mess('F', 'ALGORITH8_20')
    reorie =(angmas(1).ne.zero) .or. (angmas(2).ne.zero)&
     &         .or. (angmas(3).ne.zero)
    call hujori('LOCAL', 1, reorie, angmas, sigd,&
                bid66)
!
! --- 2 INITIALISATION SEUIL DEVIATOIRE SI NUL
    ptrac = materf(21,2)
    do 30 i = 1, ndi
        if (vind(i) .eq. zero) then
            if (materf(13, 2) .eq. zero) then
                vind(i) = 1.d-3
            else
                vind(i) = materf(13,2)
            endif
!
            call hujcrd(i, matert, sigd, vind, seuil)
!
! --- SI LE SEUIL EST DESEQUILIBRE A L'ETAT INITIAL
!     ON EQUILIBRE LE SEUIL EN CALCULANT LA VALEUR DE R
!     APPROPRIEE
            if (seuil .gt. zero) then
                call hujprj(i, sigd, tin, piso, q)
                piso = piso - ptrac
                b = materf(4,2)
                phi = materf(5,2)
                m = sin(degr*phi)
                pc0 = materf(7,2)
                vind(i) = -q/(m*piso*(un-b*log(piso/pc0)))
                vind(23+i) = un
            endif
        endif
30  continue
!
! ---> 3 INITIALISATION SEUIL ISOTROPE SI NUL
    if (vind(4) .eq. zero) then
        if (materf(14, 2) .eq. zero) then
            vind(4) = 1.d-3
        else
            vind(4) = materf(14,2)
        endif
!
        call hujcri(matert, sigd, vind, seuil)
!
! --- SI LE SEUIL EST DESEQUILIBRE A L'ETAT INITIAL
!     ON EQUILIBRE LE SEUIL EN CALCULANT LA VALEUR DE R
!     APPROPRIEE
!
        if (seuil .gt. zero) then
            piso = (sigd(1)+sigd(2)+sigd(3))/trois
            d = materf(3,2)
            pc0 = materf(7,2)
            vind(4) = piso/(d*pc0)
            vind(27)= un
        endif
    endif
!
! ---> 4 INITIALISATION SEUIL CYCLIQUE SI NUL
    do 40 i = 1, ndi
        if (vind(4+i) .eq. zero) then
            if (materf(18, 2) .eq. zero) then
                vind(4+i) = 1.d-3
            else
                vind(4+i) = materf(18,2)
            endif
        endif
40  continue
!
    if (vind(8) .eq. zero) then
        if (materf(19, 2) .eq. zero) then
            vind(8) = 1.d-3
        else
            vind(8) = materf(19,2)
        endif
    endif
!
! --- 5 CONTROLE DES INDICATEURS DE PLASTICITE
    do 50 i = 1, 4
        if (abs(vind(27+i)-un) .lt. r8prem()) vind(23+i)=-un
50  continue
!
! --- 7 ORIENTATION DES CONTRAINTES SELON ANGMAS VERS REPERE GLOBAL
    call hujori('GLOBA', 1, reorie, angmas, sigd,&
                bid66)
!
! ----------------------------------------------------------------------
end subroutine
