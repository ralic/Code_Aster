subroutine dinonc(nomte, icodre, valre, klv, raide,&
                  nbpar, param, nploi, okdire)
! ----------------------------------------------------------------------
    implicit none
    include 'asterfort/u2mesk.h'
    character(len=16) :: nomte
    integer :: icodre(*)
    integer :: nbpar, nploi
    real(kind=8) :: valre(*), klv(*), raide(*), param(6, nbpar)
    logical :: okdire(6)
!
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ======================================================================
!           AFFECTATION DES VALEURS ISSUES DU COMPORTEMENT
!
!   SI ON ESSAYE D'AFFECTER UN COMPORTEMENT SUR UN DDL NON AUTORISE
!   ON SORT EN 'F'
!
!   POUR QUE CELA FONCTIONNE CORRECTEMENT IL FAUT QUE LES PARAMETRES
!   SOIENT RANGES DANS LE DATA 'NOMRE' (TE0047) DE LA FACON SUIVANTE
!
!   PARAMETRES SUIVANT X        PARAMETRES SUIVANT Y        ETC
!   P_1_DX  P_2_DX  P_3_DX ...  P_1_DY  P_2_DY  P_3_DY ...
!
!     NOMRE  /'FLIM_X','PUIS_DX',
!             'FLIM_Y','PUIS_DY',
!             'FLIM_Z','PUIS_DZ',
!             'MLIM_X','PUIS_RX',
!             'MLIM_Y','PUIS_RY',
!             'MLIM_Z','PUIS_RZ'/
! ======================================================================
!
!  IN
!     NOMTE : NOM DE L'ELEMENT
!     ICODRE : 0 SI LE COEFF EST PRESENT SINON 1
!     VALRE : VALEUR DES COEFFICIENTS
!     KLV   : RAIDEUR ELASTIQUE DU DISCRET
!     NBPAR : NOMBRE DE PARAMETRE MAXIMUM DE LA LOI
!     NPLOI : NOMBRE DE PARAMETRE DE LA LOI PAR DDL
!  OUT
!     RAIDE  : RAIDEUR AU COMPORTEMENT
!     PARAM  : PARAMETRES DE LA LOI
!     OKDIRE : VRAI SI LA DIRECTION EST AFFECTEE PAR LE COMPORTEMENT
!
!***************** DECLARATION DES VARIABLES LOCALES *******************
!
    integer :: ii, jj
!
!************ FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
!
    do 50 ii = 1, 6
        okdire(ii)= .false.
50  end do
!
    if ((nomte .eq. 'MECA_DIS_TR_N') .or. (nomte .eq. 'MECA_DIS_TR_L')) then
        do 101 ii = 0, 5
            do 102 jj = 1, nploi
                if (icodre(nploi*ii+jj) .eq. 0) then
                    param(ii+1,jj) = valre(nploi*ii+jj)
                    okdire(ii+1) = .true.
                endif
102          continue
101      continue
        raide(1)= klv(1)
        raide(2)= klv(3)
        raide(3)= klv(6)
        raide(4)= klv(10)
        raide(5)= klv(15)
        raide(6)= klv(21)
    endif
    if ((nomte .eq. 'MECA_DIS_T_N') .or. (nomte .eq. 'MECA_DIS_T_L')) then
        do 105 ii = 0, 2
            do 106 jj = 1, nploi
                if (icodre(nploi*ii+jj) .eq. 0) then
                    param(ii+1,jj) = valre(nploi*ii+jj)
                    okdire(ii+1) = .true.
                endif
106          continue
105      continue
        do 107 ii = 3, 5
            do 108 jj = 1, nploi
                if (icodre(nploi*ii+jj) .eq. 0) then
                    call u2mesk('F', 'DISCRETS_1', 1, nomte)
                endif
108          continue
107      continue
        raide(1)= klv(1)
        raide(2)= klv(3)
        raide(3)= klv(6)
    endif
    if ((nomte .eq. 'MECA_2D_DIS_TR_N') .or. (nomte .eq. 'MECA_2D_DIS_TR_L')) then
        do 110 ii = 0, 1
            do 111 jj = 1, nploi
                if (icodre(nploi*ii+jj) .eq. 0) then
                    param(ii+1,jj) = valre(nploi*ii+jj)
                    okdire(ii+1) = .true.
                endif
111          continue
110      continue
        ii= 5
        do 112 jj = 1, nploi
            if (icodre(nploi*ii+jj) .eq. 0) then
                param(3,jj) = valre(nploi*ii+jj)
                okdire(3) = .true.
            endif
112      continue
        do 113 ii = 2, 4
            do 114 jj = 1, nploi
                if (icodre(nploi*ii+jj) .eq. 0) then
                    call u2mesk('F', 'DISCRETS_2', 1, nomte)
                endif
114          continue
113      continue
        raide(1)= klv(1)
        raide(2)= klv(3)
        raide(3)= klv(6)
    endif
    if ((nomte .eq. 'MECA_2D_DIS_T_N') .or. (nomte .eq. 'MECA_2D_DIS_T_L')) then
        do 115 ii = 0, 1
            do 116 jj = 1, nploi
                if (icodre(nploi*ii+jj) .eq. 0) then
                    param(ii+1,jj) = valre(nploi*ii+jj)
                    okdire(ii+1) = .true.
                endif
116          continue
115      continue
        do 117 ii = 2, 5
            do 118 jj = 1, nploi
                if (icodre(nploi*ii+jj) .eq. 0) then
                    call u2mesk('F', 'DISCRETS_3', 1, nomte)
                endif
118          continue
117      continue
        raide(1)= klv(1)
        raide(2)= klv(3)
    endif
!
end subroutine
