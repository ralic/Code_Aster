subroutine dinonc(nomte, icodre, valre, klv, raide,&
                  nbpar, param, okdire)
    implicit none
#include "asterf_types.h"
#include "asterfort/utmess.h"
    character(len=16) :: nomte
    integer :: icodre(*)
    integer :: nbpar
    real(kind=8) :: valre(*), klv(*), raide(*), param(6, nbpar)
    aster_logical :: okdire(6)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!           AFFECTATION DES VALEURS ISSUES DU COMPORTEMENT
!
!   si on essaye d'affecter un comportement sur un ddl non autorise on sort en 'f'
!
!   pour que cela fonctionne correctement il faut que les paramètres soient ranges dans le
!   data 'nomre' de la façon suivante
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
! --------------------------------------------------------------------------------------------------
!
!  IN
!     nomte : nom de l'élément
!     icodre : 0 si le coeff est présent sinon 1
!     valre : valeur des coefficients
!     klv   : raideur élastique du discret
!     nbpar : nombre de paramètre de la loi par ddl
!
!  OUT
!     raide  : raideur au comportement
!     param  : paramètres de la loi
!     okdire : vrai si la direction est affectée par le comportement
!
! --------------------------------------------------------------------------------------------------
    integer :: ii, jj
!
    do ii = 1, 6
        okdire(ii)= .false.
    enddo
!
    if ((nomte .eq. 'MECA_DIS_TR_N') .or. (nomte .eq. 'MECA_DIS_TR_L')) then
        do ii = 0, 5
            do jj = 1, nbpar
                if (icodre(nbpar*ii+jj) .eq. 0) then
                    param(ii+1,jj) = valre(nbpar*ii+jj)
                    okdire(ii+1) = .true.
                endif
            enddo
        enddo
        raide(1)= klv(1)
        raide(2)= klv(3)
        raide(3)= klv(6)
        raide(4)= klv(10)
        raide(5)= klv(15)
        raide(6)= klv(21)
    endif
    if ((nomte .eq. 'MECA_DIS_T_N') .or. (nomte .eq. 'MECA_DIS_T_L')) then
        do ii = 0, 2
            do jj = 1, nbpar
                if (icodre(nbpar*ii+jj) .eq. 0) then
                    param(ii+1,jj) = valre(nbpar*ii+jj)
                    okdire(ii+1) = .true.
                endif
            enddo
        enddo
        do ii = 3, 5
            do jj = 1, nbpar
                if (icodre(nbpar*ii+jj) .eq. 0) then
                    call utmess('F', 'DISCRETS_1', sk=nomte)
                endif
            enddo
        enddo
        raide(1)= klv(1)
        raide(2)= klv(3)
        raide(3)= klv(6)
    endif
    if ((nomte .eq. 'MECA_2D_DIS_TR_N') .or. (nomte .eq. 'MECA_2D_DIS_TR_L')) then
        do ii = 0, 1
            do jj = 1, nbpar
                if (icodre(nbpar*ii+jj) .eq. 0) then
                    param(ii+1,jj) = valre(nbpar*ii+jj)
                    okdire(ii+1) = .true.
                endif
            enddo
        enddo
        ii= 5
        do jj = 1, nbpar
            if (icodre(nbpar*ii+jj) .eq. 0) then
                param(3,jj) = valre(nbpar*ii+jj)
                okdire(3) = .true.
            endif
        enddo
        do ii = 2, 4
            do jj = 1, nbpar
                if (icodre(nbpar*ii+jj) .eq. 0) then
                    call utmess('F', 'DISCRETS_2', sk=nomte)
                endif
            enddo
        enddo
        raide(1)= klv(1)
        raide(2)= klv(3)
        raide(3)= klv(6)
    endif
    if ((nomte .eq. 'MECA_2D_DIS_T_N') .or. (nomte .eq. 'MECA_2D_DIS_T_L')) then
        do ii = 0, 1
            do jj = 1, nbpar
                if (icodre(nbpar*ii+jj) .eq. 0) then
                    param(ii+1,jj) = valre(nbpar*ii+jj)
                    okdire(ii+1) = .true.
                endif
            enddo
        enddo
        do ii = 2, 5
            do jj = 1, nbpar
                if (icodre(nbpar*ii+jj) .eq. 0) then
                    call utmess('F', 'DISCRETS_3', sk=nomte)
                endif
            enddo
        enddo
        raide(1)= klv(1)
        raide(2)= klv(3)
    endif
!
end subroutine
