! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! -------------------------------------------------------------------------
! Parameters definitions
! -------------------------------------------------------------------------
!
    integer :: parind(_INT_NBPAR)
    character(len=3)  :: partyp(_INT_NBPAR)
    character(len=8)  :: params(_INT_NBPAR)

    data params /'ACCE    ', 'AMOR_DIA', 'AMOR_FUL', 'DEPL    ', 'FORCE_EX', &
                 'INDEX   ', 'IND_ARCH', 'MASS_DIA', 'MASS_FAC', 'MASS_FUL', &
                 'MAT_UPDT', 'PARAMS  ', 'RIGI_DIA', 'RIGI_FUL', 'STEP    ', &
                 'TIME    ', 'VITE    ', 'WORK1   ', 'WORK2   ', 'WORK3   ', &
                 'WORK4   ', 'WORK5   ', 'WORK6   ', 'WORK7   '/

    data partyp /'R  ', 'R  ', 'R  ', 'R  ', 'R  ', &
                 'I  ', 'I  ', 'R  ', 'R  ', 'R  ', &
                 'I  ', 'R  ', 'R  ', 'R  ', 'R  ', &
                 'R  ', 'R  ', 'R  ', 'R  ', 'R  ', &
                 'R  ', 'R  ', 'R  ', 'R  '/ 

! -------------------------------------------------------------------------
!   parind = -2 : vector global        ; = -1 : scalar global ;
!          =  2 : vector per occurence ; =  1 : scalar per occurence
! -------------------------------------------------------------------------

    data parind / 2,  2,  2,  2,  2, &
                  1, -1,  2,  2,  2, &
                 -1, -2,  2,  2,  1, &
                  1,  2, -2, -2, -2, &
                 -2, -2, -2, -2/