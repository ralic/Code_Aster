subroutine cffrot(maf1, koper, maf2, mafrot, numedd)
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
! RESPONSBALE
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisd.h'
    include 'asterfort/mtcmbl.h'
    include 'asterfort/mtdefs.h'
    character(len=1) :: koper
    character(len=19) :: maf1
    character(len=19) :: maf2
    character(len=19) :: mafrot
    character(len=14) :: numedd
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - FROTTEMENT)
!
! CALCUL DE LA MATRICE DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  MAF1   : PARTIE 1 DE LA MATRICE FROTTEMENT
! IN  KOPER  : ADDITION ('+') OU SOUSTRACTION ('-')
! IN  MAF2   : PARTIE 2 DE LA MATRICE FROTTEMENT
! OUT MAFROT : MATRICE GLOBALE TANGENTE AVEC FROTTEMENT RESULTANTE
! OUT NUMEDD : NUME_DDL DE LA MATRICE DE FROTTEMENT
!
!
!
!
    integer :: iret, ibid, ier
    real(kind=8) :: coefmu(2)
    character(len=1) :: typcst(2)
    character(len=14) :: numedf, numef1, numef2
    character(len=24) :: limat(2)
! ----------------------------------------------------------------------
!
! --- DESTRUCTION ANCIENNE MATRICE FROTTEMENT
!
    call exisd('MATR_ASSE', mafrot, iret)
    if (iret .ne. 0) then
        call dismoi('F', 'NOM_NUME_DDL', mafrot, 'MATR_ASSE', ibid,&
                    numedf, ier)
        call detrsd('NUME_DDL', numedf)
        call detrsd('MATR_ASSE', mafrot)
    endif
!
! --- PREPARATION COMBINAISON LINEAIRE MAFROT=MAF1-MAF2
!
    limat(1) = maf1
    limat(2) = maf2
    coefmu(1) = 1.0d0
    if (koper .eq. '+') then
        coefmu(2) = +1.0d0
    else if (koper.eq.'-') then
        coefmu(2) = -1.0d0
    else
        call assert(.false.)
    endif
    typcst(1) = 'R'
    typcst(2) = 'R'
!
! --- COMBINAISON LINEAIRE MAFROT=MAF1-MAF2
!
    call mtdefs(mafrot, maf1, 'V', 'R')
    call mtcmbl(2, typcst, coefmu, limat, mafrot,&
                ' ', numedd, 'ELIM=')
!
! --- DESTRUCTION DES NUME_DDL
!
    call dismoi('F', 'NOM_NUME_DDL', maf1, 'MATR_ASSE', ibid,&
                numef1, ier)
    call dismoi('F', 'NOM_NUME_DDL', maf2, 'MATR_ASSE', ibid,&
                numef2, ier)
    call detrsd('NUME_DDL', numef1)
    call detrsd('NUME_DDL', numef2)
!
! --- DESTRUCTION DES MATRICES DE CONSTRUCTION
!
    call detrsd('MATR_ASSE', maf1)
    call detrsd('MATR_ASSE', maf2)
!
end subroutine
