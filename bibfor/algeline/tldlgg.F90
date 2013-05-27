subroutine tldlgg(istop, lmat, ildeb, ilfin, ndigit,&
                  ndeci, isingu, npvneg, iret)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/tldlg3.h'
    include 'asterfort/uttcpu.h'
    character(len=8) :: renum
    character(len=16) :: metres
    character(len=19) :: noma19, solveu
    integer :: ibid, istop, lmat, ildeb, ilfin, ndigit
    integer :: ndeci, isingu, npvneg, iret
!
!     ------------------------------------------------------------------
    call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
    call uttcpu('CPU.RESO.4', 'DEBUT', ' ')
!
    noma19 = zk24(zi(lmat+1))
    call dismoi('F', 'METH_RESO', noma19, 'MATR_ASSE', ibid,&
                metres, ibid)
    call assert(metres .eq. 'LDLT' .or. metres .eq. 'MULT_FRONT' .or. metres .eq. 'MUMPS')
    call dismoi('F', 'RENUM_RESO', noma19, 'MATR_ASSE', ibid,&
                renum, ibid)
!     -- ON MET SCIEMMENT CETTE VALEUR CAR ON NE CONNAIT PAS A CET
!        ENDROIT LA SD SOLVEUR LIE A L'OPERATEUR (ELLE PEUT DIFFERER
!        DE CELLE LIEE AUX MATRICES). BESOIN UNIQUEMENT POUR MUMPS.
    solveu=' '
    call tldlg3(metres, renum, istop, lmat, ildeb,&
                ilfin, ndigit, ndeci, isingu, npvneg,&
                iret, solveu)
!
    call uttcpu('CPU.RESO.1', 'FIN', ' ')
    call uttcpu('CPU.RESO.4', 'FIN', ' ')
end subroutine
