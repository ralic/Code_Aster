subroutine mechti(noma, inst, deltat, theta, chtime)
!     ------------------------------------------------------------------
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
!     CREE UNE CARTE D'INSTANT
!     ------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : TIME   : INSTANT DE CALCUL
! IN  : DELTAT : PAS DE TEMPS PRECEDENT L'INSTANT COURANT
! IN  : THETA  : COEFFICIENT DE LA THETA-METHODE EN THM
! OUT : CHTIME : NOM DE LA CARTE CREEE
!     ------------------------------------------------------------------
!
    implicit none
!
!     --- ARGUMENTS ---
    include 'asterc/r8nnem.h'
    include 'asterfort/mecact.h'
    real(kind=8) :: inst, deltat, theta
    character(len=*) :: noma
    character(len=24) :: chtime
!
!     --- VARIABLES LOCALES ---
    character(len=6) :: nompro
    parameter (nompro='MECHTI')
!
    integer :: ibid(6)
    real(kind=8) :: tps(6), rundf
    character(len=8) :: k8b, nomcmp(6)
    complex(kind=8) :: c16b
!
    data nomcmp/'INST    ','DELTAT  ','THETA   ','KHI     ',&
     &     'R       ','RHO     '/
! DEB-------------------------------------------------------------------
!
    chtime = '&&'//nompro//'.CH_INST_R'
    tps(1) = inst
    tps(2) = deltat
    tps(3) = theta
!
    rundf = r8nnem()
    tps(4) = rundf
    tps(5) = rundf
    tps(6) = rundf
!
    call mecact('V', chtime, 'MAILLA', noma, 'INST_R',&
                6, nomcmp, ibid, tps, c16b,&
                k8b)
!
end subroutine
