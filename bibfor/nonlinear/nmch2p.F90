subroutine nmch2p(solalg)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'asterfort/nmcha0.h'
    character(len=19) :: solalg(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DES VARIABLES CHAPEAUX - SOLALG
!
! ----------------------------------------------------------------------
!
!
! OUT SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! ----------------------------------------------------------------------
!
    character(len=19) :: depold, ddepla, depdel, deppr1, deppr2
    character(len=19) :: vitold, dvitla, vitdel, vitpr1, vitpr2
    character(len=19) :: accold, daccla, accdel, accpr1, accpr2
    character(len=19) :: depso1, depso2
!
    data depdel,ddepla    /'&&NMCH2P.DEPDEL','&&NMCH2P.DDEPLA'/
    data depold           /'&&NMCH2P.DEPOLD'/
    data deppr1,deppr2    /'&&NMCH2P.DEPPR1','&&NMCH2P.DEPPR2'/
    data vitdel,dvitla    /'&&NMCH2P.VITDEL','&&NMCH2P.DVITLA'/
    data vitold           /'&&NMCH2P.VITOLD'/
    data vitpr1,vitpr2    /'&&NMCH2P.VITPR1','&&NMCH2P.VITPR2'/
    data accdel,daccla    /'&&NMCH2P.ACCDEL','&&NMCH2P.DACCLA'/
    data accold           /'&&NMCH2P.ACCOLD'/
    data accpr1,accpr2    /'&&NMCH2P.ACCPR1','&&NMCH2P.ACCPR2'/
    data depso1,depso2    /'&&NMCH2P.DEPSO1','&&NMCH2P.DEPSO2'/
!
! ----------------------------------------------------------------------
!
    call nmcha0('SOLALG', 'ALLINI', ' ', solalg)
    call nmcha0('SOLALG', 'DDEPLA', ddepla, solalg)
    call nmcha0('SOLALG', 'DEPDEL', depdel, solalg)
    call nmcha0('SOLALG', 'DEPPR1', deppr1, solalg)
    call nmcha0('SOLALG', 'DEPPR2', deppr2, solalg)
    call nmcha0('SOLALG', 'DEPOLD', depold, solalg)
    call nmcha0('SOLALG', 'DVITLA', dvitla, solalg)
    call nmcha0('SOLALG', 'VITDEL', vitdel, solalg)
    call nmcha0('SOLALG', 'VITPR1', vitpr1, solalg)
    call nmcha0('SOLALG', 'VITPR2', vitpr2, solalg)
    call nmcha0('SOLALG', 'VITOLD', vitold, solalg)
    call nmcha0('SOLALG', 'DACCLA', daccla, solalg)
    call nmcha0('SOLALG', 'ACCDEL', accdel, solalg)
    call nmcha0('SOLALG', 'ACCPR1', accpr1, solalg)
    call nmcha0('SOLALG', 'ACCPR2', accpr2, solalg)
    call nmcha0('SOLALG', 'ACCOLD', accold, solalg)
    call nmcha0('SOLALG', 'DEPSO1', depso1, solalg)
    call nmcha0('SOLALG', 'DEPSO2', depso2, solalg)
!
end subroutine
