subroutine nmch1p(valinc)
!
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
#include "asterfort/nmcha0.h"
    character(len=19) :: valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DES VARIABLES CHAPEAUX - VALINC
!
! ----------------------------------------------------------------------
!
! OUT VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
!
! ----------------------------------------------------------------------
!
    character(len=19) :: depmoi, sigmoi, varmoi, commoi, vitmoi
    character(len=19) :: accmoi, strmoi, sigext
    character(len=19) :: depplu, sigplu, varplu, complu, vitplu
    character(len=19) :: accplu, strplu
    character(len=19) :: depkm1, vitkm1, acckm1, romkm1, romk
    character(len=19) :: fexmoi, fammoi, flimoi, fnomoi
    character(len=19) :: fexplu, famplu, fliplu, fnoplu
!
    data depmoi,depplu    /'&&NMCH1P.DEPMOI','&&NMCH1P.DEPPLU'/
    data sigmoi,sigplu    /'&&NMCH1P.SIGMOI','&&NMCH1P.SIGPLU'/
    data varmoi,varplu    /'&&NMCH1P.VARMOI','&&NMCH1P.VARPLU'/
    data vitmoi,vitplu    /'&&NMCH1P.VITMOI','&&NMCH1P.VITPLU'/
    data accmoi,accplu    /'&&NMCH1P.ACCMOI','&&NMCH1P.ACCPLU'/
    data commoi,complu    /'&&NMCH1P.COMMOI','&&NMCH1P.COMPLU'/
    data strmoi,strplu    /'&&NMCH1P.STRMOI','&&NMCH1P.STRPLU'/
    data sigext           /'&&NMCH1P.SIGEXT'/
    data fexmoi,fexplu    /'&&NMCH1P.FEXMOI','&&NMCH1P.FEXPLU'/
    data fammoi,famplu    /'&&NMCH1P.FAMMOI','&&NMCH1P.FAMPLU'/
    data flimoi,fliplu    /'&&NMCH1P.FLIMOI','&&NMCH1P.FLIPLU'/
    data fnomoi,fnoplu    /'&&NMCH1P.FNOMOI','&&NMCH1P.FNOPLU'/
    data depkm1,vitkm1    /'&&NMCH1P.DEPKM1','&&NMCH1P.VITKM1'/
    data acckm1           /'&&NMCH1P.ACCKM1'/
    data romkm1,romk      /'&&NMCH1P.ROMKM1','&&NMCH1P.ROMK  '/
!
! ----------------------------------------------------------------------
!
    call nmcha0('VALINC', 'ALLINI', ' ', valinc)
    call nmcha0('VALINC', 'DEPMOI', depmoi, valinc)
    call nmcha0('VALINC', 'VITMOI', vitmoi, valinc)
    call nmcha0('VALINC', 'ACCMOI', accmoi, valinc)
    call nmcha0('VALINC', 'SIGMOI', sigmoi, valinc)
    call nmcha0('VALINC', 'COMMOI', commoi, valinc)
    call nmcha0('VALINC', 'STRMOI', strmoi, valinc)
    call nmcha0('VALINC', 'VARMOI', varmoi, valinc)
    call nmcha0('VALINC', 'DEPPLU', depplu, valinc)
    call nmcha0('VALINC', 'VITPLU', vitplu, valinc)
    call nmcha0('VALINC', 'ACCPLU', accplu, valinc)
    call nmcha0('VALINC', 'SIGPLU', sigplu, valinc)
    call nmcha0('VALINC', 'COMPLU', complu, valinc)
    call nmcha0('VALINC', 'STRPLU', strplu, valinc)
    call nmcha0('VALINC', 'VARPLU', varplu, valinc)
    call nmcha0('VALINC', 'SIGEXT', sigext, valinc)
    call nmcha0('VALINC', 'DEPKM1', depkm1, valinc)
    call nmcha0('VALINC', 'VITKM1', vitkm1, valinc)
    call nmcha0('VALINC', 'ACCKM1', acckm1, valinc)
    call nmcha0('VALINC', 'ROMKM1', romkm1, valinc)
    call nmcha0('VALINC', 'ROMK', romk, valinc)
    call nmcha0('VALINC', 'FEXMOI', fexmoi, valinc)
    call nmcha0('VALINC', 'FAMMOI', fammoi, valinc)
    call nmcha0('VALINC', 'FLIMOI', flimoi, valinc)
    call nmcha0('VALINC', 'FNOMOI', fnomoi, valinc)
    call nmcha0('VALINC', 'FEXPLU', fexplu, valinc)
    call nmcha0('VALINC', 'FAMPLU', famplu, valinc)
    call nmcha0('VALINC', 'FLIPLU', fliplu, valinc)
    call nmcha0('VALINC', 'FNOPLU', fnoplu, valinc)
!
end subroutine
