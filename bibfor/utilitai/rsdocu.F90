subroutine rsdocu(docu, repk, iret)
    implicit   none
    integer :: iret
    character(len=4) :: docu
    character(len=*) :: repk
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!
    iret = 0
    if (docu .eq. 'EVEL') then
        repk = 'EVOL_ELAS'
!
    else if (docu .eq. 'MUEL') then
        repk = 'MULT_ELAS'
!
    else if (docu .eq. 'FOEL') then
        repk = 'FOURIER_ELAS'
!
    else if (docu .eq. 'FOTH') then
        repk = 'FOURIER_THER'
!
    else if (docu .eq. 'COFO') then
        repk = 'COMB_FOURIER'
!
    else if (docu .eq. 'EVNO') then
        repk = 'EVOL_NOLI'
!
    else if (docu .eq. 'EVCH') then
        repk = 'EVOL_CHAR'
!
    else if (docu .eq. 'DYTR') then
        repk = 'DYNA_TRANS'
!
    else if (docu .eq. 'DYHA') then
        repk = 'DYNA_HARMO'
!
    else if (docu .eq. 'HAGE') then
        repk = 'HARM_GENE'
!
    else if (docu .eq. 'ACHA') then
        repk = 'ACOU_HARMO'
!
    else if (docu .eq. 'MOAC') then
        repk = 'MODE_ACOU'
!
    else if (docu .eq. 'MOFL') then
        repk = 'MODE_FLAMB'
!
    else if (docu .eq. 'MOSB') then
        repk = 'MODE_STAB'
!
    else if (docu .eq. 'MOME') then
        repk = 'MODE_MECA'
!
    else if (docu .eq. 'MOGE') then
        repk = 'MODE_GENE'
!
    else if (docu .eq. 'EVTH') then
        repk = 'EVOL_THER'
!
    else if (docu .eq. 'EVVA') then
        repk = 'EVOL_VARC'
!
    else if (docu .eq. 'THET') then
        repk = 'THETA_GEOM'
!
    else
        iret = 1
    endif
!
end subroutine
