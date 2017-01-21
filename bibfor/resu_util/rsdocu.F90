subroutine rsdocu(docu, resu_type, iret)
!
implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=4), intent(in)  :: docu
    character(len=*), intent(out)  :: resu_type
    integer, intent(out) :: iret
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Get type of results datastructure from DOCU field
!
! --------------------------------------------------------------------------------------------------
!
! In  docu             : field DOCU
! Out resu_type        : type of results datastructure
! Out iret             : return code (0 if OK, 1 else)
!
! --------------------------------------------------------------------------------------------------
!
    iret = 0
    if (docu .eq. 'EVEL') then
        resu_type = 'EVOL_ELAS'
    else if (docu .eq. 'MUEL') then
        resu_type = 'MULT_ELAS'
    else if (docu .eq. 'FOEL') then
        resu_type = 'FOURIER_ELAS'
    else if (docu .eq. 'FOTH') then
        resu_type = 'FOURIER_THER'
    else if (docu .eq. 'COFO') then
        resu_type = 'COMB_FOURIER'
    else if (docu .eq. 'EVNO') then
        resu_type = 'EVOL_NOLI'
    else if (docu .eq. 'EVCH') then
        resu_type = 'EVOL_CHAR'
    else if (docu .eq. 'DYTR') then
        resu_type = 'DYNA_TRANS'
    else if (docu .eq. 'DYHA') then
        resu_type = 'DYNA_HARMO'
    else if (docu .eq. 'HAGE') then
        resu_type = 'HARM_GENE'
    else if (docu .eq. 'ACHA') then
        resu_type = 'ACOU_HARMO'
    else if (docu .eq. 'MOAC') then
        resu_type = 'MODE_ACOU'
    else if (docu .eq. 'MOFL') then
        resu_type = 'MODE_FLAMB'
    else if (docu .eq. 'MOSB') then
        resu_type = 'MODE_STAB'
    else if (docu .eq. 'MOME') then
        resu_type = 'MODE_MECA'
    else if (docu .eq. 'MOGE') then
        resu_type = 'MODE_GENE'
    else if (docu .eq. 'MOEM') then
        resu_type = 'MODE_EMPI'
    else if (docu .eq. 'EVTH') then
        resu_type = 'EVOL_THER'
    else if (docu .eq. 'EVVA') then
        resu_type = 'EVOL_VARC'
    else
        iret = 1
    endif
!
end subroutine
