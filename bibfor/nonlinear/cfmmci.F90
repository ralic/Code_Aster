subroutine cfmmci(defico, resoco)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfmmco.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mminfr.h'
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE POUR LE CONTACT (TOUTES METHODES)
!
! INITIALISE LES COEFFICIENTS VARIABLES
!
! ----------------------------------------------------------------------
!
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    integer :: nzoco, izone
    logical :: lctcd, lctcc, lxfcm
    real(kind=8) :: coefac, coefaf
    real(kind=8) :: coefpn, coefpt
    real(kind=8) :: coefcr, coeffr, coefcp, coeffp
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lctcd = cfdisl(defico,'FORMUL_DISCRETE')
!
! --- INITIALISATIONS
!
    nzoco = cfdisi(defico,'NZOCO')
!
! --- REMPLISSAGE INITIAL
!
    do 10 izone = 1, nzoco
        if (lxfcm) then
            coefcr = mminfr(defico,'COEF_AUGM_CONT' ,izone )
            coeffr = mminfr(defico,'COEF_AUGM_FROT' ,izone )
            coefcp = mminfr(defico,'COEF_PENA_CONT' ,izone )
            coeffp = mminfr(defico,'COEF_PENA_FROT' ,izone )
            call cfmmco(defico, resoco, izone, 'COEF_AUGM_CONT', 'E',&
                        coefcr)
            call cfmmco(defico, resoco, izone, 'COEF_AUGM_FROT', 'E',&
                        coeffr)
            call cfmmco(defico, resoco, izone, 'COEF_PENA_CONT', 'E',&
                        coefcp)
            call cfmmco(defico, resoco, izone, 'COEF_PENA_FROT', 'E',&
                        coeffp)
!
        else if (lctcc) then
            coefac = mminfr(defico,'COEF_AUGM_CONT' ,izone )
            coefaf = mminfr(defico,'COEF_AUGM_FROT' ,izone )
            call cfmmco(defico, resoco, izone, 'COEF_AUGM_CONT', 'E',&
                        coefac)
            call cfmmco(defico, resoco, izone, 'COEF_AUGM_FROT', 'E',&
                        coefaf)
        else if (lctcd) then
            coefpn = mminfr(defico,'E_N' ,izone )
            coefpt = mminfr(defico,'E_T' ,izone )
            call cfmmco(defico, resoco, izone, 'E_N', 'E',&
                        coefpn)
            call cfmmco(defico, resoco, izone, 'E_T', 'E',&
                        coefpt)
        else
            call assert(.false.)
        endif
10  end do
!
    call jedema()
end subroutine
