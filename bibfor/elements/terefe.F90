subroutine terefe(nomref, typele, valref)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterc/iisnan.h'
    include 'asterc/r8nnem.h'
    include 'asterfort/assert.h'
    include 'asterfort/jevech.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomref, typele
    real(kind=8) :: valref
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE POUR ACCES AUX VALEURS *_REFE POUR L'OPTION
! REFE_FORC_NODA
!
! ----------------------------------------------------------------------
!
! IN  NOMREF : NOM DE LA COMPOSANTE DE REFERENCE
! IN  TYPELE : TYPE D'ELEMENT
!           'MECA_ISO' - MECANIQUE ISOPARAMETRIQUE (2D ET 3D)
! OUT VALREF : VALEUR DE REFERENCE
!
!
!
!
    integer :: jvrefe
    integer :: index
    real(kind=8) :: val
    character(len=16) :: kmess(2)
!
! ----------------------------------------------------------------------
!
    call jevech('PREFCO', 'L', jvrefe)
    valref = r8nnem()
    if (nomref .eq. 'SIGM_REFE') then
        if (typele .eq. 'MECA_ISO') then
            index = 1
        else if (typele.eq.'THM_JOINT') then
            index = 1
        else if (typele.eq.'MECA_INTERFACE') then
            index = 1
        else if (typele.eq.'MECA_COQUE3D') then
            index = 1
        else if (typele.eq.'MECA_GRADVARI') then
            index = 1
        else if (typele.eq.'MECA_TUYAU') then
            index = 1
        else if (typele.eq.'THM') then
            index = 1
        else if (typele.eq.'MECA_INCO') then
            index = 1
        else
            call assert(.false.)
        endif
    else if (nomref.eq.'EPSI_REFE') then
        if (typele .eq. 'MECA_INCO') then
            index = 2
        else if (typele.eq.'GRILLE') then
            index = 1
        else if (typele.eq.'MEMBRANE') then
            index = 1
        else
            call assert(.false.)
        endif
    else if (nomref.eq.'FLUX_THER_REFE') then
        if (typele .eq. 'THM') then
            index = 4
        else
            call assert(.false.)
        endif
    else if (nomref.eq.'FLUX_HYD1_REFE') then
        if (typele .eq. 'THM_JOINT') then
            index = 2
        else if (typele.eq.'THM') then
            index = 2
        else
            call assert(.false.)
        endif
    else if (nomref.eq.'FLUX_HYD2_REFE') then
        if (typele .eq. 'THM') then
            index = 3
        else
            call assert(.false.)
        endif
    else if (nomref.eq.'VARI_REFE') then
        if (typele .eq. 'MECA_GRADVARI') then
            index = 2
        else
            call assert(.false.)
        endif
    else if (nomref.eq.'EFFORT_REFE') then
        if (typele .eq. 'MECA_DISCRET') then
            index = 1
        else if (typele.eq.'MECA_BARRE') then
            index = 1
        else if (typele.eq.'MECA_CABLE') then
            index = 1
        else if (typele.eq.'MECA_POULIE') then
            index = 1
        else if (typele.eq.'MECA_POUTRE') then
            index = 1
        else
            call assert(.false.)
        endif
    else if (nomref.eq.'MOMENT_REFE') then
        if (typele .eq. 'MECA_DISCRET') then
            index = 2
        else if (typele.eq.'MECA_POUTRE') then
            index = 2
        else
            call assert(.false.)
        endif
    else if (nomref.eq.'DEPL_REFE') then
        if (typele .eq. 'MECA_INTERFACE') then
            index = 2
        else
            call assert(.false.)
        endif
    else if (nomref.eq.'LAGR_REFE') then
        if (typele .eq. 'MECA_GRADVARI') then
            index = 3
        else
            call assert(.false.)
        endif
    else
        call assert(.false.)
    endif
!
    val = zr(jvrefe+index-1)
    if (iisnan(val) .eq. 1) then
        if ((nomref.eq.'EFFORT_REFE') .or. (nomref.eq.'MOMENT_REFE')) then
            kmess(2) = 'FORC_REFE'
        else
            kmess(2) = nomref
        endif
        kmess(1) = typele
        call u2mesk('F', 'MECANONLINE5_55', 2, kmess)
    else
        valref = val
    endif
!
end subroutine
