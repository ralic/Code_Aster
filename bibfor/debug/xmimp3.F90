subroutine xmimp3(ifm, noma, itpc, jvalv, jtabf)
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
    implicit     none
    include 'jeveux.h'
!
    include 'asterfort/cfmmvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jexnum.h'
    integer :: ifm
    character(len=8) :: noma
    integer :: itpc
    integer :: jvalv, jtabf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEM - UTILITAIRE - IMPRESSIONS)
!
! AFFICHAGE DE LA CARTE DES ELEMENTS DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  ITPC   : NUMERO DU POINT DE CONTACT SUR TOUTE LA SURFACE
! IN  JVALV  : POINTEUR VERS LE CHAM_ELEM
! IN  JTABF  : POINTEUR VERS DEFICO(1:16)//'.CARACF'
!
!
!
!
    integer :: ztabf
    integer :: nummae, nummam
    character(len=8) :: nomesc, nommai
    real(kind=8) :: coefff, lambda
    real(kind=8) :: coefcr, coeffr
    integer :: ifrott
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    ztabf = cfmmvd('ZTABF')
!
    lambda = zr(jvalv-1+12)
    coefcr = zr(jvalv-1+13)
    coeffr = zr(jvalv-1+14)
    coefff = zr(jvalv-1+15)
    ifrott = nint(zr(jvalv-1+16))
!
! --- ACCES A L'ELEMENT EN COURS
!
    nummae = nint(zr(jtabf+ztabf*(itpc-1)+1))
    call jenuno(jexnum(noma//'.NOMMAI', nummae), nomesc)
    nummam = nint(zr(jtabf+ztabf*(itpc-1)+2))
    call jenuno(jexnum(noma//'.NOMMAI', nummam), nommai)
    write(ifm,1000) itpc,nomesc,nommai
!
! --- POINT DE CONTACT EN COURS
!
    write(ifm,1001)
    write(ifm,1002) lambda,coefcr
    if (ifrott .eq. 3) then
        write(ifm,1006) coefff,coeffr
    endif
!
! --- FORMATS AFFICHAGE
!
    1000 format (' <CONTACT>     * LA MAILLE DE CONTACT ',i5,&
     &        '(',a8,'/',a8,')')
    1001 format (' <CONTACT>        A POUR PROPRIETES')
!
    1002 format (' <CONTACT>          - LAMBDA         : ',e10.3,&
     &        ' - COEF_REGU_CONT :  ',e10.3)
!
    1006 format (' <CONTACT>          AVEC FROTTEMENT DE COULOMB',&
     &        ' - COEFFICIENT    :  ',e10.3,&
     &        ' - COEF_REGU_FROT :  ',e10.3)
!
    call jedema()
!
end subroutine
