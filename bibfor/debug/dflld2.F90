subroutine dflld2(sdlist, ifm, iechec)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfllvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: sdlist
    integer :: ifm, iechec
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! IMPRESSION DEBUG - OPTIONS DE DECOUPE
!
! ----------------------------------------------------------------------
!
! IN  SDLIST : NOM DE LA SD RESULTAT
! IN  IFM    : UNITE LOGIQUE AFFICHAGE
!
!
!
!
    character(len=24) :: lisifr
    integer :: jlinr
    integer :: lesur
    character(len=24) :: lisesu
    integer :: jesur
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- TAILLE DES VECTEURS
!
    lesur = dfllvd('LESUR')
!
! --- ACCES SDS
!
    lisifr = sdlist(1:8)//'.LIST.INFOR'
    call jeveuo(lisifr, 'L', jlinr)
    lisesu = sdlist(1:8)//'.ECHE.SUBDR'
    call jeveuo(lisesu, 'L', jesur)
!
! --- AFFICHAGE
!
!
    if (zr(jesur-1+lesur*(iechec-1)+1) .eq. 1.d0) then
        write(ifm,*) '<DEFILISTINST> ......... MANUEL'
        write(ifm,*) '<DEFILISTINST> ............ NBRE DE'//&
     &             ' SUBDIVISIONS DEMANDEES',&
     &             nint(zr(jesur-1+lesur*(iechec-1)+2))
        if (nint(zr(jesur-1+lesur*(iechec-1)+4)) .eq. 0) then
            write(ifm,*) '<DEFILISTINST> ............ ARRET'//&
            ' DE LA SUBDIVISION QUAND LE PAS '// ' VAUT MOINS DE : ',&
            zr(jesur-1+lesur*(iechec-1)+3)
        else
            write(ifm,*) '<DEFILISTINST> ............ ARRET'//&
     &                 ' DE LA SUBDIVISION QUAND LE NIVEAU'//&
     &                 ' DE SUBDIVISION VAUT: ',&
     &                 nint(zr(jesur-1+lesur*(iechec-1)+4))
        endif
    else if (zr(jesur-1+lesur*(iechec-1)+1).eq.2.d0) then
        write(ifm,*) '<DEFILISTINST> ......... AUTOMATIQUE'
!
        if (zr(jesur-1+lesur*(iechec-1)+10) .eq. 2.d0) then
            write(ifm,*) '<DEFILISTINST> ............ EXTRAPOLATION '
        else if (zr(jesur-1+lesur*(iechec-1)+10).eq.1.d0) then
            write(ifm,*) '<DEFILISTINST> ............ COLLISION '
            write(ifm,*) '<DEFILISTINST> ............... DELTAT '//&
            ' DE LA COLLISION : ', zr(jesur-1+lesur*(iechec-1)+5)
            write(ifm,*) '<DEFILISTINST> ............... DUREE '//&
            ' DE LA DECOUPE : ', zr(jesur-1+lesur*(iechec-1)+6)
        else
            ASSERT(.false.)
        endif
    endif
!
    call jedema()
end subroutine
