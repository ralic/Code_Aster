subroutine mmnumn(noma, typint, nummae, nnomae, iptm,&
                  numnoe)
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
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    character(len=8) :: noma
    integer :: typint
    integer :: nummae, iptm, numnoe, nnomae
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! NUMERO ABSOLU DU POINT DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  TYPINT : TYPE D'INTEGRATION
! IN  NUMMAE : NUMERO ABSOLU DE LA MAILLE ESCLAVE
! IN  NNOMAE : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  IPTM   : NUMERO DU POINT D'INTEGRATION DANS LA MAILLE
! OUT NUMNOE : NUMERO ABSOLU DU POINT DE CONTACT
!                  VAUT -1 SI LE POINT N'EST PAS UN NOEUD
!
!
!
!
!
    integer :: inoe, jconnx
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    numnoe = -1
!
! --- ACCES MAILLAGE
!
    call jeveuo(jexnum(noma//'.CONNEX', nummae), 'L', jconnx)
!
! --- NUMERO ABSOLU DU NOEUD
!
    if (typint .eq. 1) then
        if (iptm .gt. nnomae) then
            numnoe = -1
        else
            inoe = iptm
            numnoe = zi(jconnx+inoe-1)
        endif
    else
        numnoe = -1
    endif
!
    call jedema()
end subroutine
