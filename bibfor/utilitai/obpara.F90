subroutine obpara(nomstr, nompaz, indice, typpar)
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
    include      'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: nomstr
    integer :: indice
    character(len=1) :: typpar
    character(len=*) :: nompaz
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT)
!
! REPERAGE INDICE PARAMETRE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMSTR : NOM DU STRUCT
! IN  NOMPAR : NOM DU PARAMETRE
! OUT INDICE : INDICE CORRESPONDANT AU PARAMETRE DANS LE VECTEUR TYPVAL
! OUT TYPPAR : TYPE PARAMETRE
!               'B' - BOOLEEN
!               'I' - ENTIER
!               'K' - REEL
!               'C' - CHAINE
!               'O' - OBJET
!
! ----------------------------------------------------------------------
!
    character(len=24) :: sddesc, sdpara
    integer :: jsdesc, jspara
    integer :: nbvalb, nbvali, nbvalr, nbvalk, nbvalo, nbpara
    integer :: ipara, indabs
    character(len=24) :: nompar, paralu
!
! ----------------------------------------------------------------------
!
    indice = 0
    indabs = 0
    nompar = nompaz
    typpar = ' '
!
! --- ACCES
!
    sddesc = nomstr(1:19)//'.DESC'
    sdpara = nomstr(1:19)//'.PARA'
    call jeveuo(sddesc, 'L', jsdesc)
    call jeveuo(sdpara, 'L', jspara)
!
! --- DESCRIPTEURS
!
    nbvalb = zi(jsdesc-1+1)
    nbvali = zi(jsdesc-1+2)
    nbvalr = zi(jsdesc-1+3)
    nbvalk = zi(jsdesc-1+4)
    nbvalo = zi(jsdesc-1+5)
    nbpara = nbvali+nbvalr+nbvalk+nbvalo+nbvalb
!
! --- REPERAGE INDICE ABSOLU
!
    do 10 ipara = 1, nbpara
        paralu = zk24(jspara-1+ipara)
        if (paralu .eq. nompar) then
            indabs = ipara
            goto 15
        endif
10  end do
15  continue
!
! --- ERREUR
!
    if (indabs .eq. 0) then
        write(6,*) 'PARAMETRE INCONNU: ',nompar
        write(6,*) 'DANS SD: ',nomstr
        call assert(.false.)
    endif
!
! --- INDICE DANS TYPE DE PARAMETRE
!
    if (indabs .gt. nbvalb+nbvali+nbvalr+nbvalk) then
        indice = indabs - (nbvalb+nbvali+nbvalr+nbvalk)
        typpar = 'O'
    else if (indabs.gt.nbvalb+nbvali+nbvalr) then
        indice = indabs - (nbvalb+nbvali+nbvalr)
        typpar = 'K'
    else if (indabs.gt.nbvalb+nbvali) then
        indice = indabs - (nbvalb+nbvali)
        typpar = 'R'
    else if (indabs.gt.nbvalb) then
        indice = indabs - (nbvalb)
        typpar = 'I'
    else
        indice = indabs
        typpar = 'B'
    endif
!
    call assert(indice.gt.0)
end subroutine
