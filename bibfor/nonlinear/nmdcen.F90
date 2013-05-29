subroutine nmdcen(sddisc, numins, nbini, nbins)
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
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juveca.h'
    character(len=19) :: sddisc
    integer :: nbins, numins, nbini
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - DECOUPE)
!
! EXTENSION DE LA LISTE DES NIVEAUX DE DECOUPAGE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  NUMINS : NUMERO D'INSTANT
! IN  NBINI  : NOMBRE DE PAS DE TEMPS INITIAL
! IN  NBINS  : NOMBRE DE PAS DE TEMPS A INSERER
!
! ----------------------------------------------------------------------
!
    integer :: ipas, nbnew
    character(len=24) :: tpsdin
    integer :: jnivtp
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    tpsdin = sddisc(1:19)//'.DINI'
!
! --- NOUVEAU NOMBRE D'INSTANTS
!
    nbnew = nbini + nbins
!
! --- ALLONGEMENT DE LA LISTE DES NIVEAUX
!
    call juveca(tpsdin, nbnew)
    call jeveuo(tpsdin, 'E', jnivtp)
!
! --- RECOPIE DE LA PARTIE HAUTE DE LA LISTE
!
    do 10 ipas = nbnew, numins+nbins, -1
        zi(jnivtp-1+ipas) = zi(jnivtp-1+ipas-nbins)
10  end do
!
! --- INCREMENTATION DU NIVEAU SUR LA PARTIE DECOUPEE
!
    do 20 ipas = numins, numins+nbins-1
        zi(jnivtp-1+ipas) = zi(jnivtp-1+ipas)+1
20  end do
!
    call jedema()
end subroutine
