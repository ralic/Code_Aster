subroutine ircael(jcesdi, jcesli, jcesvi, jcesci, nummai,&
                  nbrcou, nbrsec, nbrfib, nbrgrf, nugrfi)
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/cesexi.h'
    integer :: nmaxfi
    parameter (nmaxfi=10)
    integer :: jcesdi, jcesli, nummai, nbrcou, jcesvi
    integer :: nbrsec, nbrfib, nbrgrf, nugrfi(nmaxfi), jcesci
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
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  IMPR_RESU - CARACTERISTIQUE DE L'ELEMENT
!  -    -      --                   --
! ----------------------------------------------------------------------
!
!  CETTE ROUTINE EST UTILE POUR TROUVER LE NOMBRE DE COUCHES, ...
!    PRESENTES SUR UNE MAILLE
!
! IN  :
!   JCESDI  I    ADRESSE JEVEUX DU CESD
!   JCESLI  I    ADRESSE JEVEUX DU CESL
!   JCESVI  I    ADRESSE JEVEUX DU CESV
!   JCESCI  I    ADRESSE JEVEUX DU CESC
!   NUMMAI  I    NUMERO DE MAILLE
!
! OUT :
!   NBRCOU  I    NOMBRE DE COUCHES
!   NBRSEC  I    NOMBRE DE SECTEURS
!   NBRFIB  I    NOMBRE DE FIBRES
!   NBRGRF  I    NOMBRE DE GROUPES DE FIBRES
!   NUGRFI  I    NUMERO DU GROUPE DE FIBRES
!
    include 'jeveux.h'
!
    integer :: nbrcmp, numgrf, icmp, iad, igfib
!
    do 20, igfib = 1,nmaxfi
    nugrfi(igfib)=0
    20 end do
!
    nbrcmp = zi(jcesdi-1+5+4* (nummai-1)+3)
    numgrf = 1
    do 10, icmp = 1, nbrcmp
    call cesexi('C', jcesdi, jcesli, nummai, 1,&
                1, icmp, iad)
!
    if (iad .gt. 0) then
        if (zk8(jcesci+icmp-1) .eq. 'COQ_NCOU') then
            nbrcou=zi(jcesvi-1+iad)
        else if (zk8(jcesci+icmp-1).eq.'TUY_NCOU') then
            nbrcou=zi(jcesvi-1+iad)
        else if (zk8(jcesci+icmp-1).eq.'TUY_NSEC') then
            nbrsec=zi(jcesvi-1+iad)
        else if (zk8(jcesci+icmp-1).eq.'NBFIBR') then
            nbrfib=zi(jcesvi-1+iad)
        else if (zk8(jcesci+icmp-1).eq.'NBGRFI') then
            nbrgrf=zi(jcesvi-1+iad)
        else if (zk8(jcesci+icmp-1)(1:3).eq.'NUG') then
            nugrfi(numgrf)=zi(jcesvi-1+iad)
            numgrf=numgrf+1
        endif
    endif
    10 end do
    if (nbrfib .ne. 0) call assert(nbrgrf.ne.0)
!
end subroutine
