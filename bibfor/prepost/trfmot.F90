subroutine trfmot(mot, field, ifield)
    implicit  none
    character(len=*) :: mot, field
    integer :: ifield
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!
!     EXTRACTION DU CHAMP IFIELD DE L'ENREGISTREMENT
!
! IN  : MOT    : K80  : ENREGISTREMENT CONTENANT LE CHAMP A TRAITER
! OUT : FIELD  : K80  : CHAMP DE NUMERO IFIELD EXTRAIT DE
!                       L'ENREGISTREMENT
! IN  : IFLIED : I    : NUMERO DU CHAMP RECHERCHE
!
!-----------------------------------------------------------------------
    integer :: nbmot, nbcar, ideb, i, j
!
!- INITIALISATION
!
    do 10 i = 1, 80
        field(i:i) = ' '
10  end do
!
    nbmot = 0
    nbcar = 0
    j = 0
!
!- RECHERCHE DU MOT A TRAITER
!
    do 20 i = 1, 80
        if (mot(i:i) .ne. ' ') then
            nbcar = nbcar + 1
            if (nbcar .eq. 1) ideb = i
        else
            if (nbcar .ne. 0) then
                nbmot = nbmot + 1
                if (nbmot .eq. ifield) goto 30
                nbcar = 0
            endif
        endif
20  end do
!
!-TRANSFERT DU CHAMP IFIELD A TRAITER
!
30  continue
    do 40 i = 1, nbcar
        j = ideb - 1 + i
        if (mot(j:j) .ne. ' ') then
            field(i:i) = mot(j:j)
        else
            goto 50
        endif
40  end do
50  continue
!
end subroutine
