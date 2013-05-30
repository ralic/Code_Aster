subroutine impcom(inoda, nomddl, chaine)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8) :: nomddl
    integer :: inoda
    character(len=16) :: chaine
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (AFFICHAGE - UTILITAIRE)
!
! RETOURNE UNE CHAINE FORMATEE K16 POUR LES INFOS SUR UNE COMPOSANTE
! POUR LE CAS D UN RESIDU RESI_COMP_RELA
!
! ----------------------------------------------------------------------
!
!
! IN  INODA  : NUMERO DU NOEUD
! IN  NOMDDL : CHAINE DU COMPOSANT
! OUT CHAINE : CHAINE DU NOM DU NOEUD OU 'LIAISON_DDL'
!              CHAINE DU NOM DE LA CMP OU NOM DU LIGREL DE CHARGE
!
!
! ----------------------------------------------------------------------
!
    character(len=8) :: chnod
    integer :: i, k
    integer :: nchain
    parameter    (nchain = 7)
!
! ----------------------------------------------------------------------
!
    chaine = ' '
    chnod = '  '
    if (inoda .eq. 0) goto 99
!
    write(chnod(1:nchain),'(I7.7)') inoda
    k = 1
!
    do 10 i = 1, nchain
        if (chnod(i:i) .eq. '0') then
            k=k+1
        else
            chaine(1:1) = 'N'
            goto 20
        endif
10  end do
20  continue
!
    chaine(2:nchain-k+2) = chnod(k:nchain)
!
    chaine(9:16) = nomddl
99  continue
end subroutine
