subroutine irvcmp(ncmpmx, nomcgd, nomcmp, nbcmpt)
    implicit   none
    integer :: ncmpmx, nbcmpt
    character(len=*) :: nomcgd(*), nomcmp
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     BUT :   TROUVER SI UNE COMPOSANTE EST PRESENTE DANS LA GRANDEUR
!     ENTREES:
!        NCMPMX : NOMBRE DE COMPOSANTES DE LA GRANDEUR
!        NOMCGD : NOMS DES COMPOSANTES DE LA GRANDEUR
!        NOMCMP : NOM D'UNE COMPOSANTE
!     SORTIES:
!        NBCMPT : COMPOSANTE PRESENTE DANS LA GRANDEUR
! ----------------------------------------------------------------------
    integer :: icmp
!
    do 10 icmp = 1, ncmpmx
        if (nomcmp .eq. nomcgd(icmp)) then
            nbcmpt=nbcmpt+1
            goto 12
        endif
10  end do
12  continue
!
end subroutine
