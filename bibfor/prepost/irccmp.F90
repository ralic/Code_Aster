subroutine irccmp(typ, gd, ncmpmx, nomcgd, nbcmp,&
                  nomcmp, nbcmpt, jcmp)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
#include "jeveux.h"
#include "asterfort/u2mesg.h"
    character(len=*) :: gd, nomcgd(*), nomcmp(*), typ
    integer :: ncmpmx, nbcmp, nbcmpt, jcmp
! ----------------------------------------------------------------------
!     BUT :   TROUVER LE NOMBRE ET LES NOMS DES COMPOSANTES D'UNE LISTE
!             PRESENTENT DANS UNE GRANDEURS
!     ENTREES:
!        GD     : NOM DE LA GRANDEUR
!        NCMPMX : NOMBRE DE COMPOSANTES DE LA GRANDEUR  GD
!        NOMCGD : NOMS DES COMOPOSANTES DE LA GRANDEUR GD
!        NBCMP  : NOMBRE DE COMPOSANTES DE LA LISTE
!        NOMCMP : NOMS DES COMPOSANTES DE LA LISTE
!     SORTIES:
!        NBCMPT : NOMBRE DE COMPOSANTES DE LA LISTE PRESENTES DANS GD
!        JCMP   : ADRESSE OBJET JEVEUX CONTENANT NUMEROS DES COMPOSANTES
! ----------------------------------------------------------------------
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: icm, icmpp
!-----------------------------------------------------------------------
    nbcmpt=0
!
    do 10 icm = 1, nbcmp
        do 11 icmpp = 1, ncmpmx
            if (nomcmp(icm) .eq. nomcgd(icmpp)) then
                nbcmpt=nbcmpt+1
                zi(jcmp-1+nbcmpt) = icmpp
                goto 10
            endif
11      continue
        if (typ(1:1) .ne. ' ') then
            valk (1) = nomcmp(icm)
            valk (2) = gd
            call u2mesg(typ, 'PREPOST5_25', 2, valk, 0,&
                        0, 0, 0.d0)
        endif
10  end do
!
end subroutine
