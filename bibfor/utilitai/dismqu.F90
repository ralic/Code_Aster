subroutine dismqu(questi, nomobz, repi, repkz, ierd)
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
!     --     DISMOI(DEGRE ELEMENTS 3D)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=32) :: repk
    character(len=19) :: nomob
    character(len=*) :: repkz, nomobz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE LIGREL
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=16) :: nomte
    character(len=1) :: k1bid
!
!
!
!-----------------------------------------------------------------------
    integer :: iagrel, igr, iret, ite, n1, nbgr, nbnon
    integer :: nboui
!-----------------------------------------------------------------------
    call jemarq()
    nomob = nomobz
    repk = ' '
    repi=0
    ierd = 0
!
    if (questi .eq. 'ELEM_VOLU_QUAD') then
        nboui=0
        nbnon=0
        call jeexin(nomob//'.LIEL', iret)
        ierd=1
        if (iret .gt. 0) then
            call jelira(nomob//'.LIEL', 'NUTIOC', nbgr, k1bid)
            do 1, igr=1,nbgr
            call jeveuo(jexnum(nomob//'.LIEL', igr), 'L', iagrel)
            call jelira(jexnum(nomob//'.LIEL', igr), 'LONMAX', n1, k1bid)
            ite=zi(iagrel-1+n1)
            call jenuno(jexnum('&CATA.TE.NOMTE', ite), nomte)
            if (nomte .eq. 'MECA_HEXA20' .or. nomte .eq. 'MECA_HEXA27' .or. nomte .eq.&
                'MECA_PENTA15' .or. nomte .eq. 'MECA_TETRA10' .or. nomte .eq.&
                'MECA_PYRAM13' .or. nomte .eq. 'MECA_HEXS20' .or. nomte .eq. 'MECA_PENTA18') then
                repk = 'OUI'
                nboui=nboui+1
                else if(nomte.eq.'MECA_HEXA8' .or.&
     &          nomte.eq.'MECA_PENTA6'.or.nomte.eq.'MECA_TETRA4'.or.&
     &          nomte.eq.'MECA_PYRAM5') then
                repk = 'NON'
                nbnon=nbnon+1
            endif
            ierd=0
 1          continue
        endif
        if (nboui .ne. 0 .and. nbnon .ne. 0) repk='MEL'
    else
        ierd=1
    endif
!
!
    repkz = repk
    call jedema()
end subroutine
