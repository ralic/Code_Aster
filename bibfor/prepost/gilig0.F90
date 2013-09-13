subroutine gilig0(nfic, nboblu, nbobno, nbobo, niv)
    implicit none
#include "jeveux.h"
#include "asterfort/gilio2.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nfic, nboblu, nbobno, nbobo, niv
! ----------------------------------------------------------------------
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
!
!     BUT: LIRE LES N LIGNES DES POINTS DU MAILLAGE GIBI :
!                 ( PROCEDURE SAUVER)
!
!     IN: NFIC   : UNITE DE LECTURE
!
! ----------------------------------------------------------------------
!
    integer :: iaobno, iaobnu, nbfois, nbrest, icoj, i, j, ianoob, iadsob
    integer :: iacuel, nbnom, nbnum, nbele
!     ------------------------------------------------------------------
!
    call jemarq()
!
    if (niv .eq. 3) then
        nbnom = 8
        nbnum = 16
    else
        nbnom = 8
        nbnum = 10
    endif
    nbobo = nboblu
    if (nbobo .gt. 99999) then
        call utmess('F', 'PREPOST_57')
    endif
!
!     --- ON LIT LES OBJETS NOMMES:
!
    call wkvect('&&GILIRE.OBJET_NOM', 'V V K8', nbobno, iaobno)
    call wkvect('&&GILIRE.OBJET_NUM', 'V V I', nbobno, iaobnu)
!
    nbfois = nbobno / nbnom
    nbrest = nbobno - nbnom*nbfois
    icoj = 0
    do 10 i = 1, nbfois
        read(nfic,1007) (zk8(iaobno-1+j),j=icoj+1,icoj+nbnom)
        icoj = icoj + nbnom
10  end do
    if (nbrest .gt. 0) then
        read(nfic,1007) (zk8(iaobno-1+j),j=icoj+1,icoj+nbrest)
    endif
!
    nbfois = nbobno / nbnum
    nbrest = nbobno - nbnum*nbfois
    icoj = 0
    do 12 i = 1, nbfois
        if (niv .eq. 3) then
            read(nfic,1009) (zi(iaobnu-1+j),j=icoj+1,icoj+nbnum)
        else
            read(nfic,1008) (zi(iaobnu-1+j),j=icoj+1,icoj+nbnum)
        endif
        icoj = icoj + nbnum
12  end do
    if (nbrest .gt. 0) then
        if (niv .eq. 3) then
            read(nfic,1009) (zi(iaobnu-1+j),j=icoj+1,icoj+nbrest)
        else
            read(nfic,1008) (zi(iaobnu-1+j),j=icoj+1,icoj+nbrest)
        endif
    endif
!
!     -- ON CREE L'OBJET .NOMOBJ QUI CONTIENDRA 2 K8 POUR CHAQUE OBJET:
!        1--> 'OBJNPQ' : SUFFIXE POUR LES OBJETS JEVEUX CREES DS GILIOB
!        2--> '      ' (OU TYPE_MAILLE ( SEG2,CUB8,...) )
!
    call wkvect('&&GILIRE.NOMOBJ', 'V V K8', 2*nbobo, ianoob)
!
!     -- ON CREE L'OBJET .DESCOBJ
!        QUI CONTIENDRA 4 ENTIER POUR CHAQUE OBJET:
!        1--> NBSOOB : NOMBRE DE SOUS-OBJETS DE L'OBJET.
!        2--> NBREF  : NOMBRE DE REFERENCES.
!        3--> NBNO   : NOMBRE DE NOEUD DU TYPE_MAILLE.
!        4--> NBELEM : NOMBRE DE MAILLES DANS L'OGJET.
!
    call wkvect('&&GILIRE.DESCOBJ', 'V V I', 4*nbobo, iadsob)
!
!     -- ON CREE L'OBJET .CUMUL_ELE
!        QUI CONTIENDRA 1 ENTIER POUR CHAQUE OBJET:
!        1--> ICUEL  : NOMBRE CUMULE DES MAILLES CONTENUES DANS LES
!             OBJETS DE NUMEROS INFERIEURS A IOBJ.
!
    call wkvect('&&GILIRE.CUMUL_ELE', 'V V I', nbobo+1, iacuel)
!
!
    zi(iacuel-1+1) = 0
    do 20 i = 1, nbobo
        call gilio2(nfic, i, nbele, niv)
        zi(iacuel-1+i+1) = zi(iacuel-1+i) + nbele
20  end do
!
    1007 format ( 8(1x,a8) )
    1008 format ( 1x,10(i7,1x) )
    1009 format ( 16(1x,i4) )
!
    call jedema()
!
end subroutine
