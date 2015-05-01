subroutine gidoma(nbnoto)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/wkvect.h"
    integer :: nbnoto
!
! ----------------------------------------------------------------------
!     BUT : CREER  L'OBJET &&GILIRE.NUMANEW
!           QUI DONNE LE NOUVEAU NUMERO DE MAILLE DE TOUTES LES
!           MAILLES LUES POUR TENIR COMPTE DES MAILLES IDENTIQUES.
!           (MAILLES EN "DOUBLE" GENEREES PAR LE "ET" GIBI)
!
!     IN  : NBNOTO : NOMBRE TOTAL DE NOEUDS DU MAILLAGE.
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
!
    aster_logical :: iden
!
!
!     -- RECUPERATION DU NOMBRE DE MAILLES TOTAL (AVANT COMPACTAGE):
!-----------------------------------------------------------------------
    integer :: ianema, iawk1, iawk2, iawk3, ico, ilcnx2
    integer :: ima, imaj, imak, ino, ipos, j, k
    integer :: l, nbma, nbmato, nbnoj, nbnok, nuno1, nunoj
    integer :: nunok
    integer, pointer :: connex2(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    call jelira('&&GILIRE.CONNEX2', 'NMAXOC', nbmato)
    call jeveuo('&&GILIRE.CONNEX2', 'L', vi=connex2)
    call jeveuo(jexatr('&&GILIRE.CONNEX2', 'LONCUM'), 'L', ilcnx2)
!
!     -- CREATION DE L'OBJET .NUMANEW QUI CONTIENDRA POUR CHAQ. MAILLE:
!     -- SON NOUVEAU NUMERO (INFERIEUR OU EGAL A L'ANCIEN)
    call wkvect('&&GILIRE.NUMANEW', 'V V I', nbmato, ianema)
!
!     -- CREATION ET REMPLISSAGE DE L'OBJET &&GILIRE.OBJET_WK1
!        CET OBJET INDIQUE POUR CHAQUE NOEUD COMBIEN DE MAILLES
!        ONT UNE CONNECTIVITE QUI COMMENCE PAR CE NOEUD:
    call wkvect('&&GILIRE.OBJET_WK1', 'V V I', nbnoto, iawk1)
    do 20 ima = 1, nbmato
        nuno1= connex2(zi(ilcnx2-1+ima)-1+1 )
        zi(iawk1-1+nuno1) =zi(iawk1-1+nuno1) +1
 20 end do
!
!     -- CREATION ET REMPLISSAGE DE L'OBJET &&GILIRE.OBJET_WK2
!        CET OBJET CONTIENT LA LISTE DES MAILLES QUI COMMENCENT
!        PAR LE MEME NOEUD.
!     -- COMME JEVEUX N'ADMET TOUJOURS PAS D'OBJETS DE LONGUEURS NULLES
!        DANS LES COLLECTIONS, ON CREE AUSSI .OBJET_WK3 QUI SERT DE
!        POINTEUR DE LONGUEURS CUMULEES.
!        WK3(INO) = ADRESSE DANS WK2 DE LA PREMIERE MAILLE QUI COMMENCE
!        PAR LE NOEUD INO. (0 SI AUCUNE MAILLE).
    call wkvect('&&GILIRE.OBJET_WK2', 'V V I', nbmato, iawk2)
    call wkvect('&&GILIRE.OBJET_WK3', 'V V I', nbnoto, iawk3)
!
!     -- CALCUL DE OBJET_WK3:
    ico=1
    do 21 ino = 1, nbnoto
        nbma = zi(iawk1-1+ino)
        if (nbma .eq. 0) goto 21
        zi(iawk3-1+ino) = ico
        ico = ico + nbma
 21 end do
!
!     -- CALCUL DE OBJET_WK2: (ON MODIFIE _WK3 A CHAQUE MAILLE TRAITEE)
    do 22 ima = 1, nbmato
        nuno1= connex2(zi(ilcnx2-1+ima)-1+1 )
        ipos = zi(iawk3-1+nuno1)
        ASSERT(ipos.ne.0)
        ASSERT(zi(iawk2-1+ipos).eq.0)
        zi(iawk2-1+ipos) = ima
        zi(iawk3-1+nuno1) = ipos + 1
 22 end do
!
!     -- ON DECLARE IDENTIQUES 2 MAILLES AYANT MEME CONNECTIVITE:
    ico=0
    do 1 ino = 1, nbnoto
        nbma= zi(iawk1-1+ino)
        if (nbma .eq. 0) goto 1
        do 2 j = 1, nbma
            iden=.false.
            imaj= zi(iawk2-1+ico+j)
            nbnoj=zi(ilcnx2-1+imaj+1)-zi(ilcnx2-1+imaj)
            do 3 k = 1, j-1
                imak= zi(iawk2-1+ico+k)
                nbnok=zi(ilcnx2-1+imak+1)-zi(ilcnx2-1+imak)
                if (nbnoj .ne. nbnok) goto 3
                do 4 l = 1, nbnoj
                    nunoj= connex2(zi(ilcnx2-1+imaj)-1+l )
                    nunok= connex2(zi(ilcnx2-1+imak)-1+l )
                    if (nunoj .ne. nunok) goto 3
                    if (l .eq. nbnoj) then
                        iden=.true.
                        goto 5
                    endif
  4             continue
  3         continue
  5         continue
            if (iden) then
                zi(ianema-1+imaj) =zi(ianema-1+imak)
                goto 2
            else
                zi(ianema-1+imaj) =imaj
            endif
  2     continue
        ico = ico + nbma
  1 end do
!
    call jedema()
end subroutine
