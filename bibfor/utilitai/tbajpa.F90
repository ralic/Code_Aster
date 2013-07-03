subroutine tbajpa(nomta, nbpar, nompar, typpar)
    implicit   none
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/u2mess.h"
    integer :: nbpar
    character(len=*) :: nomta, nompar(*), typpar(*)
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
!      AJOUTER DES PARAMETRES A UNE TABLE.
! ----------------------------------------------------------------------
! IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
! IN  : NBPAR  : NOMBRE DE PARAMETRES.
! IN  : NOMPAR : NOMS DES PARAMETRES.
! IN  : TYPPAR : TYPES DES PARAMETRES.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: iret, nbpara, nblign, jtbba, jtbnp, nbpm, nbpu
    integer :: ndim, jtblp, i, j, k, ideb, jnjv, nbpar1
    character(len=1) :: base
    character(len=3) :: type
    character(len=4) :: knume
    character(len=8) :: k8b
    character(len=19) :: nomtab
    character(len=24) :: nomjv, inpar, jnpar
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = ' '
    nomtab = nomta
    call jeexin(nomtab//'.TBBA', iret)
    if (iret .eq. 0) then
        call u2mess('F', 'UTILITAI4_64')
    endif
    if (nomtab(18:19) .ne. '  ') then
        call u2mess('F', 'UTILITAI4_68')
    endif
!
    call jeveuo(nomtab//'.TBBA', 'L', jtbba)
    base = zk8(jtbba)(1:1)
!
    call jeveuo(nomtab//'.TBNP', 'E', jtbnp)
    nbpara = zi(jtbnp )
    nblign = max ( zi(jtbnp+1) , 10 )
!
! ----------------------------------------------------------------------
!
!                   --- ON INITIALISE LA TABLE ---
!
    if (nbpara .eq. 0) then
!
        zi(jtbnp) = nbpar
        ndim = 4 * nbpar
!
        call jecreo(nomtab//'.TBLP', base//' V K24')
        call jeecra(nomtab//'.TBLP', 'LONMAX', ndim, ' ')
        call jeecra(nomtab//'.TBLP', 'LONUTI', ndim, ' ')
        call jeveuo(nomtab//'.TBLP', 'E', jtblp)
!
        do 10 i = 1, nbpar
            zk24(jtblp+4*(i-1) ) = nompar(i)
            zk24(jtblp+4*(i-1)+1) = typpar(i)
            call codent(i, 'D0', knume)
            nomjv = nomtab//'.'//knume
            type = '   '
            type = typpar(i)
            call jecreo(nomjv, base//' V '//type)
            call jeecra(nomjv, 'LONMAX', nblign, ' ')
            call jeecra(nomjv, 'LONUTI', 0, ' ')
            call jeveuo(nomjv, 'E', iret)
            zk24(jtblp+4*(i-1)+2) = nomjv
            nomjv = nomtab(1:17)//'LG.'//knume
            call jecreo(nomjv, base//' V I')
            call jeecra(nomjv, 'LONMAX', nblign, ' ')
            call jeveuo(nomjv, 'E', jnjv)
            do 12 j = 1, nblign
                zi(jnjv+j-1) = 0
12          continue
            zk24(jtblp+4*(i-1)+3) = nomjv
10      continue
!
! ----------------------------------------------------------------------
!
!               --- ON AJOUTE DES PARAMETRES A LA TABLE ---
!
    else
!
        call jelira(nomtab//'.TBLP', 'LONMAX', nbpm, k8b)
        call jelira(nomtab//'.TBLP', 'LONUTI', nbpu, k8b)
        call jeveuo(nomtab//'.TBLP', 'L', jtblp)
!
!        IL FAUT INITIALISER LES COLONNES AU LONMAX ET NON PAS A NBLIGN
!        QUI EST LE NOMBRE DE LIGNES EVENTUELLEMENT REMPLI
!        ON RECUPERE LE PREMIER PARAMETRE DE LA TABLE
!
        j = 1
        call codent(j, 'D0', knume)
        nomjv = nomtab(1:17)//'LG.'//knume
        call jelira(nomjv, 'LONMAX', nblign, k8b)
!
!        --- ON VERIFIE QUE LES PARAMETRES N'EXISTENT PAS ---
        nbpar1 = 0
        do 20 i = 1, nbpar
            inpar = nompar(i)
            do 22 j = 1, nbpara
                jnpar = zk24(jtblp+4*(j-1))
                if (inpar .eq. jnpar) goto 20
22          continue
            nbpar1 = nbpar1 + 1
20      continue
        if (nbpar1 .eq. 0) goto 9999
!
        ideb = nbpara
        nbpara = nbpara + nbpar1
        zi(jtbnp) = nbpara
        ndim = 4*nbpara
!
        if (ndim .gt. nbpm) then
            call juveca(nomtab//'.TBLP', ndim)
        endif
        call jeecra(nomtab//'.TBLP', 'LONUTI', ndim, ' ')
        call jeveuo(nomtab//'.TBLP', 'E', jtblp)
        do 30 i = 1, nbpar
            inpar = nompar(i)
            do 32 j = 1, nbpara
                jnpar = zk24(jtblp+4*(j-1))
                if (inpar .eq. jnpar) goto 30
32          continue
            ideb = ideb + 1
            j = ideb
            zk24(jtblp+4*(j-1) ) = nompar(i)
            zk24(jtblp+4*(j-1)+1) = typpar(i)
            call codent(j, 'D0', knume)
            nomjv = nomtab//'.'//knume
            type = '   '
            type = typpar(i)
            call jecreo(nomjv, base//' V '//type)
            call jeecra(nomjv, 'LONMAX', nblign, ' ')
            call jeecra(nomjv, 'LONUTI', 0, ' ')
            call jeveuo(nomjv, 'E', iret)
            zk24(jtblp+4*(j-1)+2) = nomjv
            nomjv = nomtab(1:17)//'LG.'//knume
            call jecreo(nomjv, base//' V I')
            call jeecra(nomjv, 'LONMAX', nblign, ' ')
            call jeveuo(nomjv, 'E', jnjv)
            do 34 k = 1, nblign
                zi(jnjv+k-1) = 0
34          continue
            zk24(jtblp+4*(j-1)+3) = nomjv
30      continue
!
    endif
9999  continue
!
    call jedema()
end subroutine
