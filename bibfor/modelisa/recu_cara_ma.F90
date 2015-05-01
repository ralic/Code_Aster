subroutine recu_cara_ma(mailla, carte, numa, cara, vale)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!  DESCRIPTION : RENVOIE LA VALEUR DE LA CARACTERISTIQUE ELEMENTAIRE
!                SOUHAITE POUR UNE MAILLE DONNEE
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : NOCART : CHARACTER*24 , SCALAIRE
!                    NOM DE LA CARTE CONTENANT L'INFO
!  IN     : NUMA : INTEGER , SCALAIRE
!                    NUMERO DE LA MAILLE
!  IN     : CARA : CHARACTER*8 , SCALAIRE
!                    NOM DE LA CARACTERISTIQUE ELEMENTAIRE
!  OUT    : VALE : REEL , SCALAIRE
!                    VALEUR DE LA CARACTERISTIQUE POUR CETTE MAILLE
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "jeveux.h"
!~ !
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rgcmpg.h"
    character(len=8) :: mailla, cara
    character(len=19) :: carte
    integer :: numa
    real(kind=8) :: vale

!
! VARIABLES LOCALES
! -----------------
    character(len= 8) :: ngrand
    character(len=24) :: k24bid
    integer :: igrand, iasmax, iasedi, nbcmp, inomcp, nbec, irep
    integer :: iasbon, ii, icode, izone, ilnuma, nbmaza 
    integer :: irvep, jj
    real(kind=8), pointer :: v_vale(:) => null()
    integer, pointer :: desc(:) => null()
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
    call jeveuo(carte//'.DESC', 'L', vi=desc)
    call jeveuo(carte//'.VALE', 'L', vr=v_vale)
    igrand = desc(1)
    iasmax = desc(2)
    iasedi = desc(3)
    call jenuno(jexnum('&CATA.GD.NOMGD', igrand), ngrand)
    call jelira(jexnum('&CATA.GD.NOMCMP', igrand), 'LONMAX', nbcmp)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', igrand), 'L', inomcp)
    call dismoi('NB_EC', ngrand, 'GRANDEUR', repi=nbec)
    irep = indik8( zk8(inomcp), cara , 1, nbcmp )
    ASSERT(irep .ne. 0 )
!
!   RECHERCHE DE LA ZONE COMTENANT NUMA
    iasbon = 0
    do ii = 1, iasedi
        icode = desc(1+3+2*(ii-1))
        izone = desc(1+3+2*(ii-1)+1)
!       SI C'EST UNE LISTE DE MAILLE
        if (icode .eq. 3) then
            k24bid = carte//'.LIMA'
            call jeveuo(jexnum(k24bid, izone), 'L', ilnuma)
            call jelira(jexnum(k24bid, izone), 'LONMAX', nbmaza)
!       SI C'EST UN GROUPE DE MAILLE
        else if (icode.eq.2) then
            k24bid = mailla//'.GROUPEMA'
            call jeveuo(jexnum(k24bid, izone), 'L', ilnuma)
            call jelira(jexnum(k24bid, izone), 'LONMAX', nbmaza)
!       SI C'EST TOUT LE MAILLAGE
        else if (icode.eq.1) then
            iasbon = ii
            goto 160
        else
            ASSERT(.false.)
        endif
!       MAILLE DANS LISTE OU GROUPE DE MAILLE DE CETTE ZONE
        do jj = 1, nbmaza
            if (numa .eq. zi(ilnuma+jj-1)) then
                iasbon = ii
                goto 160
            endif
        end do
    end do
160 continue
    icode = desc(1+3+2*iasmax+nbec*(iasbon-1))
    irvep = rgcmpg(icode,irep)
    ASSERT(irvep .ne. 0)
    vale = v_vale(1+(iasbon-1)*nbcmp + irvep - 1)
!
    call jedema()
end subroutine
